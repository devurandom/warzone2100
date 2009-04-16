/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
	Copyright (C) 2005-2009  Warzone Resurrection Project

	Warzone 2100 is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	Warzone 2100 is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Warzone 2100; if not, write to the Free Software
	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
*/
/**
 * @file droid.c
 *
 * Droid method functions.
 *
 */
#include "lib/framework/frame.h"
#include "lib/framework/math_ext.h"
#include "lib/framework/strres.h"

#include "lib/gamelib/gtime.h"
#include "lib/gamelib/animobj.h"
#include "lib/ivis_opengl/piematrix.h"
#include "lib/ivis_opengl/screen.h"
#include "lib/framework/fixedpoint.h"
#include "lib/script/script.h"
#include "lib/sound/audio.h"
#include "lib/sound/audio_id.h"

#include "objects.h"
#include "loop.h"
#include "visibility.h"
#include "map.h"
#include "drive.h"
#include "droid.h"
#include "hci.h"
#include "game.h"
#include "power.h"
#include "miscimd.h"
#include "effects.h"
#include "feature.h"
#include "action.h"
#include "order.h"
#include "move.h"
#include "anim_id.h"
#include "geometry.h"
#include "display.h"
#include "console.h"
#include "component.h"
#include "function.h"
#include "lighting.h"
#include "multiplay.h"
#include "formationdef.h"
#include "formation.h"
#include "warcam.h"
#include "display3d.h"
#include "group.h"
#include "text.h"
#include "scripttabs.h"
#include "scriptcb.h"
#include "cmddroid.h"
#include "fpath.h"
#include "mapgrid.h"
#include "projectile.h"
#include "cluster.h"
#include "mission.h"
#include "levels.h"
#include "transporter.h"
#include "selection.h"
#include "difficulty.h"
#include "edit3d.h"
#include "scores.h"
#include "research.h"
#include "combat.h"
#include "scriptfuncs.h"			//for ThreatInRange()


#define DEFAULT_RECOIL_TIME	(GAME_TICKS_PER_SEC/4)
#define	DROID_DAMAGE_SPREAD	(16 - rand()%32)
#define	DROID_REPAIR_SPREAD	(20 - rand()%40)


/* default droid design template */
extern DROID_TEMPLATE	sDefaultDesignTemplate;

//storage
DROID_TEMPLATE		*apsDroidTemplates[MAX_PLAYERS];

// store the experience of recently recycled droids
UWORD	aDroidExperience[MAX_PLAYERS][MAX_RECYCLED_DROIDS];
UDWORD	selectedGroup = UBYTE_MAX;
UDWORD	selectedCommander = UBYTE_MAX;

/** Height the transporter hovers at above the terrain. */
#define TRANSPORTER_HOVER_HEIGHT	10

/** How far round a repair droid looks for a damaged droid. */
#define REPAIR_DIST		(TILE_UNITS * 4)//8)

/* Store for the objects near the droid currently being updated
 * NAYBOR = neighbour - thanks to Keith for a great abreviation
 */
NAYBOR_INFO			asDroidNaybors[MAX_NAYBORS];
UDWORD				numNaybors=0;

// the structure that was last hit
DROID	*psLastDroidHit;

//determines the best IMD to draw for the droid - A TEMP MEASURE!
void	groupConsoleInformOfSelection( UDWORD groupNumber );
void	groupConsoleInformOfCreation( UDWORD groupNumber );
void	groupConsoleInformOfCentering( UDWORD groupNumber );
void	droidUpdateRecoil( DROID *psDroid );


// initialise droid module
BOOL droidInit(void)
{
	memset(aDroidExperience, 0, sizeof(UWORD) * MAX_PLAYERS * MAX_RECYCLED_DROIDS);
	psLastDroidHit = NULL;

	return true;
}


#define UNIT_LOST_DELAY	(5*GAME_TICKS_PER_SEC)
/* Deals damage to a droid
 * \param psDroid droid to deal damage to
 * \param damage amount of damage to deal
 * \param weaponClass the class of the weapon that deals the damage
 * \param weaponSubClass the subclass of the weapon that deals the damage
 * \param angle angle of impact (from the damage dealing projectile in relation to this droid)
 * \return > 0 when the dealt damage destroys the droid, < 0 when the droid survives
 *
 * NOTE: This function will damage but _never_ destroy transports when in single player (campaign) mode
 */
float droidDamage(DROID *psDroid, UDWORD damage, UDWORD weaponClass, UDWORD weaponSubClass, HIT_SIDE impactSide)
{
	float		relativeDamage;
	SECONDARY_STATE	state;

	CHECK_DROID(psDroid);

	// VTOLs on the ground take triple damage
	if (isVtolDroid(psDroid) && psDroid->sMove.Status == MOVEINACTIVE)
	{
		damage *= 3;
	}

	relativeDamage = objDamage((BASE_OBJECT *)psDroid, damage, psDroid->originalBody, weaponClass, weaponSubClass, impactSide);

	if (relativeDamage > 0.0f)
	{
		// reset the attack level
		if (secondaryGetState(psDroid, DSO_ATTACK_LEVEL, &state) && state == DSS_ALEV_ATTACKED)
		{
			secondarySetState(psDroid, DSO_ATTACK_LEVEL, DSS_ALEV_ALWAYS);
		}
		// Now check for auto return on droid's secondary orders (i.e. return on medium/heavy damage)
		secondaryCheckDamageLevel(psDroid);

		// Now check for scripted run-away based on health left
		orderHealthCheck(psDroid);

		CHECK_DROID(psDroid);
	}
	else if (relativeDamage < 0.0f)
	{
		// HACK: Prevent transporters from being destroyed in single player
		if (!bMultiPlayer && psDroid->droidType == DROID_TRANSPORTER)
		{
			psDroid->body = 1;
			return 0.0f;
		}

		// Droid destroyed
		debug(LOG_ATTACK, "droidDamage(%d): DESTROYED", psDroid->id);

		// Deal with score increase/decrease and messages to the player
		if( psDroid->player == selectedPlayer)
		{
			CONPRINTF(ConsoleString,(ConsoleString, _("Unit Lost!")));
			scoreUpdateVar(WD_UNITS_LOST);
			audio_QueueTrackMinDelayPos(ID_SOUND_UNIT_DESTROYED,UNIT_LOST_DELAY,
										psDroid->pos.x, psDroid->pos.y, psDroid->pos.z );
		}
		else
		{
			scoreUpdateVar(WD_UNITS_KILLED);
		}

		// If this is droid is a person and was destroyed by flames,
		// show it nicely by burning him/her to death.
		if (psDroid->droidType == DROID_PERSON && weaponClass == WC_HEAT)
		{
			droidBurn(psDroid);
		}
		// Otherwise use the default destruction animation
		else
		{
			debug(LOG_DEATH, "droidDamage: Droid %d beyond repair", (int)psDroid->id);
			destroyDroid(psDroid);
		}
	}

	return relativeDamage;
}

// Check that psVictimDroid is not referred to by any other object in the game
BOOL droidCheckReferences(DROID *psVictimDroid)
{
	int plr;

	for (plr = 0; plr < MAX_PLAYERS; plr++)
	{
		STRUCTURE *psStruct;
		DROID *psDroid;

		for (psStruct = apsStructLists[plr]; psStruct != NULL; psStruct = psStruct->psNext)
		{
			unsigned int i;

			for (i = 0; i < psStruct->numWeaps; i++)
			{
				if ((DROID *)psStruct->psTarget[i] == psVictimDroid)
				{
#ifndef DEBUG
					debug(LOG_ERROR, "droidCheckReferences: Illegal reference to droid");
#else
					ASSERT(!"Illegal reference to droid", "Illegal reference to droid from %s line %d",
					       psStruct->targetFunc[i], psStruct->targetLine[i]);
#endif
					return false;
				}
			}
		}
		for (psDroid = apsDroidLists[plr]; psDroid != NULL; psDroid = psDroid->psNext)
		{
			unsigned int i;

			if ((DROID *)psDroid->psTarget == psVictimDroid && psVictimDroid != psDroid)
			{
#ifndef DEBUG
				debug(LOG_ERROR, "droidCheckReferences: Illegal reference to droid");
#else
				ASSERT(!"Illegal reference to droid", "Illegal reference to droid from %s line %d",
				       psDroid->targetFunc, psDroid->targetLine);
#endif
				return false;
			}
			for (i = 0; i < psDroid->numWeaps; i++)
			{
				if ((DROID *)psDroid->psActionTarget[i] == psVictimDroid && psVictimDroid != psDroid)
				{
#ifndef DEBUG
					debug(LOG_ERROR, "droidCheckReferences: Illegal action reference to droid");
#else
					ASSERT(!"Illegal reference to droid", "Illegal action reference to droid from %s line %d",
					       psDroid->actionTargetFunc[i], psDroid->actionTargetLine[i]);
#endif
					return false;
				}
			}
		}
	}
	return true;
}

/* droidRelease: release all resources associated with a droid -
 * should only be called by objmem - use vanishDroid preferably
 */
void droidRelease(DROID *psDroid)
{
	DROID	*psCurr, *psNext;

	/* remove animation if present */
	if (psDroid->psCurAnim != NULL)
	{
		animObj_Remove(psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID);
		psDroid->psCurAnim = NULL;
	}

	if (psDroid->droidType == DROID_TRANSPORTER)
	{
		if (psDroid->psGroup)
		{
			//free all droids associated with this Transporter
			for (psCurr = psDroid->psGroup->psList; psCurr != NULL && psCurr !=
				psDroid; psCurr = psNext)
			{
				psNext = psCurr->psGrpNext;
				droidRelease(psCurr);
				free(psCurr);
			}
		}
	}

	fpathRemoveDroidData(psDroid->id);

	// leave the current formation if any
	if (psDroid->sMove.psFormation)
	{
		formationLeave(psDroid->sMove.psFormation, psDroid);
	}

	// leave the current group if any
	if (psDroid->psGroup)
	{
		grpLeave(psDroid->psGroup, psDroid);
	}

	// remove the object from the grid
	gridRemoveObject((BASE_OBJECT *)psDroid);

	// remove the droid from the cluster systerm
	clustRemoveObject((BASE_OBJECT *)psDroid);

	if (psDroid->sMove.asPath)
	{
		free(psDroid->sMove.asPath);
	}
}


// recycle a droid (retain it's experience and some of it's cost)
void recycleDroid(DROID *psDroid)
{
	UDWORD		numKills, minKills;
	SDWORD		i, cost, storeIndex;
	Vector3i position;

	CHECK_DROID(psDroid);

	// store the droids kills
	numKills = psDroid->experience;
	if (numKills > UWORD_MAX)
	{
		numKills = UWORD_MAX;
	}
	minKills = UWORD_MAX;
	storeIndex = 0;
	for(i=0; i<MAX_RECYCLED_DROIDS; i++)
	{
		if (aDroidExperience[psDroid->player][i] < (UWORD)minKills)
		{
			storeIndex = i;
			minKills = aDroidExperience[psDroid->player][i];
		}
	}
	aDroidExperience[psDroid->player][storeIndex] = (UWORD)numKills;

	// return part of the cost of the droid
	cost = calcDroidPower(psDroid);
	cost = (cost/2) * psDroid->body / psDroid->originalBody;
	addPower(psDroid->player, (UDWORD)cost);

	// hide the droid
	memset(psDroid->visible, 0, sizeof(psDroid->visible));
	// stop any group moral checks
	if (psDroid->psGroup)
	{
		grpLeave(psDroid->psGroup, psDroid);
	}

	position.x = psDroid->pos.x;				// Add an effect
	position.z = psDroid->pos.y;
	position.y = psDroid->pos.z;

	vanishDroid(psDroid);

	addEffect(&position,EFFECT_EXPLOSION,EXPLOSION_TYPE_DISCOVERY,false,NULL,false);

	CHECK_DROID(psDroid);
}


void	removeDroidBase(DROID *psDel)
{
	DROID	*psCurr, *psNext;
	DROID_GROUP	*psGroup;
	STRUCTURE	*psStruct;

	CHECK_DROID(psDel);

	if (isDead((BASE_OBJECT *)psDel))
	{
		// droid has already been killed, quit
		return;
	}

	//ajl, inform others of destruction.
	if (bMultiPlayer
	 && !(psDel->player != selectedPlayer && psDel->order == DORDER_RECYCLE))
	{
		ASSERT(droidOnMap(psDel), "Asking other players to destroy droid driving off the map");
		SendDestroyDroid(psDel);
	}


	/* remove animation if present */
	if (psDel->psCurAnim != NULL)
	{
		const bool bRet = animObj_Remove(psDel->psCurAnim, psDel->psCurAnim->psAnim->uwID);
		ASSERT(bRet, "animObj_Remove failed");
		psDel->psCurAnim = NULL;
	}

	// leave the current formation if any
	if (psDel->sMove.psFormation)
	{
		formationLeave(psDel->sMove.psFormation, psDel);
		psDel->sMove.psFormation = NULL;
	}

	//kill all the droids inside the transporter
	if (psDel->droidType == DROID_TRANSPORTER)
	{
		if (psDel->psGroup)
		{
			//free all droids associated with this Transporter
			for (psCurr = psDel->psGroup->psList; psCurr != NULL && psCurr !=
				psDel; psCurr = psNext)
			{
				psNext = psCurr->psGrpNext;

				/* add droid to droid list then vanish it - hope this works! - GJ */
				addDroid(psCurr, apsDroidLists);
				vanishDroid(psCurr);
			}
		}
	}

	// check moral
	if (psDel->psGroup && psDel->psGroup->refCount > 1)
	{
		psGroup = psDel->psGroup;
		grpLeave(psDel->psGroup, psDel);
		orderGroupMoralCheck(psGroup);
	}
	else
	{
		orderMoralCheck(psDel->player);
	}

	// leave the current group if any
	if (psDel->psGroup)
	{
		grpLeave(psDel->psGroup, psDel);
		psDel->psGroup = NULL;
	}

	/* Put Deliv. Pts back into world when a command droid dies */
	if (psDel->droidType == DROID_COMMAND)
	{
		for (psStruct = apsStructLists[psDel->player]; psStruct; psStruct=psStruct->psNext)
		{
			// alexl's stab at a right answer.
			if (StructIsFactory(psStruct)
			 && psStruct->pFunctionality->factory.psCommander == psDel)
			{
				assignFactoryCommandDroid(psStruct, NULL);
			}
		}
	}

	// Check to see if constructor droid currently trying to find a location to build
	if ((psDel->droidType == DROID_CONSTRUCT || psDel->droidType == DROID_CYBORG_CONSTRUCT)
	    && psDel->player == selectedPlayer && psDel->selected)
	{
		// If currently trying to build, kill off the placement
		if (tryingToGetLocation())
		{
			kill3DBuilding();
		}
	}

	// remove the droid from the grid
	gridRemoveObject((BASE_OBJECT *)psDel);

	// remove the droid from the cluster systerm
	clustRemoveObject((BASE_OBJECT *)psDel);

	if (psDel->player == selectedPlayer)
	{
		intRefreshScreen();
	}

	killDroid(psDel);
}

static void removeDroidFX(DROID *psDel)
{
	Vector3i pos;

	CHECK_DROID(psDel);

	// only display anything if the droid is visible
	if (!psDel->visible[selectedPlayer])
	{
		return;
	}

	if ((psDel->droidType == DROID_PERSON || cyborgDroid(psDel)) && psDel->order != DORDER_RUNBURN)
	{
		/* blow person up into blood and guts */
		compPersonToBits(psDel);
	}

	/* if baba and not running (on fire) then squish */
	if (psDel->droidType == DROID_PERSON
	 && psDel->order != DORDER_RUNBURN
	 && psDel->visible[selectedPlayer])
	{
		// The babarian has been run over ...
		audio_PlayStaticTrack(psDel->pos.x, psDel->pos.y, ID_SOUND_BARB_SQUISH);
	}
	else if (psDel->visible[selectedPlayer])
	{
		destroyFXDroid(psDel);
		pos.x = psDel->pos.x;
		pos.z = psDel->pos.y;
		pos.y = psDel->pos.z;
		addEffect(&pos,EFFECT_DESTRUCTION,DESTRUCTION_TYPE_DROID,false,NULL,0);
		audio_PlayStaticTrack( psDel->pos.x, psDel->pos.y, ID_SOUND_EXPLOSION );
	}
}

void destroyDroid(DROID *psDel)
{
	if (psDel->lastHitWeapon == WSC_LAS_SAT)		// darken tile if lassat.
	{
		UDWORD width, breadth, mapX, mapY;
		MAPTILE	*psTile;

		mapX = map_coord(psDel->pos.x);
		mapY = map_coord(psDel->pos.y);
		for (width = mapX-1; width <= mapX+1; width++)
		{
			for (breadth = mapY-1; breadth <= mapY+1; breadth++)
			{
				psTile = mapTile(width,breadth);
				if(TEST_TILE_VISIBLE(selectedPlayer, psTile))
				{
					psTile->illumination /= 2;
					if(psTile->bMaxed && psTile->level > 0) //only do one's already seen
					{
						psTile->level/=2;
					}

				}
			}
		}
	}

	removeDroidFX(psDel);
	removeDroidBase(psDel);
	return;
}

void	vanishDroid(DROID *psDel)
{
	removeDroidBase(psDel);
}

/* Remove a droid from the List so doesn't update or get drawn etc
TAKE CARE with removeDroid() - usually want droidRemove since it deal with cluster and grid code*/
//returns false if the droid wasn't removed - because it died!
BOOL droidRemove(DROID *psDroid, DROID *pList[MAX_PLAYERS])
{
	CHECK_DROID(psDroid);

	driveDroidKilled(psDroid);	// Tell the driver system it's gone.

	if (isDead((BASE_OBJECT *) psDroid))
	{
		// droid has already been killed, quit
		return false;
	}

	// leave the current formation if any
	if (psDroid->sMove.psFormation)
	{
		formationLeave(psDroid->sMove.psFormation, psDroid);
		psDroid->sMove.psFormation = NULL;
	}

	// leave the current group if any - not if its a Transporter droid
	if (psDroid->droidType != DROID_TRANSPORTER && psDroid->psGroup)
	{
		grpLeave(psDroid->psGroup, psDroid);
		psDroid->psGroup = NULL;
	}

	// reset the baseStruct
	setDroidBase(psDroid, NULL);

	// remove the droid from the cluster systerm
	clustRemoveObject((BASE_OBJECT *)psDroid);

	// remove the droid from the grid
	gridRemoveObject((BASE_OBJECT *)psDroid);

	removeDroid(psDroid, pList);

	if (psDroid->player == selectedPlayer)
	{
		intRefreshScreen();
	}

	return true;
}

static void droidFlameFallCallback( ANIM_OBJECT * psObj )
{
	DROID	*psDroid;

	ASSERT( psObj != NULL,
		"unitFlameFallCallback: invalid anim object pointer\n" );
	psDroid = (DROID *) psObj->psParent;
	ASSERT( psDroid != NULL,
		"unitFlameFallCallback: invalid Unit pointer\n" );

	psDroid->psCurAnim = NULL;

	debug(LOG_DEATH, "droidFlameFallCallback: Droid %d destroyed", (int)psDroid->id);
	destroyDroid( psDroid );
}

static void droidBurntCallback( ANIM_OBJECT * psObj )
{
	DROID	*psDroid;

	ASSERT( psObj != NULL,
		"unitBurntCallback: invalid anim object pointer\n" );
	psDroid = (DROID *) psObj->psParent;
	ASSERT( psDroid != NULL,
		"unitBurntCallback: invalid Unit pointer\n" );

	/* add falling anim */
	psDroid->psCurAnim = animObj_Add((BASE_OBJECT *)psDroid, ID_ANIM_DROIDFLAMEFALL, 0, 1);
	if (!psDroid->psCurAnim)
	{
		debug( LOG_ERROR, "unitBurntCallback: couldn't add fall over anim\n" );
		abort();
		return;
	}

	animObj_SetDoneFunc( psDroid->psCurAnim, droidFlameFallCallback );
}

void droidBurn(DROID *psDroid)
{
	CHECK_DROID(psDroid);

	if ( psDroid->droidType != DROID_PERSON )
	{
		ASSERT(LOG_ERROR, "unitBurn: can't burn anything except babarians currently!");
		return;
	}

	/* if already burning return else remove currently-attached anim if present */
	if ( psDroid->psCurAnim != NULL )
	{
		/* return if already burning */
		if ( psDroid->psCurAnim->psAnim->uwID == ID_ANIM_DROIDBURN )
		{
			return;
		}
		else
		{
			const bool bRet = animObj_Remove(psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID);
			ASSERT(bRet, "animObj_Remove failed");
			psDroid->psCurAnim = NULL;
		}
	}

	/* add burning anim */
	psDroid->psCurAnim = animObj_Add( (BASE_OBJECT *) psDroid,
											ID_ANIM_DROIDBURN, 0, 3 );
	if ( psDroid->psCurAnim == NULL )
	{
		debug( LOG_ERROR, "unitBurn: couldn't add burn anim\n" );
		abort();
		return;
	}

	/* set callback */
	animObj_SetDoneFunc( psDroid->psCurAnim, droidBurntCallback );

	/* add scream */
	debug( LOG_NEVER, "baba burn\n" );

	audio_PlayObjDynamicTrack( psDroid, ID_SOUND_BARB_SCREAM+(rand()%3), NULL );


	/* set droid running */
	orderDroid( psDroid, DORDER_RUNBURN );
}

/* Add a new object to the naybor list */
static void addNaybor(BASE_OBJECT *psObj, UDWORD distSqr)
{
	UDWORD	pos;

	if (numNaybors == 0)
	{
		// No objects in the list
		asDroidNaybors[0].psObj = psObj;
		asDroidNaybors[0].distSqr = distSqr;
		numNaybors++;
	}
	else if (distSqr >= asDroidNaybors[numNaybors-1].distSqr)
	{
		// Simple case - this is the most distant object
		asDroidNaybors[numNaybors].psObj = psObj;
		asDroidNaybors[numNaybors].distSqr = distSqr;
		numNaybors++;
	}
	else
	{
		// Move all the objects further away up the list
		for (pos = numNaybors; pos && asDroidNaybors[pos - 1].distSqr > distSqr; pos--)
		{
			memcpy(asDroidNaybors + pos, asDroidNaybors + (pos - 1), sizeof(NAYBOR_INFO));
		}

		// Insert the object at the correct position
		asDroidNaybors[pos].psObj = psObj;
		asDroidNaybors[pos].distSqr = distSqr;
		numNaybors++;
	}
}


static DROID	*CurrentNaybors = NULL;
static UDWORD	nayborTime = 0;

/* Find all the objects close to the droid */
void droidGetNaybors(DROID *psDroid)
{
	SDWORD		xdiff, ydiff;
	UDWORD		dx,dy, distSqr;
	BASE_OBJECT	*psObj;

	CHECK_DROID(psDroid);

	// Ensure only called max of once per droid per game cycle.
	if (CurrentNaybors == psDroid && nayborTime == gameTime)
	{
		return;
	}
	CurrentNaybors = psDroid;
	nayborTime = gameTime;

	// reset the naybor array
	numNaybors = 0;

	// search for naybor objects
	dx = psDroid->pos.x;
	dy = psDroid->pos.y;

	gridStartIterate((SDWORD)dx, (SDWORD)dy);
	for (psObj = gridIterate(); psObj != NULL; psObj = gridIterate())
	{
		if (psObj != (BASE_OBJECT *)psDroid && !psObj->died)
		{
			xdiff = dx - (SDWORD)psObj->pos.x;
			if (xdiff < 0)
			{
				xdiff = -xdiff;
			}
			if (xdiff > NAYBOR_RANGE)
			{
				continue;
			}

			ydiff = dy - (SDWORD)psObj->pos.y;
			if (ydiff < 0)
			{
				ydiff = -ydiff;
			}
			if (ydiff > NAYBOR_RANGE)
			{
				continue;
			}

			distSqr = xdiff*xdiff + ydiff*ydiff;
			if (distSqr > NAYBOR_RANGE*NAYBOR_RANGE)
			{
				continue;
			}

			addNaybor(psObj, distSqr);
			if (numNaybors >= MAX_NAYBORS)
			{
				break;
			}

		}
	}
}

/* The main update routine for all droids */
void droidUpdate(DROID *psDroid)
{
	Vector3i dv;
	UDWORD	percentDamage, emissionInterval;
	BASE_OBJECT	*psBeingTargetted = NULL;
	SDWORD	damageToDo;

	CHECK_DROID(psDroid);

	// update the cluster of the droid
	if (psDroid->id % 20 == frameGetFrameNumber() % 20)
	{
		clustUpdateObject((BASE_OBJECT *)psDroid);
	}

	// ai update droid
	aiUpdateDroid(psDroid);

	// Update the droids order. The droid may be killed here due to burn out.
	orderUpdateDroid(psDroid);
	if (isDead((BASE_OBJECT *)psDroid))
	{
		return;	// FIXME: Workaround for babarians that were burned to death
	}

	// update the action of the droid
	actionUpdateDroid(psDroid);

	// update the move system
	moveUpdateDroid(psDroid);

	/* Only add smoke if they're visible */
	if((psDroid->visible[selectedPlayer]) && psDroid->droidType != DROID_PERSON)
	{
		// need to clip this value to prevent overflow condition
		percentDamage = 100 - clip(PERCENT(psDroid->body, psDroid->originalBody), 0, 100);

		// Is there any damage?
		if(percentDamage>=25)
		{
			if(percentDamage>=100)
			{
				percentDamage = 99;
			}

			emissionInterval = CALC_DROID_SMOKE_INTERVAL(percentDamage);

			if(gameTime > (psDroid->lastEmission + emissionInterval))
			{
   				dv.x = psDroid->pos.x + DROID_DAMAGE_SPREAD;
   				dv.z = psDroid->pos.y + DROID_DAMAGE_SPREAD;
				dv.y = psDroid->pos.z;

				dv.y += (psDroid->sDisplay.imd->max.y * 2);
				addEffect(&dv,EFFECT_SMOKE,SMOKE_TYPE_DRIFTING_SMALL,false,NULL,0);
				psDroid->lastEmission = gameTime;
			}
		}
	}

	processVisibility((BASE_OBJECT*)psDroid);

	// -----------------
	/* Are we a sensor droid or a command droid? */
	if( (psDroid->droidType == DROID_SENSOR) || (psDroid->droidType == DROID_COMMAND) )
	{
		/* If we're attacking or sensing (observing), then... */
		if ((psBeingTargetted = orderStateObj(psDroid, DORDER_ATTACK))
		 || (psBeingTargetted = orderStateObj(psDroid, DORDER_OBSERVE)))
		{
			/* If it's a structure */
			if(psBeingTargetted->type == OBJ_STRUCTURE)
			{
				/* And it's your structure or your droid... */
				if( (((STRUCTURE*)psBeingTargetted)->player == selectedPlayer)
					|| (psDroid->player == selectedPlayer) )
				{
					/* Highlight the structure in question */
					((STRUCTURE*)psBeingTargetted)->targetted = 1;
				}
			}
			else if (psBeingTargetted->type == OBJ_DROID)
			{
				/* And it's your  your droid... */
	 			if( (((DROID*)psBeingTargetted)->player == selectedPlayer)
 					|| (psDroid->player == selectedPlayer) )
 				{
 				   	((DROID*)psBeingTargetted)->bTargetted = true;

 				}
			}
			else if(psBeingTargetted->type == OBJ_FEATURE)
			{
				if(psDroid->player == selectedPlayer)
				{
					((FEATURE*)psBeingTargetted)->bTargetted = true;
				}
			}

		}
	}
	// -----------------

	/* Update the fire damage data */
	if (psDroid->inFire & IN_FIRE)
	{
		/* Still in a fire, reset the fire flag to see if we get out this turn */
		psDroid->inFire = 0;
	}
	else
	{
		/* The fire flag has not been set so we must be out of the fire */
		if (psDroid->inFire & BURNING)
		{
			if (psDroid->burnStart + BURN_TIME < gameTime)
			{
				// stop burning
				psDroid->inFire = 0;
				psDroid->burnStart = 0;
				psDroid->burnDamage = 0;
			}
			else
			{
				// do burn damage
				damageToDo = BURN_DAMAGE * ((SDWORD)gameTime - (SDWORD)psDroid->burnStart) /
								GAME_TICKS_PER_SEC;
				damageToDo -= (SDWORD)psDroid->burnDamage;
				if (damageToDo > 0)
				{
					psDroid->burnDamage += damageToDo;

					//Watermelon:just assume the burn damage is from FRONT
					droidDamage(psDroid, damageToDo, WC_HEAT,WSC_FLAME, HIT_SIDE_FRONT);
				}
			}
		}
		else if (psDroid->burnStart != 0)
		{
			// just left the fire
			psDroid->inFire |= BURNING;
			psDroid->burnStart = gameTime;
			psDroid->burnDamage = 0;
		}
	}

	// At this point, the droid may be dead due to burn damage.
	if (isDead((BASE_OBJECT *)psDroid))
	{
		return;
	}

	droidUpdateRecoil(psDroid);

	calcDroidIllumination(psDroid);

	// Check the resistance level of the droid
	if (psDroid->id % 50 == frameGetFrameNumber() % 50)
	{
		// Zero resistance means not currently been attacked - ignore these
		if (psDroid->resistance && psDroid->resistance < droidResistance(psDroid))
		{
			// Increase over time if low
			psDroid->resistance++;
		}
	}

	CHECK_DROID(psDroid);
}

/* See if a droid is next to a structure */
static BOOL droidNextToStruct(DROID *psDroid, BASE_OBJECT *psStruct)
{
	SDWORD	minX, maxX, maxY, x,y;

	CHECK_DROID(psDroid);

	minX = map_coord(psDroid->pos.x) - 1;
	y = map_coord(psDroid->pos.y) - 1;
	maxX = minX + 2;
	maxY = y + 2;
	if (minX < 0)
	{
		minX = 0;
	}
	if (maxX >= (SDWORD)mapWidth)
	{
		maxX = (SDWORD)mapWidth;
	}
	if (y < 0)
	{
		y = 0;
	}
	if (maxY >= (SDWORD)mapHeight)
	{
		maxY = (SDWORD)mapHeight;
	}
	for(;y <= maxY; y++)
	{
		for(x = minX;x <= maxX; x++)
		{
			if (TileHasStructure(mapTile((UWORD)x,(UWORD)y)) &&
				getTileStructure(x,y) == (STRUCTURE *)psStruct)

			{
				return true;
			}
		}
	}

	return false;
}

static BOOL
droidCheckBuildStillInProgress( void *psObj )
{
	DROID	*psDroid;

	if ( psObj == NULL )
	{
		return false;
	}

	psDroid = (DROID*)psObj;
	CHECK_DROID(psDroid);

	if ( !psDroid->died && psDroid->action == DACTION_BUILD )
	{
		return true;
	}
	else
	{
		return false;
	}
}

static BOOL
droidBuildStartAudioCallback( void *psObj )
{
	DROID	*psDroid;

	psDroid = (DROID*)psObj;

	if (psDroid == NULL)
	{
		return true;
	}

	if ( psDroid->visible[selectedPlayer] )
	{
		audio_PlayObjDynamicTrack( psDroid, ID_SOUND_CONSTRUCTION_LOOP, droidCheckBuildStillInProgress );
	}

	return true;
}


/* Set up a droid to build a structure - returns true if successful */
BOOL droidStartBuild(DROID *psDroid)
{
	STRUCTURE *psStruct;

	CHECK_DROID(psDroid);

	/* See if we are starting a new structure */
	if ((psDroid->psTarget == NULL) &&
		(psDroid->order == DORDER_BUILD ||
		 psDroid->order == DORDER_LINEBUILD))
	{
		STRUCTURE_STATS *psStructStat = (STRUCTURE_STATS *)psDroid->psTarStats;
		STRUCTURE_LIMITS *structLimit = &asStructLimits[psDroid->player][psStructStat - asStructureStats];

		//need to check structLimits have not been exceeded
		if (structLimit->currentQuantity >= structLimit->limit)
		{
			intBuildFinished(psDroid);
			return false;
		}
		//ok to build
		psStruct = buildStructure(psStructStat, psDroid->orderX,psDroid->orderY, psDroid->player,false);
		if (!psStruct)
		{
			intBuildFinished(psDroid);
			return false;
		}
		psStruct->body /= 10; // structures start at 10% health

		if (bMultiPlayer)
		{
			if(myResponsibility(psDroid->player) )
			{
				sendBuildStarted(psStruct, psDroid);
			}
		}

	}
	else
	{
		/* Check the structure is still there to build (joining a partially built struct) */
		psStruct = (STRUCTURE *)psDroid->psTarget;
		if (!droidNextToStruct(psDroid, (BASE_OBJECT *)psStruct))
		{
			/* Nope - stop building */
			debug( LOG_NEVER, "unitStartBuild: not next to structure\n" );
		}
	}

	//check structure not already built
	if (psStruct->status != SS_BUILT)
	{
		psDroid->actionStarted = gameTime;
		psDroid->actionPoints = 0;
		setDroidTarget(psDroid, (BASE_OBJECT *)psStruct);
		setDroidActionTarget(psDroid, (BASE_OBJECT *)psStruct, 0);
	}

	if ( psStruct->visible[selectedPlayer] )
	{
		audio_PlayObjStaticTrackCallback( psDroid, ID_SOUND_CONSTRUCTION_START,
										droidBuildStartAudioCallback );
	}

	CHECK_DROID(psDroid);

	return true;
}

static void droidAddWeldSound( Vector3i iVecEffect )
{
	SDWORD		iAudioID;

	iAudioID = ID_SOUND_CONSTRUCTION_1 + (rand()%4);

	audio_PlayStaticTrack( iVecEffect.x, iVecEffect.z, iAudioID );
}

static void addConstructorEffect(STRUCTURE *psStruct)
{
	UDWORD		widthRange,breadthRange;
	Vector3i temp;

	//FIXME
	if((ONEINTEN) && (psStruct->visible[selectedPlayer]))
	{
		/* This needs fixing - it's an arse effect! */
		widthRange = (psStruct->pStructureType->baseWidth*TILE_UNITS)/4;
		breadthRange = (psStruct->pStructureType->baseBreadth*TILE_UNITS)/4;
		temp.x = psStruct->pos.x+((rand()%(2*widthRange)) - widthRange);
		temp.y = map_TileHeight(map_coord(psStruct->pos.x), map_coord(psStruct->pos.y))+
						(psStruct->sDisplay.imd->max.y / 6);
		temp.z = psStruct->pos.y+((rand()%(2*breadthRange)) - breadthRange);
		if(rand()%2)
		{
			droidAddWeldSound( temp );
		}
	}
}

/* Update a construction droid while it is building
   returns true while building continues */
BOOL droidUpdateBuild(DROID *psDroid)
{
	UDWORD		pointsToAdd, constructPoints;
	STRUCTURE	*psStruct;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->action == DACTION_BUILD,
		"unitUpdateBuild: unit is not building" );
	ASSERT(psDroid->psTarget, "Trying to update a construction, but no target!");
	if (!psDroid->psTarget)
	{
		return false;
	}
	psStruct = (STRUCTURE *)psDroid->psTarget;
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitUpdateBuild: target is not a structure" );
	ASSERT( psDroid->asBits[COMP_CONSTRUCT].nStat < numConstructStats,
		"unitUpdateBuild: Invalid construct pointer for unit" );

	// First check the structure hasn't been completed by another droid
	if (psStruct->status == SS_BUILT)
	{
		// Update the interface
		intBuildFinished(psDroid);

		debug( LOG_NEVER, "DACTION_BUILD: done\n");
		psDroid->action = DACTION_NONE;

		return false;
	}

	constructPoints = constructorPoints(asConstructStats + psDroid->
		asBits[COMP_CONSTRUCT].nStat, psDroid->player);

	pointsToAdd = constructPoints * (gameTime - psDroid->actionStarted) /
		GAME_TICKS_PER_SEC;

	structureBuild(psStruct, psDroid, pointsToAdd - psDroid->actionPoints);

	//store the amount just added
	psDroid->actionPoints = pointsToAdd;

	addConstructorEffect(psStruct);


	return true;
}

BOOL droidStartDemolishing( DROID *psDroid )
{
	psDroid->actionStarted = gameTime;
	psDroid->actionPoints = 0;
	return true;
}

BOOL droidUpdateDemolishing( DROID *psDroid )
{
	STRUCTURE	*psStruct;
	UDWORD		pointsToAdd, constructPoints;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->action == DACTION_DEMOLISH,
		"unitUpdateDemolishing: unit is not demolishing" );
	psStruct = (STRUCTURE *)psDroid->psTarget;
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitUpdateDemolishing: target is not a structure" );

	//constructPoints = (asConstructStats + psDroid->asBits[COMP_CONSTRUCT].nStat)->
	//	constructPoints;
	constructPoints = 5 * constructorPoints(asConstructStats + psDroid->
		asBits[COMP_CONSTRUCT].nStat, psDroid->player);

	pointsToAdd = constructPoints * (gameTime - psDroid->actionStarted) /
		GAME_TICKS_PER_SEC;

	structureDemolish(psStruct, psDroid, pointsToAdd - psDroid->actionPoints);

	//store the amount just subtracted
	psDroid->actionPoints = pointsToAdd;

	addConstructorEffect(psStruct);

	CHECK_DROID(psDroid);

	return true;
}

/* Set up a droid to clear a wrecked building feature - returns true if successful */
BOOL droidStartClearing( DROID *psDroid )
{
	FEATURE			*psFeature;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->order == DORDER_CLEARWRECK,
		"unitStartClearing: unit is not clearing wreckage" );
	psFeature = (FEATURE *)psDroid->psTarget;
	ASSERT( psFeature->type == OBJ_FEATURE,
		"unitStartClearing: target is not a feature" );
	ASSERT( psFeature->psStats->subType == FEAT_BUILD_WRECK,
		"unitStartClearing: feature is not a wrecked building" );

	psDroid->actionStarted = gameTime;
	psDroid->actionPoints  = 0;

	CHECK_DROID(psDroid);

	return true;
}

/* Update a construction droid while it is clearing
   returns true while continues */
BOOL droidUpdateClearing( DROID *psDroid )
{
	FEATURE		*psFeature;
	UDWORD		pointsToAdd, constructPoints;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->action == DACTION_CLEARWRECK,
		"unitUpdateClearing: unit is not clearing wreckage" );
	psFeature = (FEATURE *)psDroid->psTarget;
	ASSERT( psFeature->type == OBJ_FEATURE,
		"unitStartClearing: target is not a feature" );
	ASSERT( psFeature->psStats->subType == FEAT_BUILD_WRECK,
		"unitStartClearing: feature is not a wrecked building" );

	if (psFeature->body > 0)
	{
		constructPoints = constructorPoints(asConstructStats + psDroid->
			asBits[COMP_CONSTRUCT].nStat, psDroid->player);

		pointsToAdd = constructPoints * (gameTime - psDroid->actionStarted) /
			GAME_TICKS_PER_SEC;

		psFeature->body -= (pointsToAdd - psDroid->actionPoints);

		//store the amount just subtracted
		psDroid->actionPoints = pointsToAdd;
	}

	/* check if structure is demolished */
	if ( psFeature->body <= 0 )
	{
		/* remove feature */
		removeFeature(psFeature);

		/* reset target stats */
		psDroid->psTarStats = NULL;

		CHECK_DROID(psDroid);

		return false;
	}

	CHECK_DROID(psDroid);

	return true;
}

BOOL droidStartRepair( DROID *psDroid )
{
	STRUCTURE	*psStruct;

	CHECK_DROID(psDroid);

	psStruct = (STRUCTURE *)psDroid->psActionTarget[0];
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitStartRepair: target is not a structure" );

	psDroid->actionStarted = gameTime;
	psDroid->actionPoints  = 0;

	CHECK_DROID(psDroid);

	return true;
}


/*Start a Repair Droid working on a damaged droid*/
BOOL droidStartDroidRepair( DROID *psDroid )
{
	DROID	*psDroidToRepair;

	CHECK_DROID(psDroid);

	psDroidToRepair = (DROID *)psDroid->psActionTarget[0];
	ASSERT( psDroidToRepair->type == OBJ_DROID,
		"unitStartUnitRepair: target is not a unit" );

	psDroid->actionStarted = gameTime;
	psDroid->actionPoints  = 0;

	CHECK_DROID(psDroid);

	return true;
}

/*checks a droids current body points to see if need to self repair*/
void droidSelfRepair(DROID *psDroid)
{
	CHECK_DROID(psDroid);

	if (!isVtolDroid(psDroid))
	{
		if (psDroid->body < psDroid->originalBody)
		{
			if (psDroid->asBits[COMP_REPAIRUNIT].nStat != 0)
			{
				psDroid->action = DACTION_DROIDREPAIR;
				setDroidActionTarget(psDroid, (BASE_OBJECT *)psDroid, 0);
				psDroid->actionStarted = gameTime;
				psDroid->actionPoints  = 0;
			}
		}
	}

	CHECK_DROID(psDroid);
}


/*Start a EW weapon droid working on a low resistance structure*/
BOOL droidStartRestore( DROID *psDroid )
{
	STRUCTURE	*psStruct;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->order == DORDER_RESTORE,
		"unitStartRestore: unit is not restoring" );
	psStruct = (STRUCTURE *)psDroid->psTarget;
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitStartRestore: target is not a structure" );

	psDroid->actionStarted = gameTime;
	psDroid->actionPoints  = 0;

	CHECK_DROID(psDroid);

	return true;
}

/*continue restoring a structure*/
BOOL droidUpdateRestore( DROID *psDroid )
{
	STRUCTURE		*psStruct;
	UDWORD			pointsToAdd, restorePoints;
	WEAPON_STATS	*psStats;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->action == DACTION_RESTORE,
		"unitUpdateRestore: unit is not restoring" );
	psStruct = (STRUCTURE *)psDroid->psTarget;
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitUpdateRestore: target is not a structure" );
	ASSERT( psStruct->pStructureType->resistance != 0,
		"unitUpdateRestore: invalid structure for EW" );

	ASSERT( psDroid->asWeaps[0].nStat > 0,
		"unitUpdateRestore: droid doean't have any weapons" );

	psStats = asWeaponStats + psDroid->asWeaps[0].nStat;

	ASSERT( psStats->weaponSubClass == WSC_ELECTRONIC,
		"unitUpdateRestore: unit's weapon is not EW" );

	restorePoints = calcDamage(weaponDamage(psStats, psDroid->player),
		psStats->weaponEffect,(BASE_OBJECT *)psStruct);

	pointsToAdd = restorePoints * (gameTime - psDroid->actionStarted) /
		GAME_TICKS_PER_SEC;

	psStruct->resistance = (SWORD)(psStruct->resistance + (pointsToAdd - psDroid->actionPoints));

	//store the amount just added
	psDroid->actionPoints = pointsToAdd;

	CHECK_DROID(psDroid);

	/* check if structure is restored */
	if (psStruct->resistance < (SDWORD)structureResistance(psStruct->
		pStructureType, psStruct->player))
	{
		return true;
	}
	else
	{
		addConsoleMessage(_("Structure Restored") ,DEFAULT_JUSTIFY,SYSTEM_MESSAGE);
		psStruct->resistance = (UWORD)structureResistance(psStruct->pStructureType,
			psStruct->player);
		return false;
	}
}

/* Code to have the droid's weapon assembly rock back upon firing */
void	droidUpdateRecoil( DROID *psDroid )
{
	UDWORD	percent;
	UDWORD	recoil;
	float	fraction;
	//Watermelon:added multiple weapon update
	UBYTE	i = 0;
	UBYTE	num_weapons = 0;

	CHECK_DROID(psDroid);

	if (psDroid->numWeaps > 1)
	{
		for(i = 0;i < psDroid->numWeaps;i++)
		{
			if (psDroid->asWeaps[i].nStat != 0)
			{
				num_weapons += (1 << (i+1));
			}
		}
	}
	else
	{
		if (psDroid->asWeaps[0].nStat == 0)
		{
			return;
		}
		num_weapons = 2;
	}

	for (i = 0; i < psDroid->numWeaps; i++)
	{
		if ( (num_weapons & (1 << (i+1))) )
		{
			/* Check it's actually got a weapon */
			if(psDroid->asWeaps[i].nStat == 0)
			{
				continue;
			}

			/* We have a weapon */
			if(gameTime > (psDroid->asWeaps[i].lastFired + DEFAULT_RECOIL_TIME) )
			{
				/* Recoil effect is over */
				psDroid->asWeaps[i].recoilValue = 0;
				continue;
			}

			/* Where should the assembly be? */
			percent = PERCENT((gameTime-psDroid->asWeaps[i].lastFired),DEFAULT_RECOIL_TIME);

			/* Outward journey */
			if(percent >= 50)
			{
				recoil = (100 - percent)/5;
			}
			/* Return journey */
			else
			{
				recoil = percent/5;
			}

			fraction =
				(float)asWeaponStats[psDroid->asWeaps[i].nStat].recoilValue / 100.f;

			recoil = (float)recoil * fraction;

			/* Put it into the weapon data */
			psDroid->asWeaps[i].recoilValue = recoil;
		}
	}
	CHECK_DROID(psDroid);
}


BOOL droidUpdateRepair( DROID *psDroid )
{
	STRUCTURE	*psStruct;
	UDWORD		iPointsToAdd, iRepairPoints;

	CHECK_DROID(psDroid);

	ASSERT( psDroid->action == DACTION_REPAIR,
		"unitUpdateRepair: unit does not have repair order" );
	psStruct = (STRUCTURE *)psDroid->psActionTarget[0];
	ASSERT( psStruct->type == OBJ_STRUCTURE,
		"unitUpdateRepair: target is not a structure" );

	iRepairPoints = constructorPoints(asConstructStats + psDroid->
		asBits[COMP_CONSTRUCT].nStat, psDroid->player);

	iPointsToAdd = iRepairPoints * (gameTime - psDroid->actionStarted) /
		GAME_TICKS_PER_SEC;

	/* add points to structure */
	structureRepair(psStruct, psDroid, iPointsToAdd - psDroid->actionPoints);
	/* store the amount just added */
	psDroid->actionPoints = iPointsToAdd;

	/* if not finished repair return true else complete repair and return false */
	if ( psStruct->body < structureBody(psStruct))
	{
		return true;
	}
	else
	{
		psStruct->body = (UWORD)structureBody(psStruct);
		return false;
	}
}

/*Updates a Repair Droid working on a damaged droid*/
BOOL droidUpdateDroidRepair(DROID *psRepairDroid)
{
	DROID		*psDroidToRepair;
	UDWORD		iPointsToAdd, iRepairPoints;
	Vector3i iVecEffect;

	CHECK_DROID(psRepairDroid);

	ASSERT( psRepairDroid->action == DACTION_DROIDREPAIR,
		"unitUpdateUnitRepair: unit does not have unit repair order" );
	ASSERT( psRepairDroid->asBits[COMP_REPAIRUNIT].nStat != 0,
		"unitUpdateUnitRepair: unit does not have a repair turret" );

	psDroidToRepair = (DROID *)psRepairDroid->psActionTarget[0];
	ASSERT( psDroidToRepair->type == OBJ_DROID,
		"unitUpdateUnitRepair: target is not a unit" );

	// FIXME: add power cost for repair
	// remember that self repair is free

	iRepairPoints = repairPoints(asRepairStats + psRepairDroid->
		asBits[COMP_REPAIRUNIT].nStat, psRepairDroid->player);

	//if self repair then add repair points depending on the time delay for the stat
	if (psRepairDroid == psDroidToRepair)
	{
		iPointsToAdd = iRepairPoints * (gameTime - psRepairDroid->actionStarted) /
			(asRepairStats + psRepairDroid->asBits[COMP_REPAIRUNIT].nStat)->time;
	}
	else
	{
		iPointsToAdd = iRepairPoints * (gameTime - psRepairDroid->actionStarted) /
			GAME_TICKS_PER_SEC;
	}

    iPointsToAdd -= psRepairDroid->actionPoints;

    if (iPointsToAdd)
    {
        //just add the points if the power cost is negligable
        //if these points would make the droid healthy again then just add
        if (psDroidToRepair->body + iPointsToAdd >= psDroidToRepair->originalBody)
        {
            //anothe HACK but sorts out all the rounding errors when values get small
            psDroidToRepair->body += iPointsToAdd;
            psRepairDroid->actionPoints += iPointsToAdd;
        }
        else
        {
			psDroidToRepair->body += iPointsToAdd;
			psRepairDroid->actionPoints += iPointsToAdd;
        }
    }

	/* add plasma repair effect whilst being repaired */
	if ((ONEINFIVE) && (psDroidToRepair->visible[selectedPlayer]))
	{
		iVecEffect.x = psDroidToRepair->pos.x + DROID_REPAIR_SPREAD;
		iVecEffect.y = psDroidToRepair->pos.z + rand()%8;;
		iVecEffect.z = psDroidToRepair->pos.y + DROID_REPAIR_SPREAD;
		effectGiveAuxVar(90+rand()%20);
		addEffect(&iVecEffect,EFFECT_EXPLOSION,EXPLOSION_TYPE_LASER,false,NULL,0);
		droidAddWeldSound( iVecEffect );
	}

	CHECK_DROID(psRepairDroid);

	/* if not finished repair return true else complete repair and return false */
	if (psDroidToRepair->body < psDroidToRepair->originalBody)
	{
		return true;
	}
	else
	{
        //reset the power accrued
		psDroidToRepair->body = psDroidToRepair->originalBody;
		return false;
	}
}

/* load the Droid stats for the components from the Access database */
BOOL loadDroidTemplates(const char *pDroidData, UDWORD bufferSize)
{
	unsigned int NumDroids = numCR(pDroidData, bufferSize), line;
	BOOL bDefaultTemplateFound = false;

	/* init default template */
	memset( &sDefaultDesignTemplate, 0, sizeof(DROID_TEMPLATE) );

	for (line = 0; line < NumDroids; line++)
	{
		char componentName[MAX_STR_LENGTH];
		unsigned int player;
		int cnt;
		DROID_TEMPLATE *pDroidDesign = malloc(sizeof(DROID_TEMPLATE));
		if (pDroidDesign == NULL)
		{
			debug(LOG_ERROR, "loadDroidTemplates: pDroidDesign: Out of memory");
			return false;
		}
		memset(pDroidDesign, 0, sizeof(DROID_TEMPLATE));

		//read the data into the storage - the data is delimited using comma's
		sscanf(pDroidData, "%[^','],%d,%n", componentName, &pDroidDesign->multiPlayerID, &cnt);
		pDroidData += cnt;

		// Store unique name in pName
		pDroidDesign->pName = strdup(componentName);

		// Store translated name in aName
		if (!getDroidResourceName(componentName))
		{
			free(pDroidDesign->pName);
			free(pDroidDesign);
			return false;
		}
		sstrcpy(pDroidDesign->aName, componentName);

		//read in Body Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
		pDroidData += cnt;

		//get the Body stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_BODY] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asBodyStats;
			const size_t size = sizeof(BODY_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}

			for (inc = 0; inc < numBodyStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_BODY] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Body component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in Brain Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
				pDroidData += cnt;

		//get the Brain stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_BRAIN] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asBrainStats;
			const size_t size = sizeof(BRAIN_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}

			for (inc=0; inc < numBrainStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_BRAIN] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Brain component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
//				DBERROR(("Brain component not found for droid %s", pDroidDesign->pName));
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in Construct Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
				pDroidData += cnt;

		//get the Construct stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_CONSTRUCT] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asConstructStats;
			const size_t size = sizeof(CONSTRUCT_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
			for (inc=0; inc < numConstructStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_CONSTRUCT] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Construct component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in Ecm Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
				pDroidData += cnt;

		//get the Ecm stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_ECM] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asECMStats;
			const size_t size = sizeof(ECM_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
			for (inc=0; inc < numECMStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_ECM] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "ECM component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in player id - Access decides the order -crap hey?
		sscanf(pDroidData, "%d,%n", &player,&cnt);
		pDroidData += cnt;

		if (getTemplateFromUniqueName(pDroidDesign->pName, player))
		{
			debug( LOG_ERROR, "Duplicate template %s", pDroidDesign->pName );
			abort();
			free(pDroidDesign->pName);
			free(pDroidDesign);
			return false;
		}

		//read in Propulsion Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
		pDroidData += cnt;

		//get the Propulsion stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_PROPULSION] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asPropulsionStats;
			const size_t size = sizeof(PROPULSION_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
			for (inc=0; inc < numPropulsionStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_PROPULSION] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Propulsion component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in Repair Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
		pDroidData += cnt;

		//get the Repair stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_REPAIRUNIT] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asRepairStats;
			const size_t size = sizeof(REPAIR_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
			for (inc=0; inc < numRepairStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_REPAIRUNIT] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Repair component not found for droid %s", getTemplateName(pDroidDesign) );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in droid type - only interested if set to PERSON
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
		pDroidData += cnt;

		if (!strcmp(componentName, "PERSON"))
		{
			pDroidDesign->droidType = DROID_PERSON;
		}
		else if (!strcmp(componentName, "CYBORG"))
		{
			pDroidDesign->droidType = DROID_CYBORG;
		}
		else if (!strcmp(componentName, "CYBORG_SUPER"))
		{
			pDroidDesign->droidType = DROID_CYBORG_SUPER;
		}
		else if (!strcmp(componentName, "CYBORG_CONSTRUCT"))
		{
			pDroidDesign->droidType = DROID_CYBORG_CONSTRUCT;
		}
		else if (!strcmp(componentName, "CYBORG_REPAIR"))
		{
			pDroidDesign->droidType = DROID_CYBORG_REPAIR;
		}
		else if (!strcmp(componentName, "TRANSPORTER"))
		{
			pDroidDesign->droidType = DROID_TRANSPORTER;
		}
		else if (!strcmp(componentName, "ZNULLDROID"))
		{
			pDroidDesign->droidType = DROID_DEFAULT;
			bDefaultTemplateFound = true;
		}

		//read in Sensor Name
		sscanf(pDroidData, "%[^','],%n", componentName, &cnt);
				pDroidData += cnt;

		//get the Sensor stats pointer
		if (!strcmp(componentName,"0"))
		{
			pDroidDesign->asParts[COMP_SENSOR] = NULL_COMP;
		}
		else
		{
			COMPONENT_STATS *pStats = (COMPONENT_STATS *)asSensorStats;
			const size_t size = sizeof(SENSOR_STATS);
			unsigned int inc = 0;
			BOOL found = false;

			if (!getResourceName(componentName))
			{
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}

			for (inc=0; inc < numSensorStats; inc++)
			{
				//compare the names
				if (!strcmp(componentName, pStats->pName))
				{
					pDroidDesign->asParts[COMP_SENSOR] = inc;
					found = true;
					break;
				}
				pStats = ((COMPONENT_STATS*)((UBYTE*)pStats + size));
			}
			if (!found)
			{
				debug( LOG_ERROR, "Sensor not found for droid Template: %s", pDroidDesign->aName );
				abort();
				free(pDroidDesign->pName);
				free(pDroidDesign);
				return false;
			}
		}

		//read in totals
		sscanf(pDroidData,"%d", &pDroidDesign->numWeaps);
		//check that not allocating more weapons than allowed
		if (pDroidDesign->numWeaps > DROID_MAXWEAPS)
		{
			debug( LOG_ERROR, "Too many weapons have been allocated for droid Template: %s", pDroidDesign->aName );
			abort();
			free(pDroidDesign->pName);
			free(pDroidDesign);
			return false;
		}


		pDroidDesign->ref = REF_TEMPLATE_START + line;
/*	Loaded in from the database now AB 29/10/98
			pDroidDesign->multiPlayerID = line;			// another unique number, just for multiplayer stuff.
*/
		/* store global default design if found else
		* store in the appropriate array
		*/
		if ( pDroidDesign->droidType == DROID_DEFAULT )
		{
			// NOTE: sDefaultDesignTemplate.pName takes ownership
			//       of the memory allocated to pDroidDesign->pName
			//       here. Which is good because pDroidDesign leaves
			//       scope here anyway.
			memcpy( &sDefaultDesignTemplate, pDroidDesign, sizeof(DROID_TEMPLATE) );
			free(pDroidDesign);
		}
		else
		{
			pDroidDesign->psNext = apsDroidTemplates[player];
			apsDroidTemplates[player] = pDroidDesign;
		}

		//increment the pointer to the start of the next record
		pDroidData = strchr(pDroidData,'\n') + 1;
	}

	if ( bDefaultTemplateFound == false )
	{
		debug( LOG_ERROR, "loadUnitTemplates: default template not found\n" );
		abort();
		return false;
	}

	return true;
}

/*initialise the template build and power points */
void initTemplatePoints(void)
{
	UDWORD			player;
	DROID_TEMPLATE	*pDroidDesign;

	for (player=0; player < MAX_PLAYERS; player++)
	{
		for(pDroidDesign = apsDroidTemplates[player]; pDroidDesign != NULL;
			pDroidDesign = pDroidDesign->psNext)
		{
			//calculate the total build points
			pDroidDesign->buildPoints = calcTemplateBuild(pDroidDesign);
			//calc the total power points
			pDroidDesign->powerPoints = calcTemplatePower(pDroidDesign);
		}
	}
}


// return whether a droid is IDF
BOOL idfDroid(DROID *psDroid)
{
    //add Cyborgs
	//if (psDroid->droidType != DROID_WEAPON)
    if (!(psDroid->droidType == DROID_WEAPON || psDroid->droidType == DROID_CYBORG ||
        psDroid->droidType == DROID_CYBORG_SUPER))
	{
		return false;
	}

	if (proj_Direct(psDroid->asWeaps[0].nStat + asWeaponStats))
	{
		return false;
	}

	return true;
}

// return whether a template is for an IDF droid
BOOL templateIsIDF(DROID_TEMPLATE *psTemplate)
{
    //add Cyborgs
    if (!(psTemplate->droidType == DROID_WEAPON || psTemplate->droidType == DROID_CYBORG ||
        psTemplate->droidType == DROID_CYBORG_SUPER))
	{
		return false;
	}

	if (proj_Direct(psTemplate->asWeaps[0] + asWeaponStats))
	{
		return false;
	}

	return true;
}

/* Return the type of a droid */
DROID_TYPE droidType(DROID *psDroid)
{
	return psDroid->droidType;
}


/* Return the type of a droid from it's template */
DROID_TYPE droidTemplateType(DROID_TEMPLATE *psTemplate)
{
	DROID_TYPE	type;

	if (psTemplate->droidType == DROID_PERSON)
	{
		type = DROID_PERSON;
	}
	else if (psTemplate->droidType == DROID_CYBORG)
	{
		type = DROID_CYBORG;
	}
	else if (psTemplate->droidType == DROID_CYBORG_SUPER)
	{
		type = DROID_CYBORG_SUPER;
	}
	else if (psTemplate->droidType == DROID_CYBORG_CONSTRUCT)
	{
		type = DROID_CYBORG_CONSTRUCT;
	}
	else if (psTemplate->droidType == DROID_CYBORG_REPAIR)
	{
		type = DROID_CYBORG_REPAIR;
	}
	else if (psTemplate->droidType == DROID_TRANSPORTER)
	{
		type = DROID_TRANSPORTER;
	}
	else if (psTemplate->asParts[COMP_BRAIN] != 0)
	{
		type = DROID_COMMAND;
	}
	else if ((asSensorStats + psTemplate->asParts[COMP_SENSOR])->location == LOC_TURRET)
	{
		type = DROID_SENSOR;
	}
	else if ((asECMStats + psTemplate->asParts[COMP_ECM])->location == LOC_TURRET)
	{
		type = DROID_ECM;
	}
	else if (psTemplate->asParts[COMP_CONSTRUCT] != 0)
	{
		type = DROID_CONSTRUCT;
	}
	else if ((asRepairStats + psTemplate->asParts[COMP_REPAIRUNIT])->location == LOC_TURRET)
	{
		type = DROID_REPAIR;
	}
	else if ( psTemplate->asWeaps[0] != 0 )
	{
		type = DROID_WEAPON;
	}
	/* Watermelon:with more than weapon is still a DROID_WEAPON */
	else if ( psTemplate->numWeaps > 1)
	{
		type = DROID_WEAPON;
	}
	else
	{
		type = DROID_DEFAULT;
	}

	return type;
}

//Load the weapons assigned to Droids in the Access database
BOOL loadDroidWeapons(const char *pWeaponData, UDWORD bufferSize)
{
	unsigned int NumWeapons = numCR(pWeaponData, bufferSize);
	unsigned int SkippedWeaponCount = 0;
	unsigned int line = 0;

	ASSERT(NumWeapons > 0, "template without weapons");

	for (line = 0; line < NumWeapons; line++)
	{
		unsigned int player;
		char WeaponName[DROID_MAXWEAPS][MAX_STR_LENGTH] = {{'\0'}},
			TemplateName[MAX_STR_LENGTH] = {'\0'};

		//read the data into the storage - the data is delimeted using comma's
		sscanf(pWeaponData, "%[^','],%[^','],%[^','],%[^','],%d",
			TemplateName, WeaponName[0], WeaponName[1], WeaponName[2], &player);

		if (player < MAX_PLAYERS)
		{
			unsigned int j;

			DROID_TEMPLATE *pTemplate = getTemplateFromUniqueName(TemplateName, player);

			/* if Template not found - try default design */
			if (!pTemplate)
			{
				if (strcmp(TemplateName, sDefaultDesignTemplate.pName) == 0)
				{
					pTemplate = &sDefaultDesignTemplate;
				}
				else
				{
					debug( LOG_ERROR, "Unable to find Template - %s", TemplateName );
					abort();
					return false;
				}
			}

			ASSERT(pTemplate->numWeaps <= DROID_MAXWEAPS, "stack corruption unavoidable");

			for (j = 0; j < pTemplate->numWeaps; j++)
			{
				int incWpn;

				if (!getResourceName(WeaponName[j]))
				{
					return false;
				}

				incWpn = getCompFromName(COMP_WEAPON, WeaponName[j]);

				//if weapon not found - error
				if (incWpn == -1)
				{
					debug( LOG_ERROR, "Unable to find Weapon %s for template %s",
						WeaponName[j], TemplateName );
					abort();
					return false;
				}
				else
				{
					//Weapon found, alloc this to the current Template
					pTemplate->asWeaps[pTemplate->storeCount] = incWpn;

					//check not allocating more than allowed
					if (pTemplate->storeCount > pTemplate->numWeaps)
					{
						debug( LOG_ERROR, "Trying to allocate more weapons than allowed for Template - %s", TemplateName );
						abort();
						return false;
					}
					//check valid weapon/propulsion
					if (!checkValidWeaponForProp(pTemplate))
					{
						debug( LOG_ERROR, "Weapon is invalid for air propulsion for template %s", pTemplate->aName );
						abort();

						return false;
					}

					pTemplate->storeCount++;
				}
			}
		}
		else
		{
			SkippedWeaponCount++;
		}

		//increment the pointer to the start of the next record
		pWeaponData = strchr(pWeaponData, '\n') + 1;
	}

	if (SkippedWeaponCount > 0)
	{
		debug( LOG_ERROR, "Illegal player number in %d droid weapons", SkippedWeaponCount );
		abort();
	}

	return true;
}

//free the storage for the droid templates
BOOL droidTemplateShutDown(void)
{
	unsigned int player;

	for (player = 0; player < MAX_PLAYERS; player++)
	{
		DROID_TEMPLATE *pTemplate, *pNext;

		for (pTemplate = apsDroidTemplates[player]; pTemplate != NULL; pTemplate = pNext)
		{
			pNext = pTemplate->psNext;
			ASSERT(sDefaultDesignTemplate.pName != pTemplate->pName, "We'll soon be getting a double free()!!!");
			if (pTemplate->pName != sDefaultDesignTemplate.pName)
			{
				free(pTemplate->pName);
			}
			free(pTemplate);
		}
		apsDroidTemplates[player] = NULL;
	}
	free(sDefaultDesignTemplate.pName);
	sDefaultDesignTemplate.pName = NULL;

	return true;
}

/* Calculate the weight of a droid from it's template */
UDWORD calcDroidWeight(DROID_TEMPLATE *psTemplate)
{
	UDWORD weight, i;

	/* Get the basic component weight */
	weight =
		(asBodyStats + psTemplate->asParts[COMP_BODY])->weight +
		(asBrainStats + psTemplate->asParts[COMP_BRAIN])->weight +
		//(asPropulsionStats + psTemplate->asParts[COMP_PROPULSION])->weight +
		(asSensorStats + psTemplate->asParts[COMP_SENSOR])->weight +
		(asECMStats + psTemplate->asParts[COMP_ECM])->weight +
		(asRepairStats + psTemplate->asParts[COMP_REPAIRUNIT])->weight +
		(asConstructStats + psTemplate->asParts[COMP_CONSTRUCT])->weight;

	/* propulsion weight is a percentage of the body weight */
	weight += (((asPropulsionStats + psTemplate->asParts[COMP_PROPULSION])->weight *
		(asBodyStats + psTemplate->asParts[COMP_BODY])->weight) / 100);

	/* Add the weapon weight */
	for(i=0; i<psTemplate->numWeaps; i++)
	{
		weight += (asWeaponStats + psTemplate->asWeaps[i])->weight;
	}

	return weight;
}

/* Calculate the body points of a droid from it's template */
UDWORD calcTemplateBody(DROID_TEMPLATE *psTemplate, UBYTE player)
{
	UDWORD body, i;

	if (psTemplate == NULL)
	{
		return 0;
	}
	/* Get the basic component body points */
	body =
		(asBodyStats + psTemplate->asParts[COMP_BODY])->body +
		(asBrainStats + psTemplate->asParts[COMP_BRAIN])->body +
		(asSensorStats + psTemplate->asParts[COMP_SENSOR])->body +
		(asECMStats + psTemplate->asParts[COMP_ECM])->body +
		(asRepairStats + psTemplate->asParts[COMP_REPAIRUNIT])->body +
		(asConstructStats + psTemplate->asParts[COMP_CONSTRUCT])->body;

	/* propulsion body points are a percentage of the bodys' body points */
	body += (((asPropulsionStats + psTemplate->asParts[COMP_PROPULSION])->body *
		(asBodyStats + psTemplate->asParts[COMP_BODY])->body) / 100);

	/* Add the weapon body points */
	for(i=0; i<psTemplate->numWeaps; i++)
	{
		body += asWeaponStats[psTemplate->asWeaps[i]].body;
	}

	//add on any upgrade value that may need to be applied
	body += (body * asBodyUpgrade[player]->body / 100);
	return body;
}

/* Calculate the base body points of a droid without upgrades*/
UDWORD calcDroidBaseBody(DROID *psDroid)
{
	//Watermelon:re-enabled i;
	UDWORD      body, i;

	if (psDroid == NULL)
	{
		return 0;
	}
	/* Get the basic component body points */
	body =
		(asBodyStats + psDroid->asBits[COMP_BODY].nStat)->body +
		(asBrainStats + psDroid->asBits[COMP_BRAIN].nStat)->body +
		(asSensorStats + psDroid->asBits[COMP_SENSOR].nStat)->body +
		(asECMStats + psDroid->asBits[COMP_ECM].nStat)->body +
		(asRepairStats + psDroid->asBits[COMP_REPAIRUNIT].nStat)->body +
		(asConstructStats + psDroid->asBits[COMP_CONSTRUCT].nStat)->body;

	/* propulsion body points are a percentage of the bodys' body points */
	body += (((asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat)->body *
		(asBodyStats + psDroid->asBits[COMP_BODY].nStat)->body) / 100);

	/* Add the weapon body points */
	for(i=0; i<psDroid->numWeaps; i++)
	{
		if (psDroid->asWeaps[i].nStat > 0)
		{
			//body += (asWeaponStats + psDroid->asWeaps[i].nStat)->body;
			body += (asWeaponStats + psDroid->asWeaps[i].nStat)->body;
		}
	}

	return body;
}


/* Calculate the base speed of a droid from it's template */
UDWORD calcDroidBaseSpeed(DROID_TEMPLATE *psTemplate, UDWORD weight, UBYTE player)
{
	UDWORD	speed;
	//Watermelon:engine output bonus? 150%
	float eoBonus = 1.5f;

	if (psTemplate->droidType == DROID_CYBORG ||
		psTemplate->droidType == DROID_CYBORG_SUPER ||
        psTemplate->droidType == DROID_CYBORG_CONSTRUCT ||
        psTemplate->droidType == DROID_CYBORG_REPAIR)
	{
		speed = (asPropulsionTypes[(asPropulsionStats + psTemplate->
            asParts[COMP_PROPULSION])->propulsionType].powerRatioMult *
            bodyPower(asBodyStats + psTemplate->asParts[COMP_BODY],
            player, CYBORG_BODY_UPGRADE)) / weight;
	}
	else
	{
		speed = (asPropulsionTypes[(asPropulsionStats + psTemplate->
            asParts[COMP_PROPULSION])->propulsionType].powerRatioMult *
            bodyPower(asBodyStats + psTemplate->asParts[COMP_BODY],
			player, DROID_BODY_UPGRADE)) / weight;
	}

	// reduce the speed of medium/heavy VTOLs
	if (asPropulsionStats[psTemplate->asParts[COMP_PROPULSION]].propulsionType == PROPULSION_TYPE_LIFT)
	{
		if ((asBodyStats + psTemplate->asParts[COMP_BODY])->size == SIZE_HEAVY)
		{
			speed /= 4;
		}
		else if ((asBodyStats + psTemplate->asParts[COMP_BODY])->size == SIZE_MEDIUM)
		{
			speed = 3 * speed / 4;
		}
	}

	// Wateremelon:applies the engine output bonus if output > weight
	if ( (asBodyStats + psTemplate->asParts[COMP_BODY])->powerOutput > weight )
	{
		speed *= eoBonus;
	}

	return speed;
}


/* Calculate the speed of a droid over a terrain */
UDWORD calcDroidSpeed(UDWORD baseSpeed, UDWORD terrainType, UDWORD propIndex, UDWORD level)
{
	PROPULSION_STATS	*propulsion = asPropulsionStats + propIndex;
	UDWORD				speed;

    speed  = baseSpeed;
    // Factor in terrain
    speed *= getSpeedFactor(terrainType, propulsion->propulsionType);
    speed /= 100;
    // Factor in experience
    speed *= (100 + EXP_SPEED_BONUS * level);
    speed /= 100;

	// Need to ensure doesn't go over the max speed possible for this propulsion
	if (speed > propulsion->maxSpeed)
	{
		speed = propulsion->maxSpeed;
	}

	return speed;
}

/* Calculate the points required to build the template - used to calculate time*/
UDWORD calcTemplateBuild(DROID_TEMPLATE *psTemplate)
{
	UDWORD	build, i;

	build = (asBodyStats + psTemplate->asParts[COMP_BODY])->buildPoints +
	(asBrainStats + psTemplate->asParts[COMP_BRAIN])->buildPoints +
	(asSensorStats + psTemplate->asParts[COMP_SENSOR])->buildPoints +
	(asECMStats + psTemplate->asParts[COMP_ECM])->buildPoints +
	(asRepairStats + psTemplate->asParts[COMP_REPAIRUNIT])->buildPoints +
	(asConstructStats + psTemplate->asParts[COMP_CONSTRUCT])->buildPoints;

	/* propulsion build points are a percentage of the bodys' build points */
	build += (((asPropulsionStats + psTemplate->asParts[COMP_PROPULSION])->buildPoints *
		(asBodyStats + psTemplate->asParts[COMP_BODY])->buildPoints) / 100);

	//add weapon power
	for(i=0; i<psTemplate->numWeaps; i++)
	{
		ASSERT( psTemplate->asWeaps[i]<numWeaponStats,
			//"Invalid Template weapon for %s", psTemplate->pName );
			"Invalid Template weapon for %s", getTemplateName(psTemplate) );
		build += (asWeaponStats + psTemplate->asWeaps[i])->buildPoints;
	}

	return build;
}


/* Calculate the power points required to build/maintain a template */
UDWORD	calcTemplatePower(DROID_TEMPLATE *psTemplate)
{
	UDWORD power, i;

	//get the component power
	power = (asBodyStats + psTemplate->asParts[COMP_BODY])->buildPower;
	power += (asBrainStats + psTemplate->asParts[COMP_BRAIN])->buildPower;
	power += (asSensorStats + psTemplate->asParts[COMP_SENSOR])->buildPower;
	power += (asECMStats + psTemplate->asParts[COMP_ECM])->buildPower;
	power += (asRepairStats + psTemplate->asParts[COMP_REPAIRUNIT])->buildPower;
	power += (asConstructStats + psTemplate->asParts[COMP_CONSTRUCT])->buildPower;

	/* propulsion power points are a percentage of the bodys' power points */
	power += (((asPropulsionStats + psTemplate->asParts[COMP_PROPULSION])->buildPower *
		(asBodyStats + psTemplate->asParts[COMP_BODY])->buildPower) / 100);

	//add weapon power
	for(i=0; i<psTemplate->numWeaps; i++)
	{
		power += (asWeaponStats + psTemplate->asWeaps[i])->buildPower;
	}

	return power;
}


/* Calculate the power points required to build/maintain a droid */
UDWORD	calcDroidPower(DROID *psDroid)
{
	//Watermelon:re-enabled i
	UDWORD      power, i;

	//get the component power
	power = (asBodyStats + psDroid->asBits[COMP_BODY].nStat)->buildPower +
	(asBrainStats + psDroid->asBits[COMP_BRAIN].nStat)->buildPower +
	//(asPropulsionStats + psDroid->asBits[COMP_PROPULSION])->buildPower +
	(asSensorStats + psDroid->asBits[COMP_SENSOR].nStat)->buildPower +
	(asECMStats + psDroid->asBits[COMP_ECM].nStat)->buildPower +
	(asRepairStats + psDroid->asBits[COMP_REPAIRUNIT].nStat)->buildPower +
	(asConstructStats + psDroid->asBits[COMP_CONSTRUCT].nStat)->buildPower;

	/* propulsion power points are a percentage of the bodys' power points */
	power += (((asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat)->buildPower *
		(asBodyStats + psDroid->asBits[COMP_BODY].nStat)->buildPower) / 100);

	//add weapon power
	for(i=0; i<psDroid->numWeaps; i++)
	{
		if (psDroid->asWeaps[i].nStat > 0)
		{
			//power += (asWeaponStats + psDroid->asWeaps[i].nStat)->buildPower;
			power += (asWeaponStats + psDroid->asWeaps[i].nStat)->buildPower;
		}
	}

	return power;
}

UDWORD calcDroidPoints(DROID *psDroid)
{
	unsigned int i;
	int points;

	points  = getBodyStats(psDroid)->buildPoints;
	points += getBrainStats(psDroid)->buildPoints;
	points += getPropulsionStats(psDroid)->buildPoints;
	points += getSensorStats(psDroid)->buildPoints;
	points += getECMStats(psDroid)->buildPoints;
	points += getRepairStats(psDroid)->buildPoints;
	points += getConstructStats(psDroid)->buildPoints;

	for (i = 0; i < psDroid->numWeaps; i++)
	{
		points += (asWeaponStats + psDroid->asWeaps[i].nStat)->buildPoints;
	}

	return points;
}

//Builds an instance of a Droid - the x/y passed in are in world coords.
DROID* buildDroid(DROID_TEMPLATE *pTemplate, UDWORD x, UDWORD y, UDWORD player,
				  BOOL onMission)
{
	DROID			*psDroid;
	DROID_GROUP		*psGrp;
	UDWORD			inc;
	UDWORD			numKills;
	SDWORD			i, experienceLoc;
	HIT_SIDE		impact_side;

	// Don't use this assertion in single player, since droids can finish building while on an away mission
	ASSERT(!bMultiPlayer || worldOnMap(x,y), "the build locations are not on the map");

	//allocate memory
	psDroid = createDroid(player);
	if (psDroid == NULL)
	{
		debug(LOG_NEVER, "unit build: unable to create\n");
		ASSERT(!"out of memory", "Cannot get the memory for the unit");
		return NULL;
	}
	psDroid->sMove.asPath = NULL;

	//fill in other details

	droidSetName(psDroid, pTemplate->aName);

	// Set the droids type
	psDroid->droidType = droidTemplateType(pTemplate);

	psDroid->pos.x = (UWORD)x;
	psDroid->pos.y = (UWORD)y;

	//don't worry if not on homebase cos not being drawn yet
	if (!onMission)
	{
		//set droid height
		psDroid->pos.z = map_Height(psDroid->pos.x, psDroid->pos.y);
	}

	psDroid->cluster = 0;
	psDroid->psGroup = NULL;
	psDroid->psGrpNext = NULL;
	if ( (psDroid->droidType == DROID_TRANSPORTER) ||
		 (psDroid->droidType == DROID_COMMAND) )
	{
		if (!grpCreate(&psGrp))
		{
			debug(LOG_NEVER, "unit build: unable to create group\n");
			ASSERT(!"unable to create group", "Can't create unit because can't create group");
			free(psDroid);
			return NULL;
		}
		grpJoin(psGrp, psDroid);
	}

	psDroid->order = DORDER_NONE;
	psDroid->orderX = 0;
	psDroid->orderY = 0;
	psDroid->orderX2 = 0;
	psDroid->orderY2 = 0;
	psDroid->secondaryOrder = DSS_ARANGE_DEFAULT | DSS_REPLEV_NEVER | DSS_ALEV_ALWAYS | DSS_HALT_GUARD;
	psDroid->action = DACTION_NONE;
	psDroid->actionX = 0;
	psDroid->actionY = 0;
	psDroid->psTarStats = NULL;
	psDroid->psTarget = NULL;

	for(i = 0;i < DROID_MAXWEAPS;i++)
	{
		psDroid->psActionTarget[i] = NULL;
		psDroid->asWeaps[i].lastFired = 0;
		psDroid->asWeaps[i].nStat = 0;
		psDroid->asWeaps[i].ammo = 0;
		psDroid->asWeaps[i].recoilValue = 0;
		psDroid->asWeaps[i].rotation = 0;
		psDroid->asWeaps[i].pitch = 0;
	}

	psDroid->listSize = 0;
	memset(psDroid->asOrderList, 0, sizeof(ORDER_LIST)*ORDER_LIST_MAX);

	psDroid->iAudioID = NO_SOUND;
	psDroid->lastSync = 0;

	psDroid->psCurAnim = NULL;
	psDroid->group = UBYTE_MAX;
	psDroid->psBaseStruct = NULL;

	// find the highest stored experience
	if ((psDroid->droidType != DROID_CONSTRUCT) &&
        (psDroid->droidType != DROID_CYBORG_CONSTRUCT) &&
		(psDroid->droidType != DROID_REPAIR) &&
        (psDroid->droidType != DROID_CYBORG_REPAIR) &&
		(psDroid->droidType != DROID_TRANSPORTER))
	{
		numKills = 0;
		experienceLoc = 0;
		for(i=0; i<MAX_RECYCLED_DROIDS; i++)
		{
			if (aDroidExperience[player][i] > numKills)
			{
				numKills = aDroidExperience[player][i];
				experienceLoc = i;
			}
		}
		aDroidExperience[player][experienceLoc] = 0;
		psDroid->experience = (UWORD)numKills;
	}
	else
	{
		psDroid->experience = 0;
	}

	droidSetBits(pTemplate,psDroid);

	//calculate the droids total weight
	psDroid->weight = calcDroidWeight(pTemplate);

	// Initialise the movement stuff
	psDroid->baseSpeed = calcDroidBaseSpeed(pTemplate, psDroid->weight, (UBYTE)player);

	initDroidMovement(psDroid);

	psDroid->direction = 0;
	psDroid->pitch =  0;
	psDroid->roll = 0;
	psDroid->selected = false;
	psDroid->lastEmission = 0;
	psDroid->bTargetted = false;
	psDroid->timeLastHit = UDWORD_MAX;
	psDroid->lastHitWeapon = UDWORD_MAX;	// no such weapon

	// it was never drawn before
	psDroid->sDisplay.frameNumber = 0;

	//allocate 'easy-access' data!
	objSensorCache((BASE_OBJECT *)psDroid, asSensorStats + pTemplate->asParts[COMP_SENSOR]);
	objEcmCache((BASE_OBJECT *)psDroid, asECMStats + pTemplate->asParts[COMP_ECM]);
	psDroid->body = calcTemplateBody(pTemplate, (UBYTE)player);
	psDroid->originalBody = psDroid->body;

	if (cyborgDroid(psDroid))
	{
		for (inc = 0; inc < WC_NUM_WEAPON_CLASSES; inc++)
		{
			for (impact_side = 0;impact_side < NUM_HIT_SIDES;impact_side=impact_side+1)
			{
				psDroid->armour[impact_side][inc] = bodyArmour(asBodyStats + pTemplate->
				asParts[COMP_BODY], (UBYTE)player, CYBORG_BODY_UPGRADE, (WEAPON_CLASS)inc, impact_side);
			}
		}
	}
	else
	{
		for (inc = 0; inc < WC_NUM_WEAPON_CLASSES; inc++)
		{
			for (impact_side = 0;impact_side < NUM_HIT_SIDES;impact_side=impact_side+1)
			{
				psDroid->armour[impact_side][inc] = bodyArmour(asBodyStats + pTemplate->
					asParts[COMP_BODY], (UBYTE)player, DROID_BODY_UPGRADE, (WEAPON_CLASS)inc, impact_side);
			}
		}
	}

	//init the resistance to indicate no EW performed on this droid
	psDroid->resistance = ACTION_START_TIME;

	memset(psDroid->visible, 0, sizeof(psDroid->visible));
	psDroid->visible[psDroid->player] = UBYTE_MAX;
	psDroid->died = 0;
	psDroid->inFire = false;
	psDroid->burnStart = 0;
	psDroid->burnDamage = 0;
	psDroid->sDisplay.screenX = OFF_SCREEN;
	psDroid->sDisplay.screenY = OFF_SCREEN;
	psDroid->sDisplay.screenR = 0;

	/* Set droid's initial illumination */
	psDroid->illumination = UBYTE_MAX;
	psDroid->sDisplay.imd = BODY_IMD(psDroid, psDroid->player);

	//don't worry if not on homebase cos not being drawn yet
	if (!onMission)
	{
		/* People always stand upright */
		if(psDroid->droidType != DROID_PERSON)
		{
			updateDroidOrientation(psDroid);
		}
		visTilesUpdate((BASE_OBJECT *)psDroid, rayTerrainCallback);
		gridAddObject((BASE_OBJECT *)psDroid);
 		clustNewDroid(psDroid);
	}

	// ajl. droid will be created, so inform others
	if(bMultiPlayer)
	{
		if (SendDroid(pTemplate,  x,  y,  (UBYTE)player, psDroid->id) == false)
		{
			return NULL;
		}
	}

	/* transporter-specific stuff */
	if (psDroid->droidType == DROID_TRANSPORTER)
	{
		//add transporter launch button if selected player and not a reinforcable situation
		if ( player == selectedPlayer && !missionCanReEnforce())
		{
			(void)intAddTransporterLaunch(psDroid);
		}

		//set droid height to be above the terrain
		psDroid->pos.z += TRANSPORTER_HOVER_HEIGHT;

		/* reset halt secondary order from guard to hold */
		secondarySetState( psDroid, DSO_HALTTYPE, DSS_HALT_HOLD );
	}

	if(player == selectedPlayer)
	{
		scoreUpdateVar(WD_UNITS_BUILT);
	}

	return psDroid;
}

//initialises the droid movement model
void initDroidMovement(DROID *psDroid)
{
	memset(&psDroid->sMove, 0, sizeof(MOVE_CONTROL));

	psDroid->sMove.fx = psDroid->pos.x;
	psDroid->sMove.fy = psDroid->pos.y;
	psDroid->sMove.fz = psDroid->pos.z;
	psDroid->sMove.speed = 0.0f;
	psDroid->sMove.moveDir = 0.0f;
}

// Set the asBits in a DROID structure given it's template.
void droidSetBits(DROID_TEMPLATE *pTemplate,DROID *psDroid)
{
	UDWORD						inc;

	psDroid->droidType = droidTemplateType(pTemplate);

	psDroid->direction = 0;
	psDroid->pitch =  0;
	psDroid->roll = 0;
	psDroid->numWeaps = pTemplate->numWeaps;
	for (inc = 0;inc < psDroid->numWeaps;inc++)
	{
		psDroid->asWeaps[inc].rotation = 0;
		psDroid->asWeaps[inc].pitch = 0;
	}

	psDroid->body = calcTemplateBody(pTemplate, psDroid->player);
	psDroid->originalBody = psDroid->body;

	//create the droids weapons
	if (pTemplate->numWeaps > 0)
	{

		for (inc=0; inc < pTemplate->numWeaps; inc++)
		{
			psDroid->asWeaps[inc].lastFired=0;
			psDroid->asWeaps[inc].nStat = pTemplate->asWeaps[inc];
			psDroid->asWeaps[inc].recoilValue = 0;
			psDroid->asWeaps[inc].ammo = (asWeaponStats + psDroid->
				asWeaps[inc].nStat)->numRounds;
		}
	}
	else
	{
		// no weapon (could be a construction droid for example)
		// this is also used to check if a droid has a weapon, so zero it
		psDroid->asWeaps[0].nStat = 0;
	}
	//allocate the components hit points
	psDroid->asBits[COMP_BODY].nStat = (UBYTE)pTemplate->asParts[COMP_BODY];

	// ajl - changed this to init brains for all droids (crashed)
	psDroid->asBits[COMP_BRAIN].nStat = 0;

	// This is done by the Command droid stuff - John.
	// Not any more - John.
	psDroid->asBits[COMP_BRAIN].nStat = pTemplate->asParts[COMP_BRAIN];
	psDroid->asBits[COMP_PROPULSION].nStat = pTemplate->asParts[COMP_PROPULSION];
	psDroid->asBits[COMP_SENSOR].nStat = pTemplate->asParts[COMP_SENSOR];
	psDroid->asBits[COMP_ECM].nStat = pTemplate->asParts[COMP_ECM];
	psDroid->asBits[COMP_REPAIRUNIT].nStat = pTemplate->asParts[COMP_REPAIRUNIT];
	psDroid->asBits[COMP_CONSTRUCT].nStat = pTemplate->asParts[COMP_CONSTRUCT];
}


// Sets the parts array in a template given a droid.
static void templateSetParts(const DROID *psDroid, DROID_TEMPLATE *psTemplate)
{
	UDWORD inc;
	psTemplate->numWeaps = 0;

	psTemplate->droidType = psDroid->droidType;

    //Watermelon:can only have DROID_MAXWEAPS weapon now
	for (inc = 0; inc < DROID_MAXWEAPS; inc++)
	{
		//Watermelon:this should fix the NULL weapon stats for empty weaponslots
		psTemplate->asWeaps[inc] = 0;
		if (psDroid->asWeaps[inc].nStat > 0)
		{
			psTemplate->numWeaps += 1;
			psTemplate->asWeaps[inc] = psDroid->asWeaps[inc].nStat;
		}
	}

	psTemplate->asParts[COMP_BODY] = psDroid->asBits[COMP_BODY].nStat;

	psTemplate->asParts[COMP_BRAIN] = psDroid->asBits[COMP_BRAIN].nStat;

	psTemplate->asParts[COMP_PROPULSION] = psDroid->asBits[COMP_PROPULSION].nStat;

	psTemplate->asParts[COMP_SENSOR] = psDroid->asBits[COMP_SENSOR].nStat;

	psTemplate->asParts[COMP_ECM] = psDroid->asBits[COMP_ECM].nStat;

	psTemplate->asParts[COMP_REPAIRUNIT] = psDroid->asBits[COMP_REPAIRUNIT].nStat;

	psTemplate->asParts[COMP_CONSTRUCT] = psDroid->asBits[COMP_CONSTRUCT].nStat;
}


/*
fills the list with Templates that can be manufactured
in the Factory - based on size. There is a limit on how many can be manufactured
at any one time. Pass back the number available.
*/
UDWORD fillTemplateList(DROID_TEMPLATE **ppList, STRUCTURE *psFactory, UDWORD limit)
{
	DROID_TEMPLATE	*psCurr;
	UDWORD			count = 0;
	UDWORD			iCapacity = psFactory->pFunctionality->factory.capacity;

	/* Add the templates to the list*/
	for (psCurr = apsDroidTemplates[psFactory->player]; psCurr != NULL;
		psCurr = psCurr->psNext)
	{
		//must add Command Droid if currently in production
		if (!getProductionQuantity(psFactory, psCurr))
		{
			//can only have (MAX_CMDDROIDS) in the world at any one time
			if (psCurr->droidType == DROID_COMMAND)
			{
				if (checkProductionForCommand(psFactory->player) +
					checkCommandExist(psFactory->player) >= (MAX_CMDDROIDS))
				{
					continue;
				}
			}
		}

		if (!validTemplateForFactory(psCurr, psFactory))
		{
			continue;
		}

		//check the factory can cope with this sized body
		if (!((asBodyStats + psCurr->asParts[COMP_BODY])->size > iCapacity) )
		{
            //cyborg templates are available when the body has been research
            //-same for Transporter in multiPlayer
			if ( psCurr->droidType == DROID_CYBORG ||
				 psCurr->droidType == DROID_CYBORG_SUPER ||
                 psCurr->droidType == DROID_CYBORG_CONSTRUCT ||
                 psCurr->droidType == DROID_CYBORG_REPAIR ||
                 psCurr->droidType == DROID_TRANSPORTER)
			{
				if ( apCompLists[psFactory->player][COMP_BODY]
					 [psCurr->asParts[COMP_BODY]] != AVAILABLE )
				{
					//ignore if not research yet
					continue;
				}
			}
			*ppList++ = psCurr;
			count++;
			//once reached the limit, stop adding any more to the list
			if (count == limit)
			{
				return count;
			}
		}
	}
	return count;
}

/* Make all the droids for a certain player a member of a specific group */
void assignDroidsToGroup(UDWORD	playerNumber, UDWORD groupNumber)
{
DROID	*psDroid;
BOOL	bAtLeastOne = false;
FLAG_POSITION	*psFlagPos;

	if(groupNumber<UBYTE_MAX)
	{
		/* Run through all the droids */
		for(psDroid = apsDroidLists[playerNumber]; psDroid!=NULL; psDroid = psDroid->psNext)
		{
			/* Clear out the old ones */
			if(psDroid->group == groupNumber)
			{
				psDroid->group = UBYTE_MAX;
			}

			/* Only assign the currently selected ones */
			if(psDroid->selected)
			{
				/* Set them to the right group - they can only be a member of one group */
				psDroid->group = (UBYTE)groupNumber;
				bAtLeastOne = true;
			}
		}
	}
	if(bAtLeastOne)
	{
		setSelectedGroup(groupNumber);
		//clear the Deliv Point if one
		for (psFlagPos = apsFlagPosLists[selectedPlayer]; psFlagPos;
		psFlagPos = psFlagPos->psNext)
		{
			psFlagPos->selected = false;
		}
		groupConsoleInformOfCreation(groupNumber);
		secondarySetAverageGroupState(selectedPlayer, groupNumber);
	}
}


BOOL activateGroupAndMove(UDWORD playerNumber, UDWORD groupNumber)
{
	DROID	*psDroid,*psCentreDroid = NULL;
	BOOL selected = false;
	FLAG_POSITION	*psFlagPos;

	if (groupNumber < UBYTE_MAX)
	{
		for(psDroid = apsDroidLists[playerNumber]; psDroid!=NULL; psDroid = psDroid->psNext)
		{
			/* Wipe out the ones in the wrong group */
			if (psDroid->selected && psDroid->group != groupNumber)
			{
				DeSelectDroid(psDroid);
			}
			/* Get the right ones */
			if(psDroid->group == groupNumber)
			{
				SelectDroid(psDroid);
				psCentreDroid = psDroid;
			}
		}

		/* There was at least one in the group */
		if (psCentreDroid)
		{
			//clear the Deliv Point if one
			for (psFlagPos = apsFlagPosLists[selectedPlayer]; psFlagPos;
			psFlagPos = psFlagPos->psNext)
			{
				psFlagPos->selected = false;
			}

			selected = true;
			if (!driveModeActive())
			{
				if (getWarCamStatus())
				{
					camToggleStatus();			 // messy - fix this
					processWarCam(); //odd, but necessary
					camToggleStatus();				// messy - FIXME
				}
				else
				{
					/* Centre display on him if warcam isn't active */
					setViewPos(map_coord(psCentreDroid->pos.x), map_coord(psCentreDroid->pos.y), true);
				}
			}
		}
	}

	if(selected)
	{
		setSelectedGroup(groupNumber);
		groupConsoleInformOfCentering(groupNumber);
	}
	else
	{
		setSelectedGroup(UBYTE_MAX);
	}

	return selected;
}

BOOL activateGroup(UDWORD playerNumber, UDWORD groupNumber)
{
	DROID	*psDroid;
	BOOL selected = false;
	FLAG_POSITION	*psFlagPos;

	if (groupNumber < UBYTE_MAX)
	{
		for (psDroid = apsDroidLists[playerNumber]; psDroid; psDroid = psDroid->psNext)
		{
			/* Wipe out the ones in the wrong group */
			if (psDroid->selected && psDroid->group != groupNumber)
			{
				DeSelectDroid(psDroid);
			}
			/* Get the right ones */
			if (psDroid->group == groupNumber)
			{
				SelectDroid(psDroid);
				selected = true;
			}
		}
	}

	if (selected)
	{
		setSelectedGroup(groupNumber);
		//clear the Deliv Point if one
		for (psFlagPos = apsFlagPosLists[selectedPlayer]; psFlagPos;
		psFlagPos = psFlagPos->psNext)
		{
			psFlagPos->selected = false;
		}
		groupConsoleInformOfSelection(groupNumber);
	}
	else
	{
		setSelectedGroup(UBYTE_MAX);
	}
	return selected;
}

void	groupConsoleInformOfSelection( UDWORD groupNumber )
{
	// ffs am
	char groupInfo[255];
	unsigned int num_selected = selNumSelected(selectedPlayer);

	ssprintf(groupInfo, ngettext("Group %u selected - %u Unit", "Group %u selected - %u Units", num_selected), groupNumber, num_selected);
	addConsoleMessage(groupInfo,RIGHT_JUSTIFY,SYSTEM_MESSAGE);

}

void	groupConsoleInformOfCreation( UDWORD groupNumber )
{
	char groupInfo[255];

	if (!getWarCamStatus())
	{
		unsigned int num_selected = selNumSelected(selectedPlayer);

		ssprintf(groupInfo, ngettext("%u unit assigned to Group %u", "%u units assigned to Group %u", num_selected), num_selected, groupNumber);
		addConsoleMessage(groupInfo,RIGHT_JUSTIFY,SYSTEM_MESSAGE);
	}

}

void	groupConsoleInformOfCentering( UDWORD groupNumber )
{
	char	groupInfo[255];
	unsigned int num_selected = selNumSelected(selectedPlayer);

	if(!getWarCamStatus())
	{
		ssprintf(groupInfo, ngettext("Centered on Group %u - %u Unit", "Centered on Group %u - %u Units", num_selected), groupNumber, num_selected);
	}
	else
	{
		ssprintf(groupInfo, ngettext("Aligning with Group %u - %u Unit", "Aligning with Group %u - %u Units", num_selected), groupNumber, num_selected);
	}

	addConsoleMessage(groupInfo,RIGHT_JUSTIFY,SYSTEM_MESSAGE);
}



UDWORD	getSelectedGroup( void )
{
	return(selectedGroup);
}

void	setSelectedGroup(UDWORD groupNumber)
{
	selectedGroup = groupNumber;
	selectedCommander = UBYTE_MAX;
}

UDWORD	getSelectedCommander( void )
{
	return(selectedCommander);
}

void	setSelectedCommander(UDWORD commander)
{
	selectedGroup = UBYTE_MAX;
	selectedCommander = commander;
}

/**
 * calculate muzzle tip location in 3d world
 */
BOOL calcDroidMuzzleLocation(DROID *psDroid, Vector3f *muzzle, int weapon_slot)
{
	iIMDShape *psShape, *psWeaponImd;

	CHECK_DROID(psDroid);

	psShape = BODY_IMD(psDroid, psDroid->player);
	psWeaponImd = (asWeaponStats[psDroid->asWeaps[weapon_slot].nStat]).pIMD;

	if(psShape && psShape->nconnectors)
	{
		Vector3f barrel = {0.0f, 0.0f, 0.0f};

		pie_MatBegin();

		pie_TRANSLATE(psDroid->pos.x, -psDroid->pos.z, psDroid->pos.y);

		//matrix = the center of droid
		pie_MatRotY( DEG( psDroid->direction ) );
		pie_MatRotX( DEG( psDroid->pitch ) );
		pie_MatRotZ( DEG( -psDroid->roll ) );
		pie_TRANSLATE( psShape->connectors[weapon_slot].x, -psShape->connectors[weapon_slot].z,
					  -psShape->connectors[weapon_slot].y);//note y and z flipped

		//matrix = the gun and turret mount on the body
		pie_MatRotY(DEG(psDroid->asWeaps[weapon_slot].rotation));	// +ve anticlockwise
		pie_MatRotX(DEG(psDroid->asWeaps[weapon_slot].pitch));		// +ve up
		pie_MatRotZ(DEG(0));

		//matrix = the muzzle mount on turret
		if( psWeaponImd && psWeaponImd->nconnectors )
		{
			barrel = Vector3f_Init(psWeaponImd->connectors->x, -psWeaponImd->connectors->y, -psWeaponImd->connectors->z);
		}

		pie_RotateTranslate3f(&barrel, muzzle);
		muzzle->z = -muzzle->z;

		pie_MatEnd();
	}
	else
	{
		*muzzle = Vector3f_Init(psDroid->pos.x, psDroid->pos.y, psDroid->pos.z + psDroid->sDisplay.imd->max.y);
	}

	CHECK_DROID(psDroid);

	return true;
}


/*!
 * Gets a template from its name
 * relies on the name being unique (or it will return the first one it finds!)
 * \param pName Template name
 * \param player Player number
 * \pre pName has to be the unique, untranslated name!
 * \pre player \< MAX_PLAYERS
 */
DROID_TEMPLATE * getTemplateFromUniqueName(const char *pName, unsigned int player)
{
	DROID_TEMPLATE *psCurr;

	for (psCurr = apsDroidTemplates[player]; psCurr != NULL; psCurr = psCurr->psNext)
	{
		if (strcmp(psCurr->pName, pName) == 0)
		{
			return psCurr;
		}
	}

	return NULL;
}


/*!
 * Gets a template from its name
 * relies on the name being unique (or it will return the first one it finds!)
 * \param pName Template name
 * \pre pName has to be the unique, untranslated name!
 * \post pName will be translated via getDroidResourceName()!
 * \warning IF YOU USE THIS FUNCTION - NOTE THAT selectedPlayer's TEMPLATES ARE NOT USED!!!!
 */
DROID_TEMPLATE * getTemplateFromTranslatedNameNoPlayer(char *pName)
{
	unsigned int player;

	/*all droid and template names are now stored as the translated
	name regardless of RESOURCE_NAMES and STORE_RESOURCE_ID! - AB 26/06/98*/
	getDroidResourceName(pName);

	for (player = 0; player < MAX_PLAYERS; player++)
	{
		DROID_TEMPLATE *psCurr;
		// OK so we want selectedPlayer's CYBORG templates since they cannot be edited
		// and we don't want to duplicate them for the sake of it! (ha!)
		// don't use selectedPlayer's templates if not multiplayer
		// this was written for use in the scripts and we don't want the scripts to use
		// selectedPlayer's templates because we cannot guarentee they will exist!
		for (psCurr = apsDroidTemplates[player]; psCurr != NULL; psCurr = psCurr->psNext)
		{
			if (strcmp(psCurr->aName, pName) == 0)
			{
				//if template is selectedPlayers' it must be a CYBORG or we ignore it
				if (!bMultiPlayer
				 && player == selectedPlayer
				 && !(psCurr->droidType == DROID_CYBORG
				   || psCurr->droidType == DROID_CYBORG_SUPER
				   || psCurr->droidType == DROID_CYBORG_CONSTRUCT
				   || psCurr->droidType == DROID_CYBORG_REPAIR))
				{
					//ignore
					continue;
				}
				return psCurr;
			}
		}
	}
	return NULL;
}

/*getTemplatefFromSinglePlayerID gets template for unique ID  searching one players list */
DROID_TEMPLATE* getTemplateFromSinglePlayerID(UDWORD multiPlayerID, UDWORD player)
{
	DROID_TEMPLATE	*pDroidDesign;

	for(pDroidDesign = apsDroidTemplates[player]; pDroidDesign != NULL; pDroidDesign = pDroidDesign->psNext)
	{
		if (pDroidDesign->multiPlayerID == multiPlayerID)
		{
			return pDroidDesign;
		}
	}
	return NULL;
}

/*getTemplatefFromMultiPlayerID gets template for unique ID  searching all lists */
DROID_TEMPLATE* getTemplateFromMultiPlayerID(UDWORD multiPlayerID)
{
	UDWORD			player;
	DROID_TEMPLATE	*pDroidDesign;

	for (player=0; player < MAX_PLAYERS; player++)
	{
		pDroidDesign = getTemplateFromSinglePlayerID(multiPlayerID, player);
		if (pDroidDesign != NULL)
		{
			return pDroidDesign;
		}
	}
	return NULL;
}

// finds a droid for the player and sets it to be the current selected droid
BOOL selectDroidByID(UDWORD id, UDWORD player)
{
	DROID	*psCurr;

	//look through the list of droids for the player and find the matching id
	for (psCurr = apsDroidLists[player]; psCurr; psCurr = psCurr->psNext)
	{
		if (psCurr->id == id)
		{
			break;
		}
	}

	if (psCurr)
	{
		clearSelection();
		SelectDroid(psCurr);
		return true;
	}

	return false;
}

struct rankMap
{
	unsigned int kills;          // required minimum amount of kills to reach this rank
	unsigned int commanderKills; // required minimum amount of kills for a commander (or sensor) to reach this rank
	const char*  name;           // name of this rank
};

static const struct rankMap arrRank[] =
{
	{0,   0,    N_("Rookie")},
	{4,   16,   NP_("rank", "Green")},
	{8,   32,   N_("Trained")},
	{16,  64,   N_("Regular")},
	{32,  128,  N_("Professional")},
	{64,  256,  N_("Veteran")},
	{128, 512,  N_("Elite")},
	{256, 1024, N_("Special")},
	{512, 2048, N_("Hero")}
};

unsigned int getDroidLevel(const DROID* psDroid)
{
	bool isCommander = (psDroid->droidType == DROID_COMMAND ||
	                    psDroid->droidType == DROID_SENSOR) ? true : false;
	unsigned int numKills = psDroid->experience;
	unsigned int i;

	// Commanders don't need as much kills for ranks in multiplayer
	if (isCommander && cmdGetDroidMultiExpBoost())
	{
		numKills *= 2;
	}

	// Search through the array of ranks until one is found
	// which requires more kills than the droid has.
	// Then fall back to the previous rank.
	for (i = 1; i < ARRAY_SIZE(arrRank); ++i)
	{
		const unsigned int requiredKills = isCommander ? arrRank[i].commanderKills : arrRank[i].kills;

		if (numKills < requiredKills)
		{
			return i - 1;
		}
	}

	// If the criteria of the last rank are met, then select the last one
	return ARRAY_SIZE(arrRank) - 1;
}

UDWORD getDroidEffectiveLevel(DROID *psDroid)
{
	UDWORD level = getDroidLevel(psDroid);
	UDWORD cmdLevel = 0;

	// get commander level
	if(hasCommander(psDroid))
	{
		cmdLevel = cmdGetCommanderLevel(psDroid);

		// Commanders boost unit's effectiveness just by being assigned to it
		if(bMultiPlayer)
		{
			cmdLevel++;
		}
	}

	return MAX(level, cmdLevel);
}


const char *getDroidNameForRank(UDWORD rank)
{
	ASSERT( rank < (sizeof(arrRank) / sizeof(struct rankMap)),
	        "getDroidNameForRank: given rank number (%d) out of bounds, we only have %zu ranks\n", rank, (sizeof(arrRank) / sizeof(struct rankMap)) );

	return PE_("rank", arrRank[rank].name);
}

const char *getDroidLevelName(DROID *psDroid)
{
	return(getDroidNameForRank(getDroidLevel(psDroid)));
}

UDWORD	getNumDroidsForLevel(UDWORD	level)
{
	DROID	*psDroid;
	UDWORD	count;

	for(psDroid = apsDroidLists[selectedPlayer],count = 0;
		psDroid; psDroid = psDroid->psNext)
	{
		if (getDroidLevel(psDroid) == level)
		{
			count++;
		}
	}

	return count;
}

// Get the name of a droid from it's DROID structure.
//
char *droidGetName(DROID *psDroid)
{
	return psDroid->aName;
}

//
// Set the name of a droid in it's DROID structure.
//
// - only possible on the PC where you can adjust the names,
//
void droidSetName(DROID *psDroid,const char *pName)
{
	sstrcpy(psDroid->aName, pName);
}



// ////////////////////////////////////////////////////////////////////////////
// returns true when no droid on x,y square.
BOOL noDroid(UDWORD x, UDWORD y)
{
	unsigned int i;

	// check each droid list
	for (i = 0; i < MAX_PLAYERS; ++i)
	{
		const DROID* psDroid;
		for (psDroid = apsDroidLists[i]; psDroid; psDroid = psDroid->psNext)
		{
			if (map_coord(psDroid->pos.x) == x
			 && map_coord(psDroid->pos.y) == y)
			{
					return false;
			}
		}
	}
	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// returns true when one droid on x,y square.
static BOOL oneDroid(UDWORD x, UDWORD y)
{
	UDWORD i;
	BOOL bFound = false;
	DROID *pD;
	// check each droid list
	for(i=0;i<MAX_PLAYERS;i++)
	{
		for(pD = apsDroidLists[i]; pD ; pD= pD->psNext)
		{
			if (map_coord(pD->pos.x) == x
			 && map_coord(pD->pos.y) == y)
			{
				if (bFound)
				{
					return false;
				}

				bFound = true;//first droid on this square so continue
			}
		}
	}
	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// returns true if it's a sensible place to put that droid.
static BOOL sensiblePlace(SDWORD x, SDWORD y, PROPULSION_TYPE propulsion)
{
	UDWORD count=0;

	// not too near the edges.
	if((x < TOO_NEAR_EDGE) || (x > (SDWORD)(mapWidth - TOO_NEAR_EDGE)))
		return false;
	if((y < TOO_NEAR_EDGE) || (y > (SDWORD)(mapHeight - TOO_NEAR_EDGE)))
		return false;

	// check no features there
	if(TileHasFeature(mapTile(x,y)))
	{
		return false;
	}

	// not on a blocking tile.
	if (fpathBlockingTile(x, y, propulsion))
	{
		return false;
	}

	// shouldn't next to more than one blocking tile, to avoid windy paths.
	if (fpathBlockingTile(x - 1, y - 1, propulsion))
		count++;
	if (fpathBlockingTile(x, y - 1, propulsion))
		count++;
	if (fpathBlockingTile(x + 1, y - 1, propulsion))
		count++;
	if (fpathBlockingTile(x - 1, y, propulsion))
		count++;
	if (fpathBlockingTile(x + 1, y, propulsion))
		count++;
	if (fpathBlockingTile(x -1, y + 1, propulsion))
		count++;
	if (fpathBlockingTile(x, y + 1, propulsion))
		count++;
	if (fpathBlockingTile(x +1, y + 1, propulsion))
		count++;

	if(count > 1)
		return false;

	return true;
}

// ------------------------------------------------------------------------------------
// Should stop things being placed in inaccessible areas? Assume wheeled propulsion.
BOOL	zonedPAT(UDWORD x, UDWORD y)
{
	return sensiblePlace(x, y, PROPULSION_TYPE_WHEELED) && noDroid(x,y);
}

static BOOL canFitDroid(UDWORD x, UDWORD y)
{
	return sensiblePlace(x, y, PROPULSION_TYPE_WHEELED) && (noDroid(x,y) || oneDroid(x, y));
}

/// find a tile for which the function will return true
BOOL	pickATileGen(UDWORD *x, UDWORD *y, UBYTE numIterations,
					 BOOL (*function)(UDWORD x, UDWORD y))
{
	return pickATileGenThreat(x, y, numIterations, -1, -1, function);
}

/// find a tile for which the passed function will return true without any threat in the specified range 
BOOL	pickATileGenThreat(UDWORD *x, UDWORD *y, UBYTE numIterations, SDWORD threatRange,
					 SDWORD player, BOOL (*function)(UDWORD x, UDWORD y))
{
SDWORD	i,j;
SDWORD	startX,endX,startY,endY;
UDWORD	passes;


	ASSERT( *x<mapWidth,"x coordinate is off-map for pickATileGen" );
	ASSERT( *y<mapHeight,"y coordinate is off-map for pickATileGen" );

	if(function(*x,*y) && ((threatRange <=0) || (!ThreatInRange(player, threatRange, *x, *y, false))))	//TODO: vtol check really not needed?
	{
		return(true);
	}

	/* Initial box dimensions and set iteration count to zero */
	startX = endX = *x;	startY = endY = *y;	passes = 0;

	/* Keep going until we get a tile or we exceed distance */
	while(passes<numIterations)
	{
		/* Process whole box */
		for(i = startX; i <= endX; i++)
		{
			for(j = startY; j<= endY; j++)
			{
				/* Test only perimeter as internal tested previous iteration */
				if(i==startX || i==endX || j==startY || j==endY)
				{
					/* Good enough? */
					if(function(i,j) && ((threatRange <=0) || (!ThreatInRange(player, threatRange, world_coord(i), world_coord(j), false))))		//TODO: vtols check really not needed?
					{
						/* Set exit conditions and get out NOW */
						*x = i;	*y = j;
						return true;
					}
				}
			}
		}
		/* Expand the box out in all directions - off map handled by tileAcceptable */
		startX--; startY--;	endX++;	endY++;	passes++;
	}
	/* If we got this far, then we failed - passed in values will be unchanged */
	return false;

}

/// find an empty tile accessible to a wheeled droid 
BOOL	pickATile(UDWORD *x, UDWORD *y, UBYTE numIterations)
{
	return pickATileGen(x, y, numIterations, zonedPAT);
}

/// find a tile for a wheeled droid with only one other droid present
PICKTILE pickHalfATile(UDWORD *x, UDWORD *y, UBYTE numIterations)
{
	return pickATileGen(x, y, numIterations, canFitDroid);
}

/* Looks through the players list of droids to see if any of them are
building the specified structure - returns true if finds one*/
BOOL checkDroidsBuilding(STRUCTURE *psStructure)
{
	DROID				*psDroid;

	for (psDroid = apsDroidLists[psStructure->player]; psDroid != NULL; psDroid =
		psDroid->psNext)
	{
		//check DORDER_BUILD, HELP_BUILD is handled the same
		BASE_OBJECT * const psStruct = orderStateObj(psDroid, DORDER_BUILD);
		if ((STRUCTURE*)psStruct == psStructure)
		{
			return true;
		}
	}
	return false;
}

/* Looks through the players list of droids to see if any of them are
demolishing the specified structure - returns true if finds one*/
BOOL checkDroidsDemolishing(STRUCTURE *psStructure)
{
	DROID				*psDroid;

	for (psDroid = apsDroidLists[psStructure->player]; psDroid != NULL; psDroid =
		psDroid->psNext)
	{
		//check DORDER_DEMOLISH
		BASE_OBJECT * const psStruct = orderStateObj(psDroid, DORDER_DEMOLISH);
		if ((STRUCTURE*)psStruct == psStructure)
		{
			return true;
		}
	}
	return false;
}


/* checks the structure for type and capacity and **NOT orders the droid*** to build
a module if it can - returns true if order is set */
BOOL buildModule(STRUCTURE *psStruct)
{
	BOOL	order = false;
	UDWORD	i = 0;

	ASSERT(psStruct != NULL && psStruct->pStructureType != NULL, "Invalid structure pointer");
	if (!psStruct || !psStruct->pStructureType)
	{
		return false;
	}

	switch (psStruct->pStructureType->type)
	{
		case REF_POWER_GEN:
			//check room for one more!
			ASSERT(psStruct->pFunctionality, "buildModule: Functionality missing for power!");
			if (psStruct->pFunctionality->powerGenerator.capacity < NUM_POWER_MODULES)
			{
				i = powerModuleStat;
				order = true;
			}
			break;
		case REF_FACTORY:
		case REF_VTOL_FACTORY:
			//check room for one more!
			ASSERT(psStruct->pFunctionality, "buildModule: Functionality missing for factory!");
			if (psStruct->pFunctionality->factory.capacity < NUM_FACTORY_MODULES)
			{
				i = factoryModuleStat;
				order = true;
			}
			break;
		case REF_RESEARCH:
			//check room for one more!
			ASSERT(psStruct->pFunctionality, "buildModule: Functionality missing for research!");
			if (psStruct->pFunctionality->researchFacility.capacity < NUM_RESEARCH_MODULES)
			{
				i = researchModuleStat;
				order = true;
			}
			break;
		default:
			//no other structures can have modules attached
			break;
	}

	if (order)
	{
		// Check availability of Module
		if (!((i < numStructureStats) &&
			(apStructTypeLists[psStruct->player][i] == AVAILABLE)))
		{
			order = false;
		}
	}

	return order;
}

/*Deals with building a module - checking if any droid is currently doing this
 - if so, helping to build the current one*/
void setUpBuildModule(DROID *psDroid)
{
	UDWORD		tileX, tileY;
	STRUCTURE	*psStruct;

	tileX = map_coord(psDroid->orderX);
	tileY = map_coord(psDroid->orderY);

	//check not another Truck started
	psStruct = getTileStructure(tileX,tileY);
	if (psStruct)
	{
		if (checkDroidsBuilding(psStruct))
		{
			//set up the help build scenario
			psDroid->order = DORDER_HELPBUILD;
			setDroidTarget(psDroid, (BASE_OBJECT *)psStruct);
			if (droidStartBuild(psDroid))
			{
				psDroid->action = DACTION_BUILD;
				intBuildStarted(psDroid);
			}
			else
			{
				psDroid->action = DACTION_NONE;
			}
		}
		else
		{
			if(buildModule(psStruct))
			{
				//no other droids building so just start it off
				if (droidStartBuild(psDroid))
				{
					psDroid->action = DACTION_BUILD;
					intBuildStarted(psDroid);
				}
				else
				{
					psDroid->action = DACTION_NONE;
				}
			}
			else
			{
				psDroid->action = DACTION_NONE;
			}
		}
	}
	else
	{
		//we've got a problem if it didn't find a structure
		psDroid->action = DACTION_NONE;
	}
}

const char *getDroidName(const DROID *psDroid)
{
	DROID_TEMPLATE sTemplate;

	templateSetParts(psDroid, &sTemplate);

	return getTemplateName(&sTemplate);
}

const char* getTemplateName(const DROID_TEMPLATE *psTemplate)
{
	return psTemplate->aName;
}

/* Just returns true if the droid's present body points aren't as high as the original*/
BOOL	droidIsDamaged(DROID *psDroid)
{
	if(psDroid->body < psDroid->originalBody)
	{
		return(true);
	}
	else
	{
		return(false);
	}
}


BOOL getDroidResourceName(char *pName)
{
	/* See if the name has a string resource associated with it by trying
	 * to get the string resource.
	 */
	const char * const name = strresGetString(psStringRes, pName);

	if (!name)
	{
		debug(LOG_ERROR, "Unable to find string resource for string with ID \"%s\"", pName);
		abort();
		return false;
	}

	// Copy the retrieved string into the output parameter
	strcpy(pName, name);

	return true;
}


/*checks to see if an electronic warfare weapon is attached to the droid*/
BOOL electronicDroid(DROID *psDroid)
{
	DROID	*psCurr;

	CHECK_DROID(psDroid);

	//Watermelon:use slot 0 for now
	//if (psDroid->numWeaps && asWeaponStats[psDroid->asWeaps[0].nStat].
    if (psDroid->numWeaps > 0 && asWeaponStats[psDroid->asWeaps[0].nStat].
		weaponSubClass == WSC_ELECTRONIC)
	{
		return true;
	}

	if (psDroid->droidType == DROID_COMMAND && psDroid->psGroup)
	{
		// if a commander has EW units attached it is electronic
		for (psCurr = psDroid->psGroup->psList; psCurr; psCurr = psCurr->psGrpNext)
		{
			if (electronicDroid(psCurr))
			{
				return true;
			}
		}
	}

	return false;
}

/*checks to see if the droid is currently being repaired by another*/
BOOL droidUnderRepair(DROID *psDroid)
{
	DROID		*psCurr;

	CHECK_DROID(psDroid);

	//droid must be damaged
	if (droidIsDamaged(psDroid))
	{
		//look thru the list of players droids to see if any are repairing this droid
		for (psCurr = apsDroidLists[psDroid->player]; psCurr != NULL; psCurr = psCurr->psNext)
		{
			//if (psCurr->droidType == DROID_REPAIR && psCurr->action ==
            if ((psCurr->droidType == DROID_REPAIR || psCurr->droidType ==
                DROID_CYBORG_REPAIR) && psCurr->action ==
				DACTION_DROIDREPAIR && psCurr->psTarget == (BASE_OBJECT *)psDroid)
			{
				return true;
			}
		}
	}
	return false;
}

//count how many Command Droids exist in the world at any one moment
UBYTE checkCommandExist(UBYTE player)
{
	DROID	*psDroid;
	UBYTE	quantity = 0;

	for (psDroid = apsDroidLists[player]; psDroid != NULL; psDroid = psDroid->psNext)
	{
		if (psDroid->droidType == DROID_COMMAND)
		{
			quantity++;
		}
	}
	return quantity;
}

//access functions for vtols
BOOL isVtolDroid(const DROID* psDroid)
{
	return asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == PROPULSION_TYPE_LIFT
	    && psDroid->droidType != DROID_TRANSPORTER;
}

/*returns true if a VTOL Weapon Droid which has completed all runs*/
BOOL vtolEmpty(DROID *psDroid)
{
	UBYTE	i;
	UBYTE	numVtolWeaps = 0;
	UBYTE	emptyWeaps = 0;
	BOOL	bEmpty = true;

	CHECK_DROID(psDroid);

	if (!isVtolDroid(psDroid))
	{
		return false;
	}
	if (psDroid->droidType != DROID_WEAPON)
	{
		return false;
	}

	if (psDroid->numWeaps > 0)
	{
		for (i = 0;i < psDroid->numWeaps;i++)
		{
			if (asWeaponStats[psDroid->asWeaps[i].nStat].vtolAttackRuns > 0)
			{
				numVtolWeaps += (1 << (1 + i));
				if (psDroid->sMove.iAttackRuns[i] >= getNumAttackRuns(psDroid, i))
				{
					emptyWeaps += (1 << (1 + i));
				}
			}
		}
	}

	for (i = 0;i < psDroid->numWeaps;i++)
	{
		if ((numVtolWeaps & (1 << (1 + i))) && !(emptyWeaps & (1 << (1 + i))))
		{
			bEmpty = false;
			break;
		}
	}
	return bEmpty;
}

// true if a vtol is waiting to be rearmed by a particular rearm pad
BOOL vtolReadyToRearm(DROID *psDroid, STRUCTURE *psStruct)
{
	BASE_OBJECT *psRearmPad;

	CHECK_DROID(psDroid);

	if (!isVtolDroid(psDroid)
	 || psDroid->action != DACTION_WAITFORREARM)
	{
		return false;
	}

	// If a unit has been ordered to rearm make sure it goes to the correct base
	psRearmPad = orderStateObj(psDroid, DORDER_REARM);
	if (psRearmPad
	 && (STRUCTURE*)psRearmPad != psStruct
	 && !vtolOnRearmPad((STRUCTURE*)psRearmPad, psDroid))
	{
		// target rearm pad is clear - let it go there
		return false;
	}

	if (vtolHappy(psDroid) &&
		vtolOnRearmPad(psStruct, psDroid))
	{
		// there is a vtol on the pad and this vtol is already rearmed
		// don't bother shifting the other vtol off
		return false;
	}

	if ((psDroid->psActionTarget[0] != NULL) &&
		(psDroid->psActionTarget[0]->cluster != psStruct->cluster))
	{
		// vtol is rearming at a different base
		return false;
	}

	return true;
}

// true if a vtol droid currently returning to be rearmed
BOOL vtolRearming(DROID *psDroid)
{
	CHECK_DROID(psDroid);

	if (!isVtolDroid(psDroid))
	{
		return false;
	}
	if (psDroid->droidType != DROID_WEAPON)
	{
		return false;
	}

	if (psDroid->action == DACTION_MOVETOREARM ||
		psDroid->action == DACTION_WAITFORREARM ||
        psDroid->action == DACTION_MOVETOREARMPOINT ||
		psDroid->action == DACTION_WAITDURINGREARM)
	{
		return true;
	}

	return false;
}

// true if a droid is currently attacking
BOOL droidAttacking(DROID *psDroid)
{
	CHECK_DROID(psDroid);

    //what about cyborgs?
    if (!(psDroid->droidType == DROID_WEAPON || psDroid->droidType == DROID_CYBORG ||
        psDroid->droidType == DROID_CYBORG_SUPER))
	{
		return false;
	}

	if (psDroid->action == DACTION_ATTACK ||
		psDroid->action == DACTION_MOVETOATTACK ||
		psDroid->action == DACTION_ROTATETOATTACK ||
		psDroid->action == DACTION_VTOLATTACK ||
		psDroid->action == DACTION_MOVEFIRE)
	{
		return true;
	}

	return false;
}

// see if there are any other vtols attacking the same target
// but still rearming
BOOL allVtolsRearmed(DROID *psDroid)
{
	DROID	*psCurr;
	BOOL	stillRearming;

	CHECK_DROID(psDroid);

	// ignore all non vtols
	if (!isVtolDroid(psDroid))
	{
		return true;
	}

	stillRearming = false;
	for (psCurr=apsDroidLists[psDroid->player]; psCurr; psCurr=psCurr->psNext)
	{
		if (vtolRearming(psCurr) &&
			psCurr->order == psDroid->order &&
			psCurr->psTarget == psDroid->psTarget)
		{
			stillRearming = true;
			break;
		}
	}

	return !stillRearming;
}


/*returns a count of the base number of attack runs for the weapon attached to the droid*/
//Watermelon:adds int weapon_slot
UWORD   getNumAttackRuns(DROID *psDroid, int weapon_slot)
{
    UWORD   numAttackRuns;

    ASSERT(isVtolDroid(psDroid), "not a VTOL Droid");

    /*if weapon attached to the droid is a salvo weapon, then number of shots that
    can be fired = vtolAttackRuns*numRounds */
    if (asWeaponStats[psDroid->asWeaps[weapon_slot].nStat].reloadTime)
    {
        numAttackRuns = (UWORD)(asWeaponStats[psDroid->asWeaps[weapon_slot].nStat].numRounds *
            asWeaponStats[psDroid->asWeaps[weapon_slot].nStat].vtolAttackRuns);
    }
    else
    {
        numAttackRuns = asWeaponStats[psDroid->asWeaps[weapon_slot].nStat].vtolAttackRuns;
    }

    return numAttackRuns;
}

/*Checks a vtol for being fully armed and fully repaired to see if ready to
leave reArm pad */
BOOL vtolHappy(const DROID* psDroid)
{
	unsigned int i;

	CHECK_DROID(psDroid);

	ASSERT(isVtolDroid(psDroid), "not a VTOL droid");

	if (psDroid->body < psDroid->originalBody)
	{
		// VTOLs with less health than their original aren't happy
		return false;
	}

	if (psDroid->droidType != DROID_WEAPON)
	{
		// Not an armed droid, so don't check the (non-existent) weapons
		return true;
	}

	/* NOTE: Previous code (r5410) returned false if a droid had no weapon,
	 *       which IMO isn't correct, but might be expected behaviour. I'm
	 *       also not sure if weapon droids (see the above droidType check)
	 *       can even have zero weapons. -- Giel
	 */
	ASSERT(psDroid->numWeaps > 0, "VTOL weapon droid without weapons found!");

	//check full complement of ammo
	for (i = 0; i < psDroid->numWeaps; ++i)
	{
		if (asWeaponStats[psDroid->asWeaps[i].nStat].vtolAttackRuns > 0
		 && psDroid->sMove.iAttackRuns[i] != 0)
		{
			return false;
		}
	}

	return true;
}

/*checks if the droid is a VTOL droid and updates the attack runs as required*/
void updateVtolAttackRun(DROID *psDroid , int weapon_slot)
{
    if (isVtolDroid(psDroid))
    {
		if (psDroid->numWeaps > 0)
		{
			if (asWeaponStats[psDroid->asWeaps[weapon_slot].nStat].vtolAttackRuns > 0)
			{
				psDroid->sMove.iAttackRuns[weapon_slot]++;
				//quick check doesn't go over limit
				ASSERT( psDroid->sMove.iAttackRuns[weapon_slot] < UWORD_MAX, "updateVtolAttackRun: too many attack runs" );
			}
		}
    }
}

/*this mends the VTOL when it has been returned to home base whilst on an
offworld mission*/
void mendVtol(DROID *psDroid)
{
	UBYTE	i;
	ASSERT( vtolEmpty(psDroid), "mendVtol: droid is not an empty weapon VTOL!" );

	CHECK_DROID(psDroid);

	/* set rearm value to no runs made */
	for (i = 0;i < psDroid->numWeaps;i++)
	{
		psDroid->sMove.iAttackRuns[i] = 0;
		//reset ammo and lastTimeFired
		psDroid->asWeaps[i].ammo = asWeaponStats[psDroid->
			asWeaps[i].nStat].numRounds;
		psDroid->asWeaps[i].lastFired = 0;
	}
	/* set droid points to max */
	psDroid->body = psDroid->originalBody;

	CHECK_DROID(psDroid);
}

//assign rearmPad to the VTOL
void assignVTOLPad(DROID *psNewDroid, STRUCTURE *psReArmPad)
{
    ASSERT(isVtolDroid(psNewDroid), "not a vtol droid");
    ASSERT( psReArmPad->type == OBJ_STRUCTURE &&
			psReArmPad->pStructureType->type == REF_REARM_PAD,
        "assignVTOLPad: not a ReArm Pad" );

	setDroidBase(psNewDroid, psReArmPad);
}

/*compares the droid sensor type with the droid weapon type to see if the
FIRE_SUPPORT order can be assigned*/
BOOL droidSensorDroidWeapon(BASE_OBJECT *psObj, DROID *psDroid)
{
	SENSOR_STATS	*psStats = NULL;

	CHECK_DROID(psDroid);

	if(!psObj || !psDroid)
	{
		return false;
	}

	//first check if the object is a droid or a structure
	if ( (psObj->type != OBJ_DROID) &&
		 (psObj->type != OBJ_STRUCTURE) )
	{
		return false;
	}
	//check same player
	if (psObj->player != psDroid->player)
	{
		return false;
	}
	//check obj is a sensor droid/structure
	switch (psObj->type)
	{
	case OBJ_DROID:
		if (((DROID *)psObj)->droidType != DROID_SENSOR &&
			((DROID *)psObj)->droidType != DROID_COMMAND)
		{
			return false;
		}
		psStats = asSensorStats + ((DROID *)psObj)->asBits[COMP_SENSOR].nStat;
		break;
	case OBJ_STRUCTURE:
		psStats = ((STRUCTURE *)psObj)->pStructureType->pSensor;
		if ((psStats == NULL) ||
			(psStats->location != LOC_TURRET))
		{
			return false;
		}
		break;
	default:
		break;
	}

	//check droid is a weapon droid - or Cyborg!!
    if (!(psDroid->droidType == DROID_WEAPON || psDroid->droidType ==
        DROID_CYBORG || psDroid->droidType == DROID_CYBORG_SUPER))
	{
		return false;
	}

	//finally check the right droid/sensor combination
	// check vtol droid with commander
	if ((isVtolDroid(psDroid) || !proj_Direct(asWeaponStats + psDroid->asWeaps[0].nStat)) &&
		((DROID *)psObj)->droidType == DROID_COMMAND)
	{
		return true;
	}

	//check vtol droid with vtol sensor
    if (isVtolDroid(psDroid) && psDroid->asWeaps[0].nStat > 0)
	{
		if (psStats->type == VTOL_INTERCEPT_SENSOR ||
			psStats->type == VTOL_CB_SENSOR ||
            psStats->type == SUPER_SENSOR)
		{
			return true;
		}
		return false;
	}

	//check indirect weapon droid with standard/cb sensor
    /*Super Sensor works as any type*/
	if (!proj_Direct(asWeaponStats + psDroid->asWeaps[0].nStat))
	{
		if (psStats->type == STANDARD_SENSOR ||
			psStats->type == INDIRECT_CB_SENSOR ||
            psStats->type == SUPER_SENSOR)
		{
			return true;
		}
		return false;
	}
	return false;
}

// return whether a droid has a CB sensor on it
BOOL cbSensorDroid(DROID *psDroid)
{
	if (psDroid->droidType != DROID_SENSOR)
	{
		return false;
	}

    /*Super Sensor works as any type*/
	if (asSensorStats[psDroid->asBits[COMP_SENSOR].nStat].type ==
		VTOL_CB_SENSOR ||
		asSensorStats[psDroid->asBits[COMP_SENSOR].nStat].type ==
		INDIRECT_CB_SENSOR ||
		asSensorStats[psDroid->asBits[COMP_SENSOR].nStat].type ==
		SUPER_SENSOR)
	{
		return true;
	}

	return false;
}

// ////////////////////////////////////////////////////////////////////////////
// give a droid from one player to another - used in Electronic Warfare and multiplayer
//returns the droid created - for single player
DROID * giftSingleDroid(DROID *psD, UDWORD to)
{
	DROID_TEMPLATE	sTemplate;
	UWORD		x, y, numKills, i;
	float		direction;
	DROID		*psNewDroid, *psCurr;
	STRUCTURE	*psStruct;
	UDWORD		body, armourK[NUM_HIT_SIDES], armourH[NUM_HIT_SIDES];
	HIT_SIDE	impact_side;

	CHECK_DROID(psD);

	if(psD->player == to)
	{
		return psD;
	}

	// FIXME: why completely separate code paths for multiplayer and single player?? - Per

	if (bMultiPlayer)
	{
		// reset order
		orderDroid(psD, DORDER_STOP);

		if (droidRemove(psD, apsDroidLists)) 		// remove droid from one list
		{
			if (!isHumanPlayer(psD->player))
			{
				droidSetName(psD, "Enemy Unit");
			}

			// if successfully removed the droid from the players list add it to new player's list
			psD->selected	= false;
			psD->player	= to;		// move droid

			addDroid(psD, apsDroidLists);	// add to other list.

			// the new player may have different default sensor/ecm/repair components
			if ((asSensorStats + psD->asBits[COMP_SENSOR].nStat)->location == LOC_DEFAULT)
			{
				if (psD->asBits[COMP_SENSOR].nStat != aDefaultSensor[psD->player])
				{
					psD->asBits[COMP_SENSOR].nStat = (UBYTE)aDefaultSensor[psD->player];
				}
			}
			if ((asECMStats + psD->asBits[COMP_ECM].nStat)->location == LOC_DEFAULT)
			{
				if (psD->asBits[COMP_ECM].nStat != aDefaultECM[psD->player])
				{
					psD->asBits[COMP_ECM].nStat = (UBYTE)aDefaultECM[psD->player];
				}
			}
			if ((asRepairStats + psD->asBits[COMP_REPAIRUNIT].nStat)->location == LOC_DEFAULT)
			{
				if (psD->asBits[COMP_REPAIRUNIT].nStat != aDefaultRepair[psD->player])
				{
					psD->asBits[COMP_REPAIRUNIT].nStat = (UBYTE)aDefaultRepair[psD->player];
				}
			}
	        }
		// add back into cluster system
		clustNewDroid(psD);

		// Update visibility
		visTilesUpdate((BASE_OBJECT*)psD, rayTerrainCallback);

		// add back into the grid system
		gridAddObject((BASE_OBJECT *)psD);

		// check through the 'to' players list of droids to see if any are targetting it
		for (psCurr = apsDroidLists[to]; psCurr != NULL; psCurr = psCurr->psNext)
		{
			if (psCurr->psTarget == (BASE_OBJECT *)psD || psCurr->psActionTarget[0] == (BASE_OBJECT *)psD)
			{
				orderDroid(psCurr, DORDER_STOP);
			}
			// check through order list
			for (i = 0; i < psCurr->listSize; i++)
			{
				if (psCurr->asOrderList[i].psOrderTarget == (BASE_OBJECT *)psD)
				{
					removeDroidOrderTarget(psCurr, i);
					// move the rest of the list down
					memmove(&psCurr->asOrderList[i], &psCurr->asOrderList[i] + 1, (psCurr->listSize - i) * sizeof(ORDER_LIST));
					// adjust list size
					psCurr->listSize -= 1;
					// initialise the empty last slot
					memset(psCurr->asOrderList + psCurr->listSize, 0, sizeof(ORDER_LIST));
				}
			}
		}
		// check through the 'to' players list of structures to see if any are targetting it
		for (psStruct = apsStructLists[to]; psStruct != NULL; psStruct = psStruct->psNext)
		{
			if (psStruct->psTarget[0] == (BASE_OBJECT *)psD)
			{
				psStruct->psTarget[0] = NULL;
			}
		}

		// skirmish callback!
		psScrCBDroidTaken = psD;
		eventFireCallbackTrigger((TRIGGER_TYPE)CALL_UNITTAKEOVER);
		psScrCBDroidTaken = NULL;

		return NULL;
	}
	else
	{
		// got to destroy the droid and build another since there are too many complications re order/action!

		// create a template based on the droid
		templateSetParts(psD, &sTemplate);

		// copy the name across
		sstrcpy(sTemplate.aName, psD->aName);

		x = psD->pos.x;
		y = psD->pos.y;
		body = psD->body;
		for (impact_side = 0;impact_side < NUM_HIT_SIDES;impact_side=impact_side+1)
		{
			armourK[impact_side] = psD->armour[impact_side][WC_KINETIC];
			armourH[impact_side] = psD->armour[impact_side][WC_HEAT];
		}
		numKills = psD->experience;
		direction = psD->direction;
		// only play the sound if unit being taken over is selectedPlayer's but not going to the selectedPlayer
		if (psD->player == selectedPlayer && to != selectedPlayer)
		{
			scoreUpdateVar(WD_UNITS_LOST);
			audio_QueueTrackPos( ID_SOUND_NEXUS_UNIT_ABSORBED, x, y, psD->pos.z );
		}
		// make the old droid vanish
		vanishDroid(psD);
		// create a new droid
		psNewDroid = buildDroid(&sTemplate, x, y, to, false);
		ASSERT(psNewDroid != NULL, "giftSingleUnit: unable to build a unit" );
		if (psNewDroid)
		{
			addDroid(psNewDroid, apsDroidLists);
			psNewDroid->body = body;
			for (impact_side = 0;impact_side < NUM_HIT_SIDES;impact_side=impact_side+1)
			{
				psNewDroid->armour[impact_side][WC_KINETIC] = armourK[impact_side];
				psNewDroid->armour[impact_side][WC_HEAT] = armourH[impact_side];
			}
			psNewDroid->experience = numKills;
			psNewDroid->direction = direction;
			if (!(psNewDroid->droidType == DROID_PERSON || cyborgDroid(psNewDroid) || psNewDroid->droidType == DROID_TRANSPORTER))
			{
				updateDroidOrientation(psNewDroid);
			}
		}
		return psNewDroid;
	}
}

/*calculates the electronic resistance of a droid based on its experience level*/
SWORD   droidResistance(DROID *psDroid)
{
    SWORD   resistance;

	CHECK_DROID(psDroid);

    resistance = (SWORD)(psDroid->experience * DROID_RESISTANCE_FACTOR);

    //ensure base minimum in MP before the upgrade effect
    if (bMultiPlayer)
    {
        //ensure resistance is a base minimum
        if (resistance < DROID_RESISTANCE_FACTOR)
        {
            resistance = DROID_RESISTANCE_FACTOR;
        }
    }

    //structure resistance upgrades are passed on to droids
    resistance = (SWORD)(resistance  + resistance * (
        asStructureUpgrade[psDroid->player].resistance/100));

    //ensure resistance is a base minimum
    if (resistance < DROID_RESISTANCE_FACTOR)
    {
        resistance = DROID_RESISTANCE_FACTOR;
    }

    return resistance;
}

/*this is called to check the weapon is 'allowed'. Check if VTOL, the weapon is
direct fire. Also check numVTOLattackRuns for the weapon is not zero - return
true if valid weapon*/
/* Watermelon:this will be buggy if the droid being checked has both AA weapon and non-AA weapon
Cannot think of a solution without adding additional return value atm.
*/
BOOL checkValidWeaponForProp(DROID_TEMPLATE *psTemplate)
{
	PROPULSION_STATS	*psPropStats;
	BOOL			bValid = true;

	//check propulsion stat for vtol
	psPropStats = asPropulsionStats + psTemplate->asParts[COMP_PROPULSION];
	ASSERT( psPropStats != NULL,
		"checkValidWeaponForProp: invalid propulsion stats pointer" );
	if (asPropulsionTypes[psPropStats->propulsionType].travel == AIR)
	{
		//check weapon stat for indirect
		if (!proj_Direct(asWeaponStats + psTemplate->asWeaps[0])
		    || !asWeaponStats[psTemplate->asWeaps[0]].vtolAttackRuns)
		{
			bValid = false;
		}
	}
	else
	{
		if ( asWeaponStats[psTemplate->asWeaps[0]].vtolAttackRuns )
		{
			bValid = false;
		}
	}

	//also checks that there is only a weapon attached and no other system component
	if (psTemplate->numWeaps == 0)
	{
		bValid = false;
	}
	if (psTemplate->asParts[COMP_BRAIN] != 0
	    && asWeaponStats[psTemplate->asWeaps[0]].weaponSubClass != WSC_COMMAND)
	{
		assert(false);
		bValid = false;
	}

	return bValid;
}

/*called when a Template is deleted in the Design screen*/
void deleteTemplateFromProduction(DROID_TEMPLATE *psTemplate, UBYTE player)
{
    STRUCTURE   *psStruct;
    UDWORD      inc, i;
	STRUCTURE	*psList;

    //see if any factory is currently using the template
	for (i=0; i<2; i++)
	{
		psList = NULL;
		switch (i)
		{
		case 0:
			psList = apsStructLists[player];
			break;
		case 1:
			psList = mission.apsStructLists[player];
			break;
		}
		for (psStruct = psList; psStruct != NULL; psStruct = psStruct->psNext)
		{
			if (StructIsFactory(psStruct))
			{
				FACTORY             *psFactory = &psStruct->pFunctionality->factory;
				DROID_TEMPLATE      *psNextTemplate = NULL;

				//if template belongs to the production player - check thru the production list (if struct is busy)
				if (player == productionPlayer && psFactory->psSubject)
				{
					for (inc = 0; inc < MAX_PROD_RUN; inc++)
					{
						if (asProductionRun[psFactory->psAssemblyPoint->factoryType][
	    					psFactory->psAssemblyPoint->factoryInc][inc].psTemplate == psTemplate)
						{
							//if this is the template currently being worked on
							if (psTemplate == (DROID_TEMPLATE *)psFactory->psSubject)
							{
								//set the quantity to 1 and then use factoryProdAdjust to subtract it
								asProductionRun[psFactory->psAssemblyPoint->factoryType][
	    							psFactory->psAssemblyPoint->factoryInc][inc].quantity = 1;
								factoryProdAdjust(psStruct, psTemplate, false);
								//init the factory production
								psFactory->psSubject = NULL;
								//check to see if anything left to produce
								psNextTemplate = factoryProdUpdate(psStruct, NULL);
								//power is returned by factoryProdAdjust()
								if (psNextTemplate)
								{
									structSetManufacture(psStruct, psNextTemplate,psFactory->quantity);
								}
								else
								{
									//nothing more to manufacture - reset the Subject and Tab on HCI Form
									intManufactureFinished(psStruct);
									//power is returned by factoryProdAdjust()
								}
							}
							else
							{
								//just need to initialise this production run
								asProductionRun[psFactory->psAssemblyPoint->factoryType][
	    							psFactory->psAssemblyPoint->factoryInc][inc].psTemplate = NULL;
								asProductionRun[psFactory->psAssemblyPoint->factoryType][
	    							psFactory->psAssemblyPoint->factoryInc][inc].quantity = 0;
								asProductionRun[psFactory->psAssemblyPoint->factoryType][
	    							psFactory->psAssemblyPoint->factoryInc][inc].built = 0;
							}
						}
					}
				}
				else
				{
					//not the production player, so check not being built in the factory for the template player
					if (psFactory->psSubject == (BASE_STATS *)psTemplate)
					{
						//clear the factories subject and quantity
						psFactory->psSubject = NULL;
						psFactory->quantity = 0;
						//return any accrued power
						if (psFactory->powerAccrued)
						{
							addPower(psStruct->player, psFactory->powerAccrued);
						}
						//tell the interface
						intManufactureFinished(psStruct);
					}
				}
			}
		}
	}
}


// Select a droid and do any necessary housekeeping.
//
void SelectDroid(DROID *psDroid)
{
	psDroid->selected = true;
	intRefreshScreen();
}


// De-select a droid and do any necessary housekeeping.
//
void DeSelectDroid(DROID *psDroid)
{
	psDroid->selected = false;
	intRefreshScreen();
}

/*calculate the power cost to repair a droid*/
UWORD powerReqForDroidRepair(DROID *psDroid)
{
    UWORD   powerReq;//powerPercent;

    powerReq = (UWORD)(repairPowerPoint(psDroid) * (psDroid->originalBody - psDroid->body));

    return powerReq;
}

/*power cost for One repair point*/
UWORD repairPowerPoint(DROID *psDroid)
{
    return (UWORD)(((POWER_FACTOR * calcDroidPower(psDroid)) / psDroid->originalBody) *
        REPAIR_POWER_FACTOR);
}

/** Callback function for stopped audio tracks
 *  Sets the droid's current track id to NO_SOUND
 *  \return true on success, false on failure
 */
BOOL droidAudioTrackStopped( void *psObj )
{
	DROID	*psDroid;

	psDroid = (DROID*)psObj;
	if (psDroid == NULL)
	{
		debug( LOG_ERROR, "droidAudioTrackStopped: droid pointer invalid\n" );
		return false;
	}

	if ( psDroid->type != OBJ_DROID || psDroid->died )
	{
		return false;
	}

	psDroid->iAudioID = NO_SOUND;
	return true;
}


/*returns true if droid type is one of the Cyborg types*/
BOOL cyborgDroid(const DROID* psDroid)
{
    return (psDroid->droidType == DROID_CYBORG
         || psDroid->droidType == DROID_CYBORG_CONSTRUCT
	 || psDroid->droidType == DROID_CYBORG_REPAIR
	 || psDroid->droidType == DROID_CYBORG_SUPER);
}

BOOL droidOnMap(const DROID *psDroid)
{
	if (psDroid->died == NOT_CURRENT_LIST || psDroid->droidType == DROID_TRANSPORTER
	    || psDroid->sMove.fx == INVALID_XY || psDroid->pos.x == INVALID_XY || missionIsOffworld()
	    || mapHeight == 0)
	{
		// Off world or on a transport or is a transport or in mission list, or on a mission, or no map - ignore
		return true;
	}
	return (worldOnMap(psDroid->sMove.fx, psDroid->sMove.fy)
	        && worldOnMap(psDroid->pos.x, psDroid->pos.y));
}

/** Teleport a droid to a new position on the map */
void droidSetPosition(DROID *psDroid, int x, int y)
{
	psDroid->pos.x = x;
	psDroid->pos.y = y;
	psDroid->pos.z = map_Height(psDroid->pos.x, psDroid->pos.y);
	initDroidMovement(psDroid);
	visTilesUpdate((BASE_OBJECT *)psDroid, rayTerrainCallback);
}

/** Check validity of a droid. Crash hard if it fails. */
void checkDroid(const DROID *droid, const char *const location, const char *function, const int recurse)
{
	int i;

	if (recurse < 0)
	{
		return;
	}

	ASSERT_HELPER(droid != NULL, location, function, "CHECK_DROID: NULL pointer");
	ASSERT_HELPER(droid->type == OBJ_DROID, location, function, "CHECK_DROID: Not droid (type %d)", (int)droid->type);
	ASSERT_HELPER(droid->direction <= 360.0f && droid->direction >= 0.0f, location, function, "CHECK_DROID: Bad droid direction %f", droid->direction);
	ASSERT_HELPER(droid->numWeaps <= DROID_MAXWEAPS, location, function, "CHECK_DROID: Bad number of droid weapons %d", (int)droid->numWeaps);
	ASSERT_HELPER(droid->listSize <= ORDER_LIST_MAX, location, function, "CHECK_DROID: Bad number of droid orders %d", (int)droid->listSize);
	ASSERT_HELPER(droid->player < MAX_PLAYERS, location, function, "CHECK_DROID: Bad droid owner %d", (int)droid->player);
	ASSERT_HELPER(droidOnMap(droid), location, function, "CHECK_DROID: Droid off map");

	for (i = 0; i < DROID_MAXWEAPS; ++i)
	{
		ASSERT_HELPER(droid->asWeaps[i].rotation <= 360, location, function, "CHECK_DROID: Bad turret rotation of turret %u", i);
		ASSERT_HELPER(droid->asWeaps[i].lastFired <= gameTime, location, function, "CHECK_DROID: Bad last fired time for turret %u", i);
		if (droid->psActionTarget[i])
		{
			ASSERT_HELPER(droid->psActionTarget[i]->direction >= 0.0f, location, function, "CHECK_DROID: Bad direction of turret %u's target", i);
		}
	}
}
