/*
 * ScriptFuncs.c
 *
 * All the C functions callable from the script code
 *
 */

#include <time.h>
#include <string.h>

#include "lib/framework/frame.h"
#include "lib/framework/strres.h"
#include "lib/widget/widget.h"

#include "effects.h"
#include "lib/script/script.h"
#include "scripttabs.h"
#include "lib/gamelib/gtime.h"
#include "objects.h"
#include "hci.h"
#include "message.h"
#include "intelmap.h"
#include "map.h"
#include "player.h"
#include "structure.h"
#include "display3d.h"
#include "research.h"
#include "lib/sound/audio.h"
#include "text.h"
#include "audio_id.h"
#include "power.h"
#include "console.h"
#include "scriptfuncs.h"
#include "geometry.h"
#include "visibility.h"
#include "gateway.h"
#include "drive.h"
#include "display.h"
#include "component.h"
#include "scriptextern.h"
#include "seqdisp.h"

#include "configuration.h"
#include "fpath.h"

#include "warzoneconfig.h"
#include "lighting.h"
#include "atmos.h"
#include "lib/sound/cdaudio.h"
#include "cdspan.h"
#include "lib/netplay/netplay.h"
#include "multiplay.h"
#include "multigifts.h"
#include "multilimit.h"
#include "advvis.h"

#include "lib/ivis_common/piestate.h"
#include "wrappers.h"
#include "order.h"
#include "orderdef.h"
#include "mission.h"
#include "loop.h"
#include "frontend.h"
#include "group.h"
#include "transporter.h"
#include "radar.h"
#include "levels.h"
#include "mission.h"
#include "projectile.h"
#include "cluster.h"
#include "multigifts.h"			//because of giftRadar()
#include "aiexperience.h"
#include "display3d.h"			//for showRangeAtPos()
#include "multimenu.h"


//used in the set nogoArea and LandingZone functions - use the ones defined in Map.h
//#define MAX_MAP_WIDTH		192
//#define MAX_MAP_HEIGHT		128

// If this is defined then check max number of units not reached before adding more.
#define SCRIPT_CHECK_MAX_UNITS

SDWORD		playerFlag[] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

// -----------------------------------------------------------------------------------------
BOOL	structHasModule(STRUCTURE *psStruct);
SDWORD	guessPlayerFromMessage(char **str);
SDWORD	getPlayerFromString(char *playerName);
SDWORD	getFirstWord(char *sText, char **sWord, SDWORD *readCount);

/******************************************************************************************/
/*                 Check for objects in areas                                             */

// check for a base object being in range of a point
BOOL objectInRange(BASE_OBJECT *psList, SDWORD x, SDWORD y, SDWORD range)
{
	BASE_OBJECT		*psCurr;
	SDWORD			xdiff, ydiff, rangeSq;

	// See if there is a droid in range
	rangeSq = range * range;
	for(psCurr = psList; psCurr; psCurr = psCurr->psNext)
	{
		// skip partially build structures
		if ( (psCurr->type == OBJ_STRUCTURE) &&
			 (((STRUCTURE *)psCurr)->status != SS_BUILT) )
		{
			continue;
		}

		// skip flying vtols
		if ( (psCurr->type == OBJ_DROID) &&
			 vtolDroid((DROID *)psCurr) &&
			 ((DROID *)psCurr)->sMove.Status != MOVEINACTIVE )
		{
			continue;
		}

		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff < rangeSq)
		{
			return TRUE;
		}
	}

	return FALSE;
}

// -----------------------------------------------------------------------------------------
// Check for any player object being within a certain range of a position
BOOL scrObjectInRange(void)
{
	SDWORD		range, player, x,y;
	BOOL		found;

	if (!stackPopParams(4, VAL_INT, &player, VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrObjectInRange: invalid player number" );
		return FALSE;
	}

	found = objectInRange((BASE_OBJECT *)apsDroidLists[player], x,y, range) ||
			objectInRange((BASE_OBJECT *)apsStructLists[player], x,y, range);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Check for a droid being within a certain range of a position
BOOL scrDroidInRange(void)
{
	SDWORD		range, player, x,y;
	BOOL		found;

	if (!stackPopParams(4, VAL_INT, &player, VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrUnitInRange: invalid player number" );
		return FALSE;
	}

	found = objectInRange((BASE_OBJECT *)apsDroidLists[player], x,y, range);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Check for a struct being within a certain range of a position
BOOL scrStructInRange(void)
{
	SDWORD		range, player, x,y;
	BOOL		found;

	if (!stackPopParams(4, VAL_INT, &player, VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructInRange: invalid player number" );
		return FALSE;
	}

	found = objectInRange((BASE_OBJECT *)apsStructLists[player], x,y, range);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrPlayerPower(void)
{
	SDWORD player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}
	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrPlayerPower: invalid player number" );
		return FALSE;
	}
	if (!stackPushResult(VAL_INT, asPower[player]->currentPower))
	{
		return FALSE;
	}
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// check for a base object being in an area
static BOOL objectInArea(BASE_OBJECT *psList, SDWORD x1, SDWORD y1, SDWORD x2, SDWORD y2)
{
	BASE_OBJECT		*psCurr;
	SDWORD			ox,oy;

	// See if there is a droid in Area
	for(psCurr = psList; psCurr; psCurr = psCurr->psNext)
	{
		// skip partially build structures
		if ( (psCurr->type == OBJ_STRUCTURE) &&
			 (((STRUCTURE *)psCurr)->status != SS_BUILT) )
		{
			continue;
		}

		ox = (SDWORD)psCurr->x;
		oy = (SDWORD)psCurr->y;
		if (ox >= x1 && ox <= x2 &&
			oy >= y1 && oy <= y2)
		{
			return TRUE;
		}
	}

	return FALSE;
}

// -----------------------------------------------------------------------------------------
// Check for any player object being within a certain area
BOOL scrObjectInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	BOOL		found;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrObjectInArea: invalid player number" );
		return FALSE;
	}

	found = objectInArea((BASE_OBJECT *)apsDroidLists[player], x1,y1, x2,y2) ||
			objectInArea((BASE_OBJECT *)apsStructLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Check for a droid being within a certain area
BOOL scrDroidInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	BOOL		found;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrUnitInArea: invalid player number" );
		return FALSE;
	}

	found = objectInArea((BASE_OBJECT *)apsDroidLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Check for a struct being within a certain Area of a position
BOOL scrStructInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	BOOL		found;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructInArea: invalid player number" );
		return FALSE;
	}

	found = objectInArea((BASE_OBJECT *)apsStructLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
BOOL scrSeenStructInArea(void)
{
	BOOL		walls=FALSE,found = FALSE;
	SDWORD		player,enemy,x1,y1, x2,y2;
	STRUCTURE	*psCurr;
	SDWORD		ox,oy;

	// player, enemyplayer, walls, x1,r1,x2,y2
	if (!stackPopParams(7, VAL_INT, &player, VAL_INT, &enemy, VAL_BOOL,&walls,VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}


	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSeenStructInArea: invalid player number" );
		return FALSE;
	}

	for(psCurr = apsStructLists[enemy]; psCurr; psCurr = psCurr->psNext)
	{
		// skip partially build structures
		if ( (psCurr->type == OBJ_STRUCTURE) && (((STRUCTURE *)psCurr)->status != SS_BUILT) )
		{
			continue;
		}

		// possible skip walls
		if(walls && (psCurr->pStructureType->type != REF_WALL  || psCurr->pStructureType->type !=REF_WALLCORNER))
		{
			continue;
		}

		ox = (SDWORD)psCurr->x;
		oy = (SDWORD)psCurr->y;
		if (ox >= x1 && ox <= x2 &&	oy >= y1 && oy <= y2)
		{
			// structure is in area.
			if(psCurr->visible[player])
			{
				found = TRUE;
			}
		}
	}

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Check for a players structures but no walls being within a certain area
BOOL scrStructButNoWallsInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	SDWORD		ox,oy;
	STRUCTURE	*psStruct;
	SDWORD		found = FALSE;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructButNoWallsInArea: invalid player number" );
		return FALSE;
	}

	for(psStruct = apsStructLists[player]; psStruct; psStruct = psStruct->psNext)
	{
		if ((psStruct->pStructureType->type != REF_WALL) &&
			(psStruct->pStructureType->type != REF_WALLCORNER) &&
			(psStruct->status == SS_BUILT) )
		{
			ox = (SDWORD)psStruct->x;
			oy = (SDWORD)psStruct->y;
			if ((ox >= x1) && (ox <= x2) &&
				(oy >= y1) && (oy <= y2))
			{
				found = TRUE;
				break;
			}
		}
	}

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// check for the number of base objects in an area
static SDWORD numObjectsInArea(BASE_OBJECT *psList, SDWORD x1, SDWORD y1, SDWORD x2, SDWORD y2)
{
	BASE_OBJECT		*psCurr;
	SDWORD			ox,oy;
	SDWORD			count;

	// See if there is a droid in Area
	count = 0;
	for(psCurr = psList; psCurr; psCurr = psCurr->psNext)
	{
		// skip partially build structures
		if ( (psCurr->type == OBJ_STRUCTURE) &&
			 (((STRUCTURE *)psCurr)->status != SS_BUILT) )
		{
			continue;
		}

		ox = (SDWORD)psCurr->x;
		oy = (SDWORD)psCurr->y;
		if (ox >= x1 && ox <= x2 &&
			oy >= y1 && oy <= y2)
		{
			count += 1;
		}
	}

	return count;
}

// -----------------------------------------------------------------------------------------
// Count the number of player objects within a certain area
BOOL scrNumObjectsInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	SDWORD		count;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumObjectsInArea: invalid player number" );
		return FALSE;
	}

	count = numObjectsInArea((BASE_OBJECT *)apsDroidLists[player], x1,y1, x2,y2) +
			numObjectsInArea((BASE_OBJECT *)apsStructLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_INT, count))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Count the number of player droids within a certain area
BOOL scrNumDroidsInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	SDWORD		count;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumUnitInArea: invalid player number" );
		return FALSE;
	}

	count = numObjectsInArea((BASE_OBJECT *)apsDroidLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_INT, count))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Count the number of player structures within a certain area
BOOL scrNumStructsInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	SDWORD		count;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumStructsInArea: invalid player number" );
		return FALSE;
	}

	count = numObjectsInArea((BASE_OBJECT *)apsStructLists[player], x1,y1, x2,y2);

	if (!stackPushResult(VAL_INT, count))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Count the number of player structures but not walls within a certain area
BOOL scrNumStructsButNotWallsInArea(void)
{
	SDWORD		player, x1,y1, x2,y2;
	SDWORD		count, ox,oy;
	STRUCTURE	*psStruct;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumStructsButNotWallsInArea: invalid player number" );
		return FALSE;
	}

	count = 0;
	for(psStruct = apsStructLists[player]; psStruct; psStruct = psStruct->psNext)
	{
		if ((psStruct->pStructureType->type != REF_WALL) &&
			(psStruct->pStructureType->type != REF_WALLCORNER) &&
			(psStruct->status == SS_BUILT))
		{
			ox = (SDWORD)psStruct->x;
			oy = (SDWORD)psStruct->y;
			if ((ox >= x1) && (ox <= x2) &&
				(oy >= y1) && (oy <= y2))
			{
				count += 1;
			}
		}
	}

	if (!stackPushResult(VAL_INT, count))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Count the number of structures in an area of a certain type
BOOL scrNumStructsByTypeInArea(void)
{
	SDWORD		player, type, x1,y1, x2,y2;
	SDWORD		count, ox,oy;
	STRUCTURE	*psStruct;

	if (!stackPopParams(6, VAL_INT, &player, VAL_INT, &type,
					VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumStructsByTypeInArea: invalid player number" );
		return FALSE;
	}

	count = 0;
	for(psStruct = apsStructLists[player]; psStruct; psStruct = psStruct->psNext)
	{
		if ((psStruct->pStructureType->type == (UDWORD)type) &&
			(psStruct->status == SS_BUILT))
		{
			ox = (SDWORD)psStruct->x;
			oy = (SDWORD)psStruct->y;
			if ((ox >= x1) && (ox <= x2) &&
				(oy >= y1) && (oy <= y2))
			{
				count += 1;
			}
		}
	}

	if (!stackPushResult(VAL_INT, count))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Check for a droid having seen a certain object
BOOL scrDroidHasSeen(void)
{
	SDWORD		player;
	BASE_OBJECT	*psObj;
	BOOL		seen;

	if (!stackPopParams(2, ST_BASEOBJECT, &psObj, VAL_INT, &player))
	{
		return FALSE;
	}

	if (psObj == NULL)
	{
		ASSERT( FALSE, "scrUnitHasSeen: NULL object" );
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrUnitHasSeen:player number is too high" );
		return FALSE;
	}

	// See if any droid has seen this object
	seen = FALSE;
	if (psObj->visible[player])
	{
		seen = TRUE;
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)seen))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Check for a droid being within range of a position
static BOOL scrDroidInRangeOfPosition(void)
{
	SDWORD		range, player;
	DROID		*psCurr;
	SDWORD		rangeSquared;
	SDWORD		dx, dy, dz, iX, iY, iZ;
	BOOL		found;

	if (!stackPopParams(5, VAL_INT, &range, VAL_INT, &player,
						VAL_INT, &iX, VAL_INT, &iY, VAL_INT, &iZ ))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrUnitInRangeOfPosition: invalid player number" );
		return FALSE;
	}

	// See if there is a droid in range
	rangeSquared = range * range;
	found = FALSE;
	for(psCurr = apsDroidLists[player]; psCurr; psCurr = psCurr->psNext)
	{
		dx = (SDWORD)psCurr->x - iX;
		dy = (SDWORD)psCurr->y - iY;
		dz = (SDWORD)psCurr->z - iZ;

		if ( (dx*dx + dy*dy + dz*dz) < rangeSquared)
		{
			found = TRUE;
			break;
		}
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)found))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Enable a component to be researched
BOOL scrEnableComponent(void)
{
	SDWORD		player;
	INTERP_VAL	sVal;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}
	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrEnableComponent:player number is too high" );
		return FALSE;
	}

	// enable the appropriate component
	switch (sVal.type)
	{
	case ST_BODY:
		apCompLists[player][COMP_BODY][sVal.v.ival] = FOUND;
		break;
	case ST_PROPULSION:
		apCompLists[player][COMP_PROPULSION][sVal.v.ival] = FOUND;
		break;
	case ST_ECM:
		apCompLists[player][COMP_ECM][sVal.v.ival] = FOUND;
		break;
	case ST_SENSOR:
		apCompLists[player][COMP_SENSOR][sVal.v.ival] = FOUND;
		break;
	case ST_CONSTRUCT:
		apCompLists[player][COMP_CONSTRUCT][sVal.v.ival] = FOUND;
		break;
	case ST_WEAPON:
		apCompLists[player][COMP_WEAPON][sVal.v.ival] = FOUND;
		break;
	case ST_REPAIR:
		apCompLists[player][COMP_REPAIRUNIT][sVal.v.ival] = FOUND;
		break;
	case ST_BRAIN:
		apCompLists[player][COMP_BRAIN][sVal.v.ival] = FOUND;
		break;
	default:
		ASSERT( FALSE, "scrEnableComponent: unknown type" );
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Make a component available
BOOL scrMakeComponentAvailable(void)
{
	SDWORD		player;
	INTERP_VAL	sVal;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}
	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrMakeComponentAvailable:player number is too high" );
		return FALSE;
	}

	// make the appropriate component available
	switch (sVal.type)
	{
	case ST_BODY:
		apCompLists[player][COMP_BODY][sVal.v.ival] = AVAILABLE;
		break;
	case ST_PROPULSION:
		apCompLists[player][COMP_PROPULSION][sVal.v.ival] = AVAILABLE;
		break;
	case ST_ECM:
		apCompLists[player][COMP_ECM][sVal.v.ival] = AVAILABLE;
		break;
	case ST_SENSOR:
		apCompLists[player][COMP_SENSOR][sVal.v.ival] = AVAILABLE;
		break;
	case ST_CONSTRUCT:
		apCompLists[player][COMP_CONSTRUCT][sVal.v.ival] = AVAILABLE;
		break;
	case ST_WEAPON:
		apCompLists[player][COMP_WEAPON][sVal.v.ival] = AVAILABLE;
		break;
	case ST_REPAIR:
		apCompLists[player][COMP_REPAIRUNIT][sVal.v.ival] = AVAILABLE;
		break;
	case ST_BRAIN:
		apCompLists[player][COMP_BRAIN][sVal.v.ival] = AVAILABLE;
		break;
	default:
		ASSERT( FALSE, "scrEnableComponent: unknown type" );
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Add a droid
BOOL scrAddDroidToMissionList(void)
{
	SDWORD			player;
	DROID_TEMPLATE	*psTemplate;
	DROID			*psDroid;

	if (!stackPopParams(2, ST_TEMPLATE, &psTemplate, VAL_INT, &player))
	{
		return FALSE;
	}

/*	if ((UBYTE)player == selectedPlayer )
	{
		ASSERT( FALSE, "scrAddDroidToMissionList: can't add own player to list" );
		return FALSE;
	}*/

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddUnitToMissionList:player number is too high" );
		return FALSE;
	}

	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),
		"scrAddUnitToMissionList: Invalid template pointer" );

#ifdef SCRIPT_CHECK_MAX_UNITS
	// Don't build a new droid if player limit reached, unless it's a transporter.
	if( IsPlayerDroidLimitReached(player) && (psTemplate->droidType != DROID_TRANSPORTER) ) {
		debug( LOG_NEVER, "scrAddUnit : Max units reached ,player %d\n", player );
		psDroid = NULL;
	} else
#endif
	{
		psDroid = buildMissionDroid( psTemplate, 128, 128, player );
	}

	if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)psDroid))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Add a droid
BOOL scrAddDroid(void)
{
	SDWORD			x, y, player;
//	INTERP_VAL		sVal;
	DROID_TEMPLATE	*psTemplate;
	DROID			*psDroid;

	if (!stackPopParams(4, ST_TEMPLATE, &psTemplate, VAL_INT, &x, VAL_INT, &y, VAL_INT, &player))
	{
		return FALSE;
	}
/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}
	if (sVal.type != ST_TEMPLATE)
	{
		ASSERT( FALSE, "scrAddDroid: type mismatch for object" );
		return FALSE;
	}
	psTemplate = (DROID_TEMPLATE *)sVal.v.ival;
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddUnit:player number is too high" );
		return FALSE;
	}

	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),
		"scrAddUnit: Invalid template pointer" );

#ifdef SCRIPT_CHECK_MAX_UNITS
	// Don't build a new droid if player limit reached, unless it's a transporter.
	if( IsPlayerDroidLimitReached(player) && (psTemplate->droidType != DROID_TRANSPORTER) ) {
		debug( LOG_NEVER, "scrAddUnit : Max units reached ,player %d\n", player );
		psDroid = NULL;
	} else
#endif
	{
		psDroid = buildDroid(psTemplate, x, y, player, FALSE);
		if (psDroid)
		{
			addDroid(psDroid, apsDroidLists);
			if (vtolDroid(psDroid))
			{
				// vtols start in the air
				moveMakeVtolHover(psDroid);
			}
		}
	}

	if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)psDroid))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Add droid to transporter

BOOL scrAddDroidToTransporter(void)
{
	DROID	*psTransporter, *psDroid;

	if (!stackPopParams(2, ST_DROID, &psTransporter, ST_DROID, &psDroid))
	{
		return FALSE;
	}

    if (psTransporter == NULL OR psDroid == NULL)
    {
        //ignore!
        ASSERT( FALSE, "scrAddUnitToTransporter: null unit passed" );
        return TRUE;
    }

	ASSERT( PTRVALID(psTransporter, sizeof(DROID)),
			"scrAddUnitToTransporter: invalid transporter pointer" );
	ASSERT( PTRVALID(psDroid, sizeof(DROID)),
			"scrAddUnitToTransporter: invalid unit pointer" );
	ASSERT( psTransporter->droidType == DROID_TRANSPORTER,
			"scrAddUnitToTransporter: invalid transporter type" );

	/* check for space */
	if (checkTransporterSpace(psTransporter, psDroid))
	{
		if (droidRemove(psDroid, mission.apsDroidLists))
        {
		    grpJoin(psTransporter->psGroup, psDroid);
        }
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//check for a building to have been destroyed
BOOL scrBuildingDestroyed(void)
{
	SDWORD		player;
	UDWORD		structureID;
//	INTERP_VAL	sVal;
	BOOL		destroyed;
	STRUCTURE	*psCurr;

	if (!stackPopParams(2, ST_STRUCTUREID, &structureID, VAL_INT, &player))
	{
		return FALSE;
	}
/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_STRUCTUREID)
	{
		ASSERT( FALSE, "scrBuildingDestroyed: type mismatch for object" );
		return FALSE;
	}
	structureID = (UDWORD)sVal.v.ival;
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrBuildingDestroyed:player number is too high" );
		return FALSE;
	}

	destroyed = TRUE;
	//look thru the players list to see if the structure still exists
	for (psCurr = apsStructLists[player]; psCurr != NULL; psCurr = psCurr->psNext)
	{
		if (psCurr->id == structureID)
		{
			destroyed = FALSE;
		}
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)destroyed))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Enable a structure to be built
BOOL scrEnableStructure(void)
{
	SDWORD		player, index;
//	INTERP_VAL	sVal;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &index, VAL_INT, &player))
	{
		return FALSE;
	}
/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_STRUCTURESTAT)
	{
		ASSERT( FALSE, "scrEnableStructure: type mismatch for object" );
		return FALSE;
	}*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrEnableStructure:player number is too high" );
		return FALSE;
	}

	if (index < (SDWORD)0 OR index > (SDWORD)numStructureStats)
	{
		ASSERT( FALSE, "scrEnableStructure:invalid structure stat" );
		return FALSE;
	}

	// enable the appropriate structure
	apStructTypeLists[player][index] = AVAILABLE;

	return TRUE;
}



// -----------------------------------------------------------------------------------------
// Check if a structure can be built.
// currently PC skirmish only.
BOOL scrIsStructureAvailable(void)
{
	SDWORD		player, index;
	BOOL		result;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &index, VAL_INT, &player))
	{
		return FALSE;
	}
	if(	apStructTypeLists[player][index] == AVAILABLE)
	{
		result = TRUE;
	}
	else
	{
		result = FALSE;
	}

	if (!stackPushResult(VAL_BOOL, result))
	{
		return FALSE;
	}
	return TRUE;
}


// -----------------------------------------------------------------------------------------
//make the droid with the matching id the currently selected droid
BOOL scrSelectDroidByID(void)
{
	SDWORD			player, droidID;
//	INTERP_VAL		sVal;
	BOOL			selected;

	if (!stackPopParams(2, ST_DROIDID, &droidID, VAL_INT, &player))
	{
		return FALSE;
	}
/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_DROIDID)
	{
		ASSERT( FALSE, "scrSelectDroidByID: type mismatch for object" );
		return FALSE;
	}
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSelectUnitByID:player number is too high" );
		return FALSE;
	}

	selected = FALSE;
	if (selectDroidByID(droidID, player))
	{
		selected = TRUE;
	}

	//store the reult cos might need to check the droid exists before doing anything else
	if (!stackPushResult(VAL_BOOL, (UDWORD)selected))
	{
		return FALSE;
	}
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Pop up a message box with a number value in it
BOOL scrNumMB(void)
{
	SDWORD	val;

	if (!stackPopParams(1, VAL_INT, &val))
	{
		return FALSE;
	}

/*	gameTimeStop();
	DBERROR(("scrNumMB: called by script with value: %d", val));
	gameTimeStart();*/
	debug( LOG_NEVER, "scrNumMB: called by script with value: %d\n", val );

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Do an approximation to a square root
BOOL scrApproxRoot(void)
{
	SDWORD	val1, val2;

	if (!stackPopParams(2, VAL_INT, &val1, VAL_INT, &val2))
	{
		return FALSE;
	}

	if (val1 < val2)
	{
		val1 = val2 + (val1 >> 1);
	}
	else
	{
		val1 = val1 + (val2 >> 1);
	}

	if (!stackPushResult(VAL_INT, val1))
	{
		return FALSE;
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
extern void intShowReticuleButton(UDWORD id,BOOL Show);
// Add a reticule button to the interface
BOOL scrAddReticuleButton(void)
{
	SDWORD	val;


	if (!stackPopParams(1, VAL_INT, &val))
	{
		return FALSE;
	}

	//set the appropriate flag to 'draw' the button
	switch (val)
	{
	case IDRET_OPTIONS:
		// bit of a hack here to keep compatibility with old scripts
		widgReveal(psWScreen, IDRET_COMMAND);
		break;
	case IDRET_COMMAND:
		widgReveal(psWScreen, IDRET_COMMAND);
		break;
	case IDRET_BUILD:
		widgReveal(psWScreen, IDRET_BUILD);
		break;
	case IDRET_MANUFACTURE:
		widgReveal(psWScreen, IDRET_MANUFACTURE);
		break;
	case IDRET_RESEARCH:
		widgReveal(psWScreen, IDRET_RESEARCH);
		break;
	case IDRET_INTEL_MAP:
		widgReveal(psWScreen, IDRET_INTEL_MAP);
		break;
	case IDRET_DESIGN:
		widgReveal(psWScreen, IDRET_DESIGN);
		break;
	case IDRET_CANCEL:
		widgReveal(psWScreen, IDRET_CANCEL);
		break;
	default:
		ASSERT( FALSE, "scrAddReticuleButton: Invalid reticule Button ID" );
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//Remove a reticule button from the interface
BOOL scrRemoveReticuleButton(void)
{
	SDWORD	val;

	BOOL	bReset;



	if (!stackPopParams(2, VAL_INT, &val,VAL_BOOL, &bReset))
	{
		return FALSE;
	}


	if(bInTutorial)
	{
		if(bReset)	// not always desirable
		{
			intResetScreen(TRUE);
		}
	}
	switch (val)
	{
	case IDRET_OPTIONS:
		// bit of a hack here to keep compatibility with old scripts
		widgHide(psWScreen, IDRET_COMMAND);
		break;
	case IDRET_COMMAND:
		widgHide(psWScreen, IDRET_COMMAND);
		break;
	case IDRET_BUILD:
		widgHide(psWScreen, IDRET_BUILD);
		break;
	case IDRET_MANUFACTURE:
		widgHide(psWScreen, IDRET_MANUFACTURE);
		break;
	case IDRET_RESEARCH:
		widgHide(psWScreen, IDRET_RESEARCH);
		break;
	case IDRET_INTEL_MAP:
		widgHide(psWScreen, IDRET_INTEL_MAP);
		break;
	case IDRET_DESIGN:
		widgHide(psWScreen, IDRET_DESIGN);
		break;
	case IDRET_CANCEL:
		widgHide(psWScreen, IDRET_CANCEL);
		break;
	default:
		ASSERT( FALSE, "scrAddReticuleButton: Invalid reticule Button ID" );
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// add a message to the Intelligence Display
BOOL scrAddMessage(void)
{
	MESSAGE			*psMessage;
	SDWORD			msgType, player;
	BOOL			playImmediate;
//	INTERP_VAL		sVal;
	VIEWDATA		*psViewData;
	UDWORD			height;


	if (!stackPopParams(4, ST_INTMESSAGE, &psViewData , VAL_INT, &msgType,
				VAL_INT, &player, VAL_BOOL, &playImmediate))
	{
		return FALSE;
	}

/*
	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_INTMESSAGE)
	{
		ASSERT( FALSE, "scrAddMessage: type mismatch for object" );
		return FALSE;
	}
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddMessage:player number is too high" );
		return FALSE;
	}

	//create the message
	psMessage = addMessage(msgType, FALSE, player);
	if (psMessage)
	{
		//set the data
		psMessage->pViewData = (MSG_VIEWDATA *)psViewData;
		if (msgType == MSG_PROXIMITY)
		{
			//check the z value is at least the height of the terrain
			height = map_Height(((VIEW_PROXIMITY *)psViewData->pData)->x,
				((VIEW_PROXIMITY *)psViewData->pData)->y);
			if (((VIEW_PROXIMITY *)psViewData->pData)->z < height)
			{
				((VIEW_PROXIMITY *)psViewData->pData)->z = height;
			}
		}

		if (playImmediate)
		{
	//		psCurrentMsg = psMessage;
			//initTextDisplay(psCurrentMsg, WFont, 255);
			//addIntelScreen(TRUE);
	//		addIntelScreen();
			displayImmediateMessage(psMessage);
			stopReticuleButtonFlash(IDRET_INTEL_MAP);
		}
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// remove a message from the Intelligence Display
BOOL scrRemoveMessage(void)
{
	MESSAGE			*psMessage;
	SDWORD			msgType, player;
	VIEWDATA		*psViewData;


	if (!stackPopParams(3, ST_INTMESSAGE, &psViewData , VAL_INT, &msgType,
				VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddMessage:player number is too high" );
		return FALSE;
	}

	//find the message
	psMessage = findMessage((MSG_VIEWDATA *)psViewData, msgType, player);
	if (psMessage)
	{
		//delete it
		removeMessage(psMessage, player);
	}
	else
	{
		ASSERT( FALSE, "scrRemoveMessage:cannot find message - %s",
			psViewData->pName );
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// add a tutorial message to the Intelligence Display
/*BOOL scrAddTutorialMessage(void)
{
	SDWORD			player;
	VIEWDATA		*psViewData;


	if (!stackPopParams(2, ST_INTMESSAGE, &psViewData , VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddTutorialMessage:player number is too high" );
		return FALSE;
	}

	//set the data
	tutorialMessage.pViewData = psViewData;
	tutorialMessage.player = player;

	//play the tutorial message immediately
	psCurrentMsg = &tutorialMessage;
	initTextDisplay(psCurrentMsg, WFont, 255);
	addIntelScreen(TRUE);

	return TRUE;
}*/

// -----------------------------------------------------------------------------------------
/*builds a droid in the specified factory*/
BOOL scrBuildDroid(void)
{
	SDWORD			player, productionRun;
//	INTERP_VAL		sVal, sVal2;
	STRUCTURE		*psFactory;
	DROID_TEMPLATE	*psTemplate;

	if (!stackPopParams(4, ST_TEMPLATE, &psTemplate, ST_STRUCTURE, &psFactory,
					VAL_INT, &player, VAL_INT, &productionRun))
	{
		return FALSE;
	}

	if (psFactory == NULL)
	{
		ASSERT( FALSE, "scrBuildUnit: NULL factory object" );
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrBuildUnit:player number is too high" );
		return FALSE;
	}

	if (productionRun > UBYTE_MAX)
	{
		ASSERT( FALSE, "scrBuildUnit: production run too high" );
		return FALSE;
	}

	ASSERT( PTRVALID(psFactory, sizeof(STRUCTURE)),
		"scrBuildUnit: Invalid structure pointer" );
	ASSERT( (psFactory->pStructureType->type == REF_FACTORY OR
		psFactory->pStructureType->type == REF_CYBORG_FACTORY OR
		psFactory->pStructureType->type == REF_VTOL_FACTORY),
		"scrBuildUnit: structure is not a factory" );
	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),
		"scrBuildUnit: Invalid template pointer" );

	//check building the right sort of droid for the factory
	if (!validTemplateForFactory(psTemplate, psFactory))
	{

		ASSERT( FALSE, "scrBuildUnit: invalid template - %s for factory - %s",
			psTemplate->aName, psFactory->pStructureType->pName );

		return FALSE;
	}

	structSetManufacture(psFactory, psTemplate, (UBYTE)productionRun);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// for a specified structure, set the assembly point droids go to when built
BOOL	scrSetAssemblyPoint(void)
{
	SDWORD		x, y;
	STRUCTURE	*psBuilding;

	if (!stackPopParams(3, ST_STRUCTURE, &psBuilding, VAL_INT, &x, VAL_INT, &y))
	{
		return FALSE;
	}

	if (psBuilding == NULL)
	{
		ASSERT( FALSE, "scrSetAssemblyPoint: NULL structure" );
		return FALSE;
	}

	if (psBuilding->pStructureType->type != REF_FACTORY AND
		psBuilding->pStructureType->type != REF_CYBORG_FACTORY AND
		psBuilding->pStructureType->type != REF_VTOL_FACTORY)
	{
		ASSERT( FALSE, "scrSetAssemblyPoint: structure is not a factory" );
		return FALSE;
	}

	setAssemblyPoint(((FACTORY *)psBuilding->pFunctionality)->psAssemblyPoint,x,y,
        psBuilding->player, TRUE);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// test for structure is idle or not
BOOL	scrStructureIdle(void)
{
//	INTERP_VAL	sVal;
	STRUCTURE	*psBuilding;
	BOOL		idle;

	if (!stackPopParams(1, ST_STRUCTURE, &psBuilding))
	{
		return FALSE;
	}
//	DBPRINTF(("scrStructureIdle called\n"));

	if (psBuilding == NULL)
	{
		ASSERT( FALSE, "scrStructureIdle: NULL structure" );
		return FALSE;
	}

	//test for idle
	idle = FALSE;
	if (structureIdle(psBuilding))
	{
		idle = TRUE;
	}


//	DBPRINTF(("structure %p is %d\n",psBuilding,idle));

	if (!stackPushResult(VAL_BOOL, (UDWORD)idle))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// sends a players droids to a location to attack
BOOL	scrAttackLocation(void)
{
	SDWORD		player, x, y;

	if (!stackPopParams(3, VAL_INT, &x, VAL_INT, &y, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAttackLocation:player number is too high" );
		return FALSE;
	}

	attackLocation(x, y, player);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//Destroy a feature
BOOL scrDestroyFeature(void)
{
	FEATURE		*psFeature;
//	INTERP_VAL	sVal;

	if (!stackPopParams(1, ST_FEATURE, &psFeature))
	{
		return FALSE;
	}

	if (psFeature == NULL)
	{
		ASSERT( PTRVALID(psFeature, sizeof(FEATURE)),
			"scrDestroyFeature: Invalid feature pointer" );
	}

	removeFeature(psFeature);

	return TRUE;
}




// -----------------------------------------------------------------------------------------
// static vars to enum features.
static	FEATURE_STATS	*psFeatureStatToFind[MAX_PLAYERS];
static	SDWORD			playerToEnum[MAX_PLAYERS];
static  SDWORD			getFeatureCount[MAX_PLAYERS]={0};
//static	FEATURE			*psCurrEnumFeature[MAX_PLAYERS];

// -----------------------------------------------------------------------------------------
// init enum visible features.
BOOL scrInitGetFeature(void)
{
	SDWORD			player,iFeat,bucket;

	if ( !stackPopParams(3, ST_FEATURESTAT, &iFeat,  VAL_INT, &player,VAL_INT,&bucket) )
	{
		return FALSE;
	}

	psFeatureStatToFind[bucket]		= (FEATURE_STATS *)(asFeatureStats + iFeat);				// find this stat
	playerToEnum[bucket]			= player;				// that this player can see
//	psCurrEnumFeature[bucket]		= apsFeatureLists[0];
	getFeatureCount[bucket]			= 0;					// start at the beginning of list.
	return TRUE;
}

// -----------------------------------------------------------------------------------------
// get next visible feature of required type
// notes:	can't rely on just using the feature list, since it may change
//			between calls, Use an index into list instead.
//			Doesn't return Features sharing a tile with a structure.
//			Skirmish Only, dunno if kev uses this?
BOOL scrGetFeature(void)
{
	SDWORD	bucket,count;
	FEATURE	*psFeat;

	if ( !stackPopParams(1,VAL_INT,&bucket) )
	{
		ASSERT( FALSE, "scrGetFeature: Failed to pop player number from stack" );
		return FALSE;
	}

	count =0;
	// go to the correct start point in the feature list.
	for(psFeat=apsFeatureLists[0];psFeat && count<getFeatureCount[bucket] ;count++)
	{
		psFeat = psFeat->psNext;
	}

	if(psFeat == NULL)		// no more to find.
	{
		if (!stackPushResult((INTERP_TYPE)ST_FEATURE, (SDWORD)NULL))
		{
			ASSERT( FALSE, "scrGetFeature: Failed to push result" );
			return FALSE;
		}
		return TRUE;
	}

	// check to see if badly called
	if(psFeatureStatToFind[bucket] == NULL)
	{
		debug( LOG_NEVER, "invalid feature to find. possibly due to save game\n" );
		if(!stackPushResult((INTERP_TYPE)ST_FEATURE,(SDWORD)NULL))
		{
			ASSERT( FALSE, "scrGetFeature: Failed to push result" );
			return FALSE;
		}
		return TRUE;
	}

	// begin searching the feature list for the required stat.
	while(psFeat)
	{
		if(	( psFeat->psStats->subType == psFeatureStatToFind[bucket]->subType)
			&&( psFeat->visible[playerToEnum[bucket]]	!= 0)
			&&!TILE_HAS_STRUCTURE(mapTile(psFeat->x>>TILE_SHIFT,psFeat->y>>TILE_SHIFT) )
			&&!fireOnLocation(psFeat->x,psFeat->y)		// not burning.
			)
		{
			if (!stackPushResult((INTERP_TYPE)ST_FEATURE,(SDWORD)psFeat))	//	push result
			{
				ASSERT( FALSE, "scrGetFeature: Failed to push result" );
				return FALSE;
			}

			getFeatureCount[bucket]++;
			return TRUE;
		}
		getFeatureCount[bucket]++;
		psFeat=psFeat->psNext;
	}

	// none found
	if (!stackPushResult((INTERP_TYPE)ST_FEATURE, (UDWORD)NULL))
	{
		ASSERT( FALSE, "scrGetFeature: Failed to push result" );
		return FALSE;
	}
	return TRUE;
}


/*
// -----------------------------------------------------------------------------------------
// enum next visible feature of required type.
// note: wont return features covered by structures (ie oil resources)
// YUK NASTY BUG. CANT RELY ON THE FEATURE LIST BETWEEN CALLS.
BOOL scrGetFeature(void)
{
	SDWORD	bucket;

	if ( !stackPopParams(1,VAL_INT,&bucket) )
	{
		return FALSE;
	}

	while(psCurrEnumFeature[bucket])
	{
		if(	( psCurrEnumFeature[bucket]->psStats->subType == psFeatureStatToFind[bucket]->subType)
			&&
			( psCurrEnumFeature[bucket]->visible[playerToEnum[bucket]]	!= 0)
			&&
			!TILE_HAS_STRUCTURE(mapTile(psCurrEnumFeature[bucket]->x>>TILE_SHIFT,psCurrEnumFeature[bucket]->y>>TILE_SHIFT) )
		   )
		{
			if (!stackPushResult(ST_FEATURE,(UDWORD) psCurrEnumFeature[bucket]))			//	push result
			{
				return FALSE;
			}
			psCurrEnumFeature[bucket] = psCurrEnumFeature[bucket]->psNext;
			return TRUE;
		}

		psCurrEnumFeature[bucket] = psCurrEnumFeature[bucket]->psNext;
	}
	// push NULL, none found;
	if (!stackPushResult(ST_FEATURE, (UDWORD)NULL))
	{
		return FALSE;
	}
	return TRUE;
}
*/

// -----------------------------------------------------------------------------------------
//Add a feature
BOOL scrAddFeature(void)
{
	FEATURE_STATS	*psStat;
	FEATURE			*psFeat = NULL;
	SDWORD			iX, iY, iMapX, iMapY, iTestX, iTestY, iFeat;

	if ( !stackPopParams(3, ST_FEATURESTAT, &iFeat,
		 VAL_INT, &iX, VAL_INT, &iY ) )
	{
		return FALSE;
	}

	psStat = (FEATURE_STATS *)(asFeatureStats + iFeat);

	ASSERT( PTRVALID(psStat, sizeof(FEATURE_STATS)),
			"scrAddFeature: Invalid feature pointer" );

	if ( psStat != NULL )
	{
		iMapX = iX >> TILE_SHIFT;
		iMapY = iY >> TILE_SHIFT;

		/* check for wrecked feature already on-tile and remove */
		for(psFeat = apsFeatureLists[0]; psFeat; psFeat = psFeat->psNext)
		{
			iTestX = psFeat->x >> TILE_SHIFT;
			iTestY = psFeat->y >> TILE_SHIFT;

			if ( (iTestX == iMapX) && (iTestY == iMapY) )
			{
				if ( psFeat->psStats->subType == FEAT_BUILD_WRECK )
				{
					/* remove feature */
					removeFeature( psFeat );
					break;
				}
				else
				{
					ASSERT( FALSE,
					"scrAddFeature: building feature on tile already occupied\n" );
				}
			}
		}

		psFeat = buildFeature( psStat, iX, iY, FALSE );
	}

	if (!stackPushResult((INTERP_TYPE)ST_FEATURE, (UDWORD)psFeat))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//Add a structure
BOOL scrAddStructure(void)
{
	STRUCTURE_STATS		*psStat;
	STRUCTURE			*psStruct = NULL;
	SDWORD				iX, iY, iMapX, iMapY;//, iWidth, iBreadth;
	SDWORD				iStruct, iPlayer;//, iW, iB;

	if ( !stackPopParams( 4, ST_STRUCTURESTAT, &iStruct, VAL_INT, &iPlayer,
							 VAL_INT, &iX, VAL_INT, &iY ) )
	{
		return FALSE;
	}

	psStat = (STRUCTURE_STATS *)(asStructureStats + iStruct);

	ASSERT( PTRVALID(psStat, sizeof(STRUCTURE_STATS)),
			"scrAddStructure: Invalid feature pointer" );

	if ( psStat != NULL )
	{
		/* offset coords so building centre at (iX, iY) */
/*		no longer necessary - buildStruct no longer uses top left
		iX -= psStat->baseWidth*TILE_UNITS/2;
		iY -= psStat->baseBreadth*TILE_UNITS/2;*/

		iMapX = iX >> TILE_SHIFT;
		iMapY = iY >> TILE_SHIFT;

		/* check for structure already on-tile */
		if(TILE_HAS_STRUCTURE(mapTile(iMapX,iMapY)))
		{
			ASSERT( FALSE,
			"scrAddStructure: tile already occupied by structure\n" );
		}

		psStruct = buildStructure( psStat, iX, iY, iPlayer, FALSE );
		if ( psStruct != NULL )
		{
			psStruct->status = SS_BUILT;
			buildingComplete(psStruct);

            /*
            Apart from this being wrong (iWidth = 0 when psStat->baseWidth = 1
            and you end up in an infinite loop) we don't need to do this here
            since the map is flattened as part of buildStructure

			iWidth   = psStat->baseWidth/2;
			iBreadth = psStat->baseBreadth/2;

			// flatten tiles across building base
			for ( iW=iMapX; iW<=iMapX+(SDWORD)psStat->baseWidth; iW+=iWidth )
			{
				for ( iB=iMapY; iB<=iMapY+(SDWORD)psStat->baseBreadth; iB+=iBreadth )
				{
					setTileHeight(iW, iB, psStruct->z);
				}
			}*/
		}
	}

	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (UDWORD)psStruct))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//Destroy a structure
BOOL scrDestroyStructure(void)
{
	STRUCTURE	*psStruct;

	if (!stackPopParams(1, ST_STRUCTURE, &psStruct))
	{
		return FALSE;
	}

	if (psStruct == NULL)
	{
		ASSERT( PTRVALID(psStruct, sizeof(STRUCTURE)),
			"scrDestroyStructure: Invalid structure pointer" );
	}

	removeStruct( psStruct, TRUE );

	return TRUE;
}



// -----------------------------------------------------------------------------------------
//NEXT 2 FUNCS ONLY USED IN MULTIPLAYER AS FAR AS I KNOW (25 AUG98) alexl.
// static vars to enum structs;
static	STRUCTURE_STATS	*psStructStatToFind;
static	UDWORD			playerToEnumStruct;
static	UDWORD			enumStructCount;
static	BOOL			structfindany;
static	SDWORD			playerVisibleStruct;		//player whose structures must be visible

// init enum visible structures.
BOOL scrInitEnumStruct(void)
{
	SDWORD			player,iStat,targetplayer,any;

	if ( !stackPopParams(4,VAL_BOOL,&any, ST_STRUCTURESTAT, &iStat,  VAL_INT, &player, VAL_INT, &targetplayer) )
	{
		return FALSE;
	}

	if(any == 1)
	{
		structfindany = TRUE;
	}
	else
	{
		structfindany = FALSE;
	}
	psStructStatToFind	= (STRUCTURE_STATS *)(asStructureStats + iStat);
	playerToEnumStruct	= (UDWORD)player;
	playerVisibleStruct = targetplayer;		//fix: remember who must be able to see the structure
	enumStructCount		= 0;
	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrEnumStruct(void)
{
	UDWORD		count;
	STRUCTURE	*psStruct;

	// go to the correct start point in the structure list.
	count = 0;
	for(psStruct=apsStructLists[playerToEnumStruct];psStruct && count<enumStructCount;count++)
	{
		psStruct = psStruct->psNext;
	}

	if(psStruct == NULL)		// no more to find.
	{
		if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (SDWORD)NULL))
		{
			return FALSE;
		}
		return TRUE;
	}

	while(psStruct)	// find a visible structure of required type.
	{
//		if(	(structfindany || (psStruct->pStructureType->type == psStructStatToFind->type))
		if(	(structfindany || (psStruct->pStructureType->ref == psStructStatToFind->ref))
			&&
			((playerVisibleStruct < 0) || (psStruct->visible[playerToEnumStruct]))	//fix: added playerVisibleStruct for visibility test
		   )
		{
			if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE,(UDWORD) psStruct))			//	push result
			{
				return FALSE;
			}
			enumStructCount++;
			return TRUE;
		}
		enumStructCount++;
		psStruct = psStruct->psNext;
	}
	// push NULL, none found;
	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (UDWORD)NULL))
	{
		return FALSE;
	}
	return TRUE;
}



// -----------------------------------------------------------------------------------------
/*looks to see if a structure (specified by type) exists and is being built*/
BOOL scrStructureBeingBuilt(void)
{
//	INTERP_VAL			sVal;
	UDWORD				structInc;
	STRUCTURE_STATS		*psStats;
	SDWORD				player;
	BOOL				beingBuilt;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &structInc, VAL_INT, &player))
	{
		return FALSE;
	}

/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_STRUCTURESTAT)
	{
		ASSERT( FALSE, "scrStructureBeingBuilt: type mismatch for object" );
		return FALSE;
	}
	psStats = (STRUCTURE_STATS *)(asStructureStats + sVal.v.ival);
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructureBeingBuilt:player number is too high" );
		return FALSE;
	}

	psStats = (STRUCTURE_STATS *)(asStructureStats + structInc);
	beingBuilt = FALSE;
	if (checkStructureStatus(psStats, player, SS_BEING_BUILT))
	{
		beingBuilt = TRUE;
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)beingBuilt))
	{
		return FALSE;
	}

	return TRUE;
}



// -----------------------------------------------------------------------------------------
// multiplayer skirmish only for now.
// returns TRUE if a specific struct is complete. I know it's like the previous func,
BOOL scrStructureComplete(void)
{
	STRUCTURE	*psStruct;
	BOOL		result;

	if (!stackPopParams(1, ST_STRUCTURE, &psStruct))
	{
		return FALSE;
	}
	if(psStruct->status == SS_BUILT)
	{
		result = TRUE;
	}
	else
	{
		result = FALSE;
	}
	if (!stackPushResult(VAL_BOOL, result))
	{
		return FALSE;
	}

	return TRUE;
}



// -----------------------------------------------------------------------------------------
/*looks to see if a structure (specified by type) exists and built*/
BOOL scrStructureBuilt(void)
{
//	INTERP_VAL			sVal;
	UDWORD				structInc;
	STRUCTURE_STATS		*psStats;
	SDWORD				player;
	BOOL				built;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &structInc, VAL_INT, &player))
	{
		return FALSE;
	}

/*	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	if (sVal.type != ST_STRUCTURESTAT)
	{
		ASSERT( FALSE, "scrStructureBuilt: type mismatch for object" );
		return FALSE;
	}
	psStats = (STRUCTURE_STATS *)(asStructureStats + sVal.v.ival);
*/
	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructureBuilt:player number is too high" );
		return FALSE;
	}

	psStats = (STRUCTURE_STATS *)(asStructureStats + structInc);

	built = FALSE;
	if (checkStructureStatus(psStats, player, SS_BUILT))
	{
		built = TRUE;
	}
	if (!stackPushResult(VAL_BOOL, (UDWORD)built))
	{
		return FALSE;
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
/*centre the view on an object - can be droid/structure or feature */
BOOL scrCentreView(void)
{
	BASE_OBJECT	*psObj;
//	INTERP_VAL	sVal;

	if (!stackPopParams(1, ST_BASEOBJECT, &psObj))
	{
		return FALSE;
	}

	if (psObj == NULL)
	{
		ASSERT( FALSE, "scrCentreView: NULL object" );
		return FALSE;
	}

	//centre the view on the objects x/y
	setViewPos(psObj->x >> TILE_SHIFT, psObj->y >> TILE_SHIFT,FALSE);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
/*centre the view on a position */
BOOL scrCentreViewPos(void)
{
	SDWORD		x,y;

	if (!stackPopParams(2, VAL_INT, &x, VAL_INT, &y))
	{
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth*TILE_UNITS) ||
		 (y < 0) || (y >= (SDWORD)mapHeight*TILE_UNITS))
	{
		ASSERT( FALSE, "scrCenterViewPos: coords off map" );
		return FALSE;
	}

	//centre the view on the objects x/y
	setViewPos(x >> TILE_SHIFT, y >> TILE_SHIFT,FALSE);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Get a pointer to a structure based on a stat - returns NULL if cannot find one
BOOL scrGetStructure(void)
{
	SDWORD				player, index;
	STRUCTURE			*psStruct;
	UDWORD				structType;
	BOOL				found;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &index, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetStructure:player number is too high" );
		return FALSE;
	}

	structType = asStructureStats[index].ref;

	//search the players' list of built structures to see if one exists
	found = FALSE;
	for (psStruct = apsStructLists[player]; psStruct != NULL; psStruct =
		psStruct->psNext)
	{
		if (psStruct->pStructureType->ref == structType)
		{
			found = TRUE;
			break;
		}
	}

	//make sure pass NULL back if not got one
	if (!found)
	{
		psStruct = NULL;
	}

	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (UDWORD)psStruct))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Get a pointer to a template based on a component stat - returns NULL if cannot find one
BOOL scrGetTemplate(void)
{
	SDWORD				player;
	DROID_TEMPLATE		*psTemplate;
	BOOL				found;
	INTERP_VAL			sVal;
	UDWORD				i;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetTemplate:player number is too high" );
		return FALSE;
	}

	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	//search the players' list of templates to see if one exists
	found = FALSE;
	for (psTemplate = apsDroidTemplates[player]; psTemplate != NULL; psTemplate =
		psTemplate->psNext)
	{
		switch( sVal.type)
		{
		case ST_BODY:
			if (psTemplate->asParts[COMP_BODY] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_PROPULSION:
			if (psTemplate->asParts[COMP_PROPULSION] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_ECM:
			if (psTemplate->asParts[COMP_ECM] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_SENSOR:
			if (psTemplate->asParts[COMP_SENSOR] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_CONSTRUCT:
			if (psTemplate->asParts[COMP_CONSTRUCT] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_REPAIR:
			if (psTemplate->asParts[COMP_REPAIRUNIT] == sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_WEAPON:
			for (i=0; i < DROID_MAXWEAPS; i++)
			{
				if (psTemplate->asWeaps[i] == (UDWORD)sVal.v.ival)
				{
					found = TRUE;
					break;
				}
			}
			break;
		default:
			ASSERT( FALSE, "scrGetTemplate: unknown type" );
			return FALSE;
		}

		if (found)
		{
			break;
		}
	}

	//make sure pass NULL back if not got one
	if (!found)
	{
		psTemplate = NULL;
	}

	if (!stackPushResult((INTERP_TYPE)ST_TEMPLATE, (UDWORD)psTemplate))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Get a pointer to a droid based on a component stat - returns NULL if cannot find one
BOOL scrGetDroid(void)
{
	SDWORD				player;
	DROID				*psDroid;
	BOOL				found;
	INTERP_VAL			sVal;
	UDWORD				i;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetUnit:player number is too high" );
		return FALSE;
	}

	if (!stackPop(&sVal))
	{
		return FALSE;
	}

	//search the players' list of droid to see if one exists
	found = FALSE;
	for (psDroid = apsDroidLists[player]; psDroid != NULL; psDroid =
		psDroid->psNext)
	{
		switch( sVal.type)
		{
		case ST_BODY:
			if (psDroid->asBits[COMP_BODY].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_PROPULSION:
			if (psDroid->asBits[COMP_PROPULSION].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_ECM:
			if (psDroid->asBits[COMP_ECM].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_SENSOR:
			if (psDroid->asBits[COMP_SENSOR].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_CONSTRUCT:
			if (psDroid->asBits[COMP_CONSTRUCT].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_REPAIR:
			if (psDroid->asBits[COMP_REPAIRUNIT].nStat == (UDWORD)sVal.v.ival)
			{
				found = TRUE;
			}
			break;
		case ST_WEAPON:
			for (i=0; i < DROID_MAXWEAPS; i++)
			{
				if (psDroid->asWeaps[i].nStat == (UDWORD)sVal.v.ival)
				{
					found = TRUE;
					break;
				}
			}
			break;
		default:
			ASSERT( FALSE, "scrGetUnit: unknown type" );
			return FALSE;
		}

		if (found)
		{
			break;
		}
	}

	//make sure pass NULL back if not got one
	if (!found)
	{
		psDroid = NULL;
	}

	if (!stackPushResult((INTERP_TYPE)ST_DROID, (UDWORD)psDroid))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets all the scroll params for the map
BOOL scrSetScrollParams(void)
{
	SDWORD				minX, minY, maxX, maxY;
    SDWORD              prevMinX, prevMinY, prevMaxX, prevMaxY;

	if (!stackPopParams(4, VAL_INT, &minX, VAL_INT, &minY, VAL_INT, &maxX, VAL_INT, &maxY))
	{
		return FALSE;
	}

	//check the values entered are valid
	if (minX < 0)
	{
		ASSERT( FALSE, "Minimum scroll x value %d is less than zero - ", minX );
		return FALSE;
	}
	if (minY < 0)
	{
		ASSERT( FALSE, "Minimum scroll y value %d is less than zero - ", minY );
	}
	if (maxX > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "Maximum scroll x value %d is greater than mapWidth - ", maxX );
	}
	if (maxX < (SDWORD)(visibleXTiles+1))
	{
		ASSERT( FALSE, "Maximum scroll x %d has to be bigger than Visible Width(22) - ", maxX );
	}
	if (maxY > (SDWORD)mapHeight)
	{
		ASSERT( FALSE, "Maximum scroll y value %d is greater than mapWidth - ", maxY );
	}
	if (maxY < (SDWORD)(visibleYTiles+1))
	{
		ASSERT( FALSE, "Maximum scroll y %d has to be bigger than Visible Height(22) - ", maxY );
	}

    prevMinX = scrollMinX;
    prevMinY = scrollMinY;
    prevMaxX = scrollMaxX;
    prevMaxY = scrollMaxY;

	scrollMinX = minX;
	scrollMaxX = maxX;
	scrollMinY = minY;
	scrollMaxY = maxY;

    //when the scroll limits change midgame - need to redo the lighting
    //initLighting(scrollMinX, scrollMinY, scrollMaxX, scrollMaxY);
    initLighting(prevMinX < scrollMinX ? prevMinX : scrollMinX,
        prevMinY < scrollMinY ? prevMinY : scrollMinY,
        prevMaxX < scrollMaxX ? prevMaxX : scrollMaxX,
        prevMaxY < scrollMaxY ? prevMaxY : scrollMaxY);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the scroll minX separately for the map
BOOL scrSetScrollMinX(void)
{
	SDWORD				minX, prevMinX;

	if (!stackPopParams(1, VAL_INT, &minX))
	{
		return FALSE;
	}

	//check the value entered are valid
	if (minX < 0)
	{
		ASSERT( FALSE, "Minimum scroll x value %d is less than zero - ", minX );
		return FALSE;
	}

    prevMinX = scrollMinX;

    scrollMinX = minX;

    //when the scroll limits change midgame - need to redo the lighting
    //initLighting(scrollMinX, scrollMinY, scrollMaxX, scrollMaxY);
    initLighting(prevMinX < scrollMinX ? prevMinX : scrollMinX,
        scrollMinY, scrollMaxX, scrollMaxY);

    return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the scroll minY separately for the map
BOOL scrSetScrollMinY(void)
{
	SDWORD				minY, prevMinY;

	if (!stackPopParams(1, VAL_INT, &minY))
	{
		return FALSE;
	}

	//check the value entered are valid
	if (minY < 0)
	{
		ASSERT( FALSE, "Minimum scroll y value %d is less than zero - ", minY );
		return FALSE;
	}

    prevMinY = scrollMinY;

	scrollMinY = minY;

    //when the scroll limits change midgame - need to redo the lighting
    //initLighting(scrollMinX, scrollMinY, scrollMaxX, scrollMaxY);
    initLighting(scrollMinX,
        prevMinY < scrollMinY ? prevMinY : scrollMinY,
        scrollMaxX, scrollMaxY);

    return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the scroll maxX separately for the map
BOOL scrSetScrollMaxX(void)
{
	SDWORD				maxX, prevMaxX;

	if (!stackPopParams(1, VAL_INT, &maxX))
	{
		return FALSE;
	}

	//check the value entered are valid
	if (maxX > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "Maximum scroll x value %d is greater than mapWidth - ", maxX );
		return FALSE;
	}

    prevMaxX = scrollMaxX;

	scrollMaxX = maxX;

    //when the scroll limits change midgame - need to redo the lighting
    //initLighting(scrollMinX, scrollMinY, scrollMaxX, scrollMaxY);
    initLighting(scrollMinX,  scrollMinY,
        prevMaxX < scrollMaxX ? prevMaxX : scrollMaxX,
        scrollMaxY);

    return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the scroll maxY separately for the map
BOOL scrSetScrollMaxY(void)
{
	SDWORD				maxY, prevMaxY;

	if (!stackPopParams(1, VAL_INT, &maxY))
	{
		return FALSE;
	}

	//check the value entered are valid
	if (maxY > (SDWORD)mapHeight)
	{
		ASSERT( FALSE, "Maximum scroll y value %d is greater than mapWidth - ", maxY );
		return FALSE;
	}

    prevMaxY = scrollMaxY;

	scrollMaxY = maxY;

    //when the scroll limits change midgame - need to redo the lighting
    //initLighting(scrollMinX, scrollMinY, scrollMaxX, scrollMaxY);
    initLighting(scrollMinX, scrollMinY, scrollMaxX,
        prevMaxY < scrollMaxY ? prevMaxY : scrollMaxY);

    return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets which sensor will be used as the default for a player
BOOL scrSetDefaultSensor(void)
{
	SDWORD				player;
	UDWORD				sensorInc;

	if (!stackPopParams(2, ST_SENSOR, &sensorInc, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetDefaultSensor:player number is too high" );
		return FALSE;
	}

	//check is a valid sensor Inc
	if (sensorInc > numSensorStats)
	{
		ASSERT( FALSE, "scrSetDefaultSensor: Sensor Inc is too high - %d", sensorInc );
		return FALSE;
	}

	//check that this sensor is a default sensor
	if (asSensorStats[sensorInc].location != LOC_DEFAULT)
	{

		ASSERT( FALSE, "scrSetDefaultSensor: This sensor is not a default one - %s",
			getStatName(&asSensorStats[sensorInc]) );
		return FALSE;
	}

	//assign since OK!
	aDefaultSensor[player] = sensorInc;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets which ECM will be used as the default for a player
BOOL scrSetDefaultECM(void)
{
	SDWORD				player;
	UDWORD				ecmInc;

	if (!stackPopParams(2, ST_ECM, &ecmInc, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetDefaultECM:player number is too high" );
		return FALSE;
	}

	//check is a valid ecmInc
	if (ecmInc > numECMStats)
	{
		ASSERT( FALSE, "scrSetDefaultECM: ECM Inc is too high - %d", ecmInc );
		return FALSE;
	}

	//check that this ecm is a default ecm
	if (asECMStats[ecmInc].location != LOC_DEFAULT)
	{
		ASSERT( FALSE, "scrSetDefaultECM: This ecm is not a default one - %s",
			getStatName(&asECMStats[ecmInc]) );
		return FALSE;
	}

	//assign since OK!
	aDefaultECM[player] = ecmInc;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets which RepairUnit will be used as the default for a player
BOOL scrSetDefaultRepair(void)
{
	SDWORD				player;
	UDWORD				repairInc;

	if (!stackPopParams(2, ST_REPAIR, &repairInc, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetDefaultRepair:player number is too high" );
		return FALSE;
	}

	//check is a valid repairInc
	if (repairInc > numRepairStats)
	{
		ASSERT( FALSE, "scrSetDefaultRepair: Repair Inc is too high - %d", repairInc );
		return FALSE;
	}

	//check that this repair is a default repair
	if (asRepairStats[repairInc].location != LOC_DEFAULT)
	{
		ASSERT( FALSE, "scrSetDefaultRepair: This repair is not a default one - %s",
			getStatName(&asRepairStats[repairInc]) );
		return FALSE;
	}

	//assign since OK!
	aDefaultRepair[player] = repairInc;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the structure limits for a player
BOOL scrSetStructureLimits(void)
{
	SDWORD				player, limit;
	UDWORD				structInc;
	STRUCTURE_LIMITS	*psStructLimits;

	if (!stackPopParams(3, ST_STRUCTURESTAT, &structInc, VAL_INT, &limit, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetStructureLimits:player number is too high" );
		return FALSE;
	}

	if (structInc > numStructureStats)
	{
		ASSERT( FALSE, "scrSetStructureLimits: Structure stat is too high - %d", structInc );
		return FALSE;
	}

	if (limit < 0)
	{
		ASSERT( FALSE, "scrSetStructureLimits: limit is less than zero - %d", limit );
		return FALSE;
	}

	if (limit > LOTS_OF)
	{
		ASSERT( FALSE, "scrSetStructureLimits: limit is too high - %d - must be less than %d",
			limit, LOTS_OF );
		return FALSE;
	}

	psStructLimits = asStructLimits[player];
	psStructLimits[structInc].limit = (UBYTE)limit;

	psStructLimits[structInc].globalLimit = (UBYTE)limit;

	return TRUE;
}



// -----------------------------------------------------------------------------------------
// multiplayer limit handler.
BOOL scrApplyLimitSet(void)
{
	applyLimitSet();
	return TRUE;
}



// -----------------------------------------------------------------------------------------
// plays a sound for the specified player - only plays the sound if the
// specified player = selectedPlayer
BOOL scrPlaySound(void)
{
	SDWORD	player, soundID;

	if (!stackPopParams(2, ST_SOUND, &soundID, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrPlaySound:player number is too high" );
		return FALSE;
	}

	if (player == (SDWORD)selectedPlayer)
	{
		audio_QueueTrack(soundID);
		if(bInTutorial)
		{
			audio_QueueTrack(ID_SOUND_OF_SILENCE);
		}
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
// plays a sound for the specified player - only plays the sound if the
// specified player = selectedPlayer - saves position
BOOL scrPlaySoundPos(void)
{
	SDWORD	player, soundID, iX, iY, iZ;

	if (!stackPopParams(5, ST_SOUND, &soundID, VAL_INT, &player,
							VAL_INT, &iX, VAL_INT, &iY, VAL_INT, &iZ))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrPlaySoundPos:player number is too high" );
		return FALSE;
	}

	if (player == (SDWORD)selectedPlayer)
	{
		audio_QueueTrackPos(soundID, iX, iY, iZ);
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------

/* add a text message to the top of the screen for the selected player*/
BOOL scrShowConsoleText(void)
{
	char				*pText;
	SDWORD				player;

	if (!stackPopParams(2, ST_TEXTSTRING, &pText, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddConsoleText:player number is too high" );
		return FALSE;
	}

	if (player == (SDWORD)selectedPlayer)
	{
		permitNewConsoleMessages(TRUE);
		addConsoleMessage(pText, CENTRE_JUSTIFY);
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
/* add a text message to the top of the screen for the selected player*/
BOOL scrAddConsoleText(void)
{
	char				*pText;
	SDWORD				player;

	if (!stackPopParams(2, ST_TEXTSTRING, &pText, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddConsoleText:player number is too high" );
		return FALSE;
	}

	if (player == (SDWORD)selectedPlayer)
	{
		permitNewConsoleMessages(TRUE);
		setConsolePermanence(TRUE,TRUE);
		addConsoleMessage(pText, CENTRE_JUSTIFY);
		permitNewConsoleMessages(FALSE);
	}

	return TRUE;
}



// -----------------------------------------------------------------------------------------
/* add a text message to the top of the screen for the selected player - without clearing whats there*/
BOOL scrTagConsoleText(void)
{
	char				*pText;
	SDWORD				player;

	if (!stackPopParams(2, ST_TEXTSTRING, &pText, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddConsoleText:player number is too high" );
		return FALSE;
	}

	if (player == (SDWORD)selectedPlayer)
	{
		permitNewConsoleMessages(TRUE);
		setConsolePermanence(TRUE,FALSE);
		addConsoleMessage(pText, CENTRE_JUSTIFY);
		permitNewConsoleMessages(FALSE);
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------

BOOL	scrClearConsole(void)
{
	flushConsoleMessages();
	return(TRUE);
}

// -----------------------------------------------------------------------------------------
//demo functions for turning the power on
BOOL scrTurnPowerOff(void)
{
	//powerCalculated = FALSE;
	powerCalc(FALSE);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//demo functions for turning the power off
BOOL scrTurnPowerOn(void)
{

	//powerCalculated = TRUE;
	powerCalc(TRUE);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//flags when the tutorial is over so that console messages can be turned on again
BOOL scrTutorialEnd(void)
{
	initConsoleMessages();
	return TRUE;
}

// -----------------------------------------------------------------------------------------
//function to play a full-screen video in the middle of the game for the selected player
BOOL scrPlayVideo(void)
{
	char				*pVideo, *pText;

	if (!stackPopParams(2, ST_TEXTSTRING, &pVideo, ST_TEXTSTRING, &pText))
	{
		return FALSE;
	}

		seq_ClearSeqList();
		seq_AddSeqToList(pVideo, NULL, pText, FALSE,0);		// Arpzzzzzzzzzzzzzzzlksht!
		seq_StartNextFullScreenVideo();

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//checks to see if there are any droids for the specified player
BOOL scrAnyDroidsLeft(void)
{
	SDWORD		player;
	BOOL		droidsLeft;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAnyUnitsLeft:player number is too high" );
		return FALSE;
	}

	//check the players list for any droid
	droidsLeft = TRUE;
	if (apsDroidLists[player] == NULL)
	{
		droidsLeft = FALSE;
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)droidsLeft))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//function to call when the game is over, plays a message then does game over stuff.
//
BOOL scrGameOverMessage(void)
{
	BOOL			gameOver;
	MESSAGE			*psMessage;
	SDWORD			msgType, player;
	VIEWDATA		*psViewData;
	//UDWORD			height;


	if (!stackPopParams(4, ST_INTMESSAGE, &psViewData , VAL_INT, &msgType,
				VAL_INT, &player, VAL_BOOL, &gameOver))
	{
		return FALSE;
	}


	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGameOverMessage:player number is too high" );
		return FALSE;
	}

	//create the message
	psMessage = addMessage(msgType, FALSE, player);

	ASSERT( msgType != MSG_PROXIMITY, "scrGameOverMessage: Bad message type (MSG_PROXIMITY)" );

	if (psMessage)
	{
		//we need to set this here so the VIDEO_QUIT callback is not called
		setScriptWinLoseVideo((UBYTE)(gameOver ? PLAY_WIN : PLAY_LOSE));

		//set the data
		psMessage->pViewData = (MSG_VIEWDATA *)psViewData;
		displayImmediateMessage(psMessage);
		stopReticuleButtonFlash(IDRET_INTEL_MAP);

        // Can't do this cos won't process windows stuff
        // Wait for the video to finish.
		/*while (loop_GetVideoStatus())
		{
			videoLoop();
		}*/
	}

    //this now called when the video Quit is processed
	//displayGameOver(gameOver);

	return TRUE;
}




// -----------------------------------------------------------------------------------------
//function to call when the game is over
BOOL scrGameOver(void)
{
	BOOL	gameOver;

	if (!stackPopParams(1, VAL_BOOL, &gameOver))
	{
		return FALSE;
	}

    /*this function will only be called with gameOver = TRUE when at the end of
    the game so we'll just hard-code what happens!*/

    //don't want this in multiplayer...
    if (!bMultiPlayer)

    {
        if (gameOver == TRUE AND !bInTutorial)
        {
            //we need to set this here so the VIDEO_QUIT callback is not called
		    setScriptWinLoseVideo(PLAY_WIN);

    	    seq_ClearSeqList();

	        seq_AddSeqToList("outro.rpl",NULL,"outro.txa", FALSE,0);
	        seq_StartNextFullScreenVideo();

        }
    }

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrAnyFactoriesLeft(void)
{
	SDWORD		player;
	BOOL		result;
	STRUCTURE	*psCurr;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAnyFactorysLeft:player number is too high" );
		return FALSE;
	}

	//check the players list for any structures
	result = FALSE;
	if(apsStructLists[player])
	{
		for (psCurr = apsStructLists[player]; psCurr != NULL; psCurr = psCurr->psNext)
		{
//			if (psCurr->pStructureType->type	== REF_FACTORY OR
//				psCurr->pStructureType->type == REF_CYBORG_FACTORY OR
//				psCurr->pStructureType->type == REF_VTOL_FACTORY )
			if(StructIsFactory(psCurr))
			{
				result = TRUE;
				break;
			}
		}
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)result))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
//checks to see if there are any structures (except walls) for the specified player
BOOL scrAnyStructButWallsLeft(void)
{
	SDWORD		player;
	BOOL		structuresLeft;
	STRUCTURE	*psCurr;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAnyStructuresButWallsLeft:player number is too high" );
		return FALSE;
	}

	//check the players list for any structures
	structuresLeft = TRUE;
	if (apsStructLists[player] == NULL)
	{
		structuresLeft = FALSE;
	}
	else
	{
		structuresLeft = FALSE;
		for (psCurr = apsStructLists[player]; psCurr != NULL; psCurr = psCurr->psNext)
		{
			if (psCurr->pStructureType->type != REF_WALL AND psCurr->pStructureType->
				type != REF_WALLCORNER)
			{
				structuresLeft = TRUE;
				break;
			}
		}
	}

	if (!stackPushResult(VAL_BOOL, (UDWORD)structuresLeft))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//defines the background audio to play
BOOL scrPlayBackgroundAudio(void)
{
	char	*pText;
	SDWORD	iVol;

	if (!stackPopParams(2, ST_TEXTSTRING, &pText, VAL_INT, &iVol))
	{
		return FALSE;
	}


	cdspan_PlayInGameAudio(pText, iVol);


	return TRUE;

}

// -----------------------------------------------------------------------------------------
//defines the CD audio to play
BOOL scrPlayCDAudio(void)
{
	SDWORD	iTrack;

	if (!stackPopParams(1, VAL_INT, &iTrack))
	{
		return FALSE;
	}



	if (war_GetPlayAudioCDs()) {
		cdAudio_PlayTrack( iTrack );
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrStopCDAudio(void)
{
	if (war_GetPlayAudioCDs()) {
		cdAudio_Stop();
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrPauseCDAudio(void)
{
	if (war_GetPlayAudioCDs()) {
		cdAudio_Pause();
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrResumeCDAudio(void)
{
	if (war_GetPlayAudioCDs()) {
		cdAudio_Resume();
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat point for a player
BOOL scrSetRetreatPoint(void)
{
	SDWORD	player, x,y;

	if (!stackPopParams(3, VAL_INT, &player, VAL_INT, &x, VAL_INT, &y))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetRetreatPoint: player out of range" );
		return FALSE;
	}
	if (x < 0 || x >= (SDWORD)mapWidth*TILE_UNITS ||
		y < 0 || y >= (SDWORD)mapHeight*TILE_UNITS)
	{
		ASSERT( FALSE, "scrSetRetreatPoint: coords off map" );
		return FALSE;
	}

	asRunData[player].sPos.x = x;
	asRunData[player].sPos.y = y;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat force level
BOOL scrSetRetreatForce(void)
{
	SDWORD	player, level, numDroids;
	DROID	*psCurr;

	if (!stackPopParams(2, VAL_INT, &player, VAL_INT, &level))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetRetreatForce: player out of range" );
		return FALSE;
	}

	if (level > 100 || level < 0)
	{
		ASSERT( FALSE, "scrSetRetreatForce: level out of range" );
		return FALSE;
	}

	// count up the current number of droids
	numDroids = 0;
	for(psCurr = apsDroidLists[player]; psCurr; psCurr=psCurr->psNext)
	{
		numDroids += 1;
	}

	asRunData[player].forceLevel = (UBYTE)(level * numDroids / 100);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat leadership
BOOL scrSetRetreatLeadership(void)
{
	SDWORD	player, level;

	if (!stackPopParams(2, VAL_INT, &player, VAL_INT, &level))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetRetreatLeadership: player out of range" );
		return FALSE;
	}

	if (level > 100 || level < 0)
	{
		ASSERT( FALSE, "scrSetRetreatLeadership: level out of range" );
		return FALSE;
	}

	asRunData[player].leadership = (UBYTE)level;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat point for a group
BOOL scrSetGroupRetreatPoint(void)
{
	SDWORD		x,y;
	DROID_GROUP	*psGroup;

	if (!stackPopParams(3, ST_GROUP, &psGroup, VAL_INT, &x, VAL_INT, &y))
	{
		return FALSE;
	}

	if (x < 0 || x >= (SDWORD)mapWidth*TILE_UNITS ||
		y < 0 || y >= (SDWORD)mapHeight*TILE_UNITS)
	{
		ASSERT( FALSE, "scrSetRetreatPoint: coords off map" );
		return FALSE;
	}

	psGroup->sRunData.sPos.x = x;
	psGroup->sRunData.sPos.y = y;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrSetGroupRetreatForce(void)
{
	SDWORD		level, numDroids;
	DROID_GROUP	*psGroup;
	DROID		*psCurr;

	if (!stackPopParams(2, ST_GROUP, &psGroup, VAL_INT, &level))
	{
		return FALSE;
	}

	if (level > 100 || level < 0)
	{
		ASSERT( FALSE, "scrSetRetreatForce: level out of range" );
		return FALSE;
	}

	// count up the current number of droids
	numDroids = 0;
	for(psCurr = psGroup->psList; psCurr; psCurr=psCurr->psGrpNext)
	{
		numDroids += 1;
	}

	psGroup->sRunData.forceLevel = (UBYTE)(level * numDroids / 100);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat health level
BOOL scrSetRetreatHealth(void)
{
	SDWORD	player, health;

	if (!stackPopParams(2, VAL_INT, &player, VAL_INT, &health))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetHealthForce: player out of range" );
		return FALSE;
	}

	if (health > 100 || health < 0)
	{
		ASSERT( FALSE, "scrSetHealthForce: health out of range" );
		return FALSE;
	}

	asRunData[player].healthLevel = (UBYTE)health;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrSetGroupRetreatHealth(void)
{
	SDWORD		health;
	DROID_GROUP	*psGroup;

	if (!stackPopParams(2, ST_GROUP, &psGroup, VAL_INT, &health))
	{
		return FALSE;
	}

	if (health > 100 || health < 0)
	{
		ASSERT( FALSE, "scrSetGroupRetreatHealth: health out of range" );
		return FALSE;
	}

	psGroup->sRunData.healthLevel = (UBYTE)health;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// set the retreat leadership
BOOL scrSetGroupRetreatLeadership(void)
{
	SDWORD		level;
	DROID_GROUP	*psGroup;

	if (!stackPopParams(2, ST_GROUP, &psGroup, VAL_INT, &level))
	{
		return FALSE;
	}

	if (level > 100 || level < 0)
	{
		ASSERT( FALSE, "scrSetRetreatLeadership: level out of range" );
		return FALSE;
	}

	psGroup->sRunData.leadership = (UBYTE)level;

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//start a Mission - the missionType is ignored now - gets it from the level data ***********
BOOL scrStartMission(void)
{
	char				*pGame;
	SDWORD				missionType;
	LEVEL_DATASET		*psNewLevel;

	if (!stackPopParams(2, VAL_INT, &missionType, ST_LEVEL, &pGame))
	{
		return FALSE;
	}

	//if (missionType > MISSION_NONE)
	if (missionType > LDS_NONE)
	{
		ASSERT( FALSE, "Invalid Mission Type" );
		return FALSE;
	}

	// check the last mission got finished
	/*if (mission.type != MISSION_NONE)
	{
		DBMB(("scrStartMission: old mission incomplete\n   ending mission with success"));
		endMission(TRUE);
	}*/

	// tell the loop that a new level has to be loaded up - not yet!
	//loopNewLevel = TRUE;
	strcpy(pLevelName, pGame);

	// find the level dataset
	if (!levFindDataSet(pGame, &psNewLevel))
	{
		debug( LOG_ERROR, "scrStartMission: couldn't find level data" );
		abort();
		return FALSE;
	}

	//set the mission rolling...
	//nextMissionType = missionType;
	nextMissionType = psNewLevel->type;
//	loopMissionState = LMS_SETUPMISSION;
	loopMissionState = LMS_CLEAROBJECTS;

/*	if (!setUpMission(missionType))
	{
		ASSERT( FALSE, "Unable to start mission - %s", pGame );
		return FALSE;
	}*/

	return TRUE;
}

//end a mission - NO LONGER CALLED FROM SCRIPT
/*BOOL scrEndMission(void)
{
	BOOL	status;

	if (!stackPopParams(1, VAL_BOOL, &status))
	{
		return FALSE;
	}

	endMission(status);
	return TRUE;
}*/
// -----------------------------------------------------------------------------------------
//set Snow (enable disable snow)
BOOL scrSetSnow(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}


	if(bState)
	{
		atmosSetWeatherType(WT_SNOWING);
	}
	else
	{
		atmosSetWeatherType(WT_NONE);
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set Rain (enable disable Rain)
BOOL scrSetRain(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}


	if(bState)
	{
		atmosSetWeatherType(WT_RAINING);
	}
	else
	{
		atmosSetWeatherType(WT_NONE);
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set Background Fog (replace fade out with fog)
BOOL scrSetBackgroundFog(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}

	//jps 17 feb 99 just set the status let other code worry about fogEnable/reveal
	if (bState)//true, so go to false
	{
		//restart fog if it was off
		if ((fogStatus == 0) && war_GetFog() && !(bMultiPlayer && game.fog))
		{
			pie_EnableFog(TRUE);
		}
		fogStatus |= FOG_BACKGROUND;//set lowest bit of 3
	}
	else
	{
		fogStatus &= FOG_FLAGS-FOG_BACKGROUND;//clear middle bit of 3
		//disable fog if it longer used
		if (fogStatus == 0)
		{
			pie_SetFogStatus(FALSE);
			pie_EnableFog(FALSE);
		}
	}

/* jps 17 feb 99
	if(getRevealStatus())		// fog'o war enabled
	{
		pie_SetFogStatus(FALSE);
		pie_EnableFog(FALSE);
//		fogStatus = 0;
		return TRUE;
	}

	if (bState)//true, so go to false
	{
		if (war_GetFog())
		{
			//restart fog if it was off
			if (fogStatus == 0)
			{
				pie_EnableFog(TRUE);
			}
			fogStatus |= FOG_BACKGROUND;//set lowest bit of 3
		}
	}
	else
	{
		if (war_GetFog())
		{
			fogStatus &= FOG_FLAGS-FOG_BACKGROUND;//clear middle bit of 3
			//disable fog if it longer used
			if (fogStatus == 0)
			{
				pie_SetFogStatus(FALSE);
				pie_EnableFog(FALSE);
			}
		}
	}
*/

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set Depth Fog (gradual fog from mid range to edge of world)
BOOL scrSetDepthFog(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}
	// ffs am
//jps 17 feb 99 just set the status let other code worry about fogEnable/reveal
	if (bState)//true, so go to false
	{
		//restart fog if it was off
		if ((fogStatus == 0) && war_GetFog() )
		{
			pie_EnableFog(TRUE);
		}
		fogStatus |= FOG_DISTANCE;//set lowest bit of 3
	}
	else
	{
		fogStatus &= FOG_FLAGS-FOG_DISTANCE;//clear middle bit of 3
		//disable fog if it longer used
		if (fogStatus == 0)
		{
			pie_SetFogStatus(FALSE);
			pie_EnableFog(FALSE);
		}
	}

/* jps 17 feb 99	if(getRevealStatus())		// fog'o war enabled
	{
		pie_SetFogStatus(FALSE);
		pie_EnableFog(FALSE);
//		fogStatus = 0;
		return TRUE;
	}

	if (bState)//true, so go to false
	{
		if (war_GetFog())
		{
			//restart fog if it was off
			if (fogStatus == 0)
			{
				pie_EnableFog(TRUE);
			}
			fogStatus |= FOG_DISTANCE;//set lowest bit of 3
		}
	}
	else
	{
		if (war_GetFog())
		{
			fogStatus &= FOG_FLAGS-FOG_DISTANCE;//clear middle bit of 3
			//disable fog if it longer used
			if (fogStatus == 0)
			{
				pie_SetFogStatus(FALSE);
				pie_EnableFog(FALSE);
			}
		}
	}
*/

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set Mission Fog colour, may be modified by weather effects
BOOL scrSetFogColour(void)
{
	SDWORD	red,green,blue;
	SDWORD	scrFogColour;

	if (!stackPopParams(3, VAL_INT, &red, VAL_INT, &green, VAL_INT, &blue))
	{
		return FALSE;
	}


//	if (pie_GetRenderEngine() == ENGINE_GLIDE)
//	{
		red &= 0xff;
		green &= 0xff;
		blue &= 0xff;
		scrFogColour = ((red << 16) + (green << 8) + blue);
		pie_SetFogColour(scrFogColour);
//	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// test function to test variable references
BOOL scrRefTest(void)
{
	SDWORD		Num = 0;

	if (!stackPopParams(1,VAL_INT, Num));
	{
		return FALSE;
	}

	debug( LOG_NEVER, "scrRefTest: num: %d \n", Num );

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// is player a human or computer player? (multiplayer only)

BOOL scrIsHumanPlayer(void)
{
	SDWORD	player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL,isHumanPlayer(player) ))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Set an alliance between two players
BOOL scrCreateAlliance(void)
{
	SDWORD	player1,player2;

	if (!stackPopParams(2, VAL_INT, &player1, VAL_INT, &player2))
	{
		return FALSE;
	}

	if (player1 < 0 || player1 >= MAX_PLAYERS ||
		player2 < 0 || player2 >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrCreateAlliance: player out of range" );
		return FALSE;
	}

	if(bMultiPlayer)
	{
		if(game.alliance==NO_ALLIANCES || game.alliance==ALLIANCES_TEAMS
			|| player1 >= game.maxPlayers || player2>=game.maxPlayers)
		{
			return TRUE;
		}
	}

	formAlliance((UBYTE)player1, (UBYTE)player2,TRUE,FALSE,TRUE);

/*
#ifndef PSX
	if(bMultiPlayer)
	{

		if(game.alliance==NO_ALLIANCES || player1 >= game.maxPlayers || player2>=game.maxPlayers)
		{
			return TRUE;
		}

		if(alliances[player1][player2] != ALLIANCE_FORMED)
		{
#ifdef DEBUG
			CONPRINTF(ConsoleString,(ConsoleString,"%d and %d form an alliance.",player1,player2));
#endif
			sendAlliance((UBYTE)player1,(UBYTE)player2,ALLIANCE_FORMED,0);
		}
	}
#endif

	alliances[player1][player2] = ALLIANCE_FORMED;
	alliances[player2][player1] = ALLIANCE_FORMED;
*/
	return TRUE;
}



// -----------------------------------------------------------------------------------------
// offer an alliance
BOOL scrOfferAlliance(void)
{
	SDWORD	player1,player2;
	if (!stackPopParams(2, VAL_INT, &player1, VAL_INT, &player2))
	{
		return FALSE;
	}
	if (game.alliance==NO_ALLIANCES || game.alliance==ALLIANCES_TEAMS ||
		player1 < 0 || player1 >= MAX_PLAYERS ||
		player2 < 0 || player2 >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrCreateAlliance: player out of range" );
		return FALSE;
	}


	requestAlliance((UBYTE)player1,(UBYTE)player2,TRUE,TRUE);
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Break an alliance between two players
BOOL scrBreakAlliance(void)
{
	SDWORD	player1,player2;

	if (!stackPopParams(2, VAL_INT, &player1, VAL_INT, &player2))
	{
		return FALSE;
	}

	if (
		player1 < 0 || player1 >= MAX_PLAYERS ||
		player2 < 0 || player2 >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrCreateAlliance: player out of range" );
		return FALSE;
	}
/*
if(bMultiPlayer)
	{


		if(alliances[player1][player2] != ALLIANCE_BROKEN)
		{
			CONPRINTF(ConsoleString,(ConsoleString,"%d and %d break alliance.",player1,player2));
			sendAlliance((UBYTE)player1,(UBYTE)player2,ALLIANCE_BROKEN,0);
		}
}
*/


	if(bMultiPlayer)
	{
		if(game.alliance==NO_ALLIANCES || game.alliance==ALLIANCES_TEAMS
			|| player1 >= game.maxPlayers || player2>=game.maxPlayers)
		{
			return TRUE;
		}
		breakAlliance(player1,player2,TRUE,TRUE);
	}
	else
	{
		breakAlliance(player1,player2,FALSE,TRUE);
	}
/*
	alliances[player1][player2] = ALLIANCE_BROKEN;
	alliances[player2][player1] = ALLIANCE_BROKEN;
*/
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Multiplayer relevant scriptfuncs
// returns true if 2 or more players are in alliance.
BOOL scrAllianceExists(void)
{

	UDWORD i,j;
	for(i=0;i<MAX_PLAYERS;i++)
	{
		for(j=0;j<MAX_PLAYERS;j++)
		{
			if(alliances[i][j] == ALLIANCE_FORMED)
			{
				if (!stackPushResult(VAL_BOOL, TRUE))
				{
					return FALSE;
				}
				return TRUE;
			}
		}
	}



	if (!stackPushResult(VAL_BOOL, FALSE))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrAllianceExistsBetween(void)
{
	UDWORD i,j;


	if (!stackPopParams(2, VAL_INT, &i,VAL_INT, &j))
	{
		return FALSE;
	}
	if(alliances[i][j] == ALLIANCE_FORMED)
	{
		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			return FALSE;
		}
		return TRUE;
	}

	if (!stackPushResult(VAL_BOOL, FALSE))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
BOOL scrPlayerInAlliance(void)
{
	UDWORD player,j;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	for(j=0;j<MAX_PLAYERS;j++)
	{
		if(alliances[player][j] == ALLIANCE_FORMED)
		{
			if (!stackPushResult(VAL_BOOL, TRUE))
			{
				return FALSE;
			}
			return TRUE;
		}
	}
	if (!stackPushResult(VAL_BOOL, FALSE))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// returns true if a single alliance is dominant.
BOOL scrDominatingAlliance(void)
{
	UDWORD i,j;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		for(j=0;j<MAX_PLAYERS;j++)
		{
			if(   isHumanPlayer(j)
			   && isHumanPlayer(i)
			   && i != j
			   && alliances[i][j] != ALLIANCE_FORMED)
			{
				if (!stackPushResult(VAL_BOOL, FALSE))
				{
					return FALSE;
				}
				return TRUE;
			}
		}
// -----------------------------------------------------------------------------------------
	}


	if (!stackPushResult(VAL_BOOL, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}


BOOL scrMyResponsibility(void)
{
	SDWORD player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if(	myResponsibility(player) )
	{
		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}
	}


	return TRUE;
}

// -----------------------------------------------------------------------------------------
/*checks to see if a structure of the type specified exists within the
specified range of an XY location */
BOOL scrStructureBuiltInRange(void)
{
	SDWORD		player, index, x, y, range;
	SDWORD		rangeSquared;
	STRUCTURE	*psCurr;
	BOOL		found;
	SDWORD		xdiff, ydiff;
	STRUCTURE_STATS *psTarget;

	if (!stackPopParams(5, ST_STRUCTURESTAT, &index, VAL_INT, &x, VAL_INT, &y,
		VAL_INT, &range, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange:player number is too high" );
		return FALSE;
	}

	if (x < (SDWORD)0 OR (x >> TILE_SHIFT) > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : invalid X coord" );
		return FALSE;
	}
	if (y < (SDWORD)0 OR (y >> TILE_SHIFT) > (SDWORD)mapHeight)
	{
		ASSERT( FALSE,"scrStructureBuiltInRange : invalid Y coord" );
		return FALSE;
	}
	if (index < (SDWORD)0 OR index > (SDWORD)numStructureStats)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : Invalid structure stat" );
		return FALSE;
	}
	if (range < (SDWORD)0)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : Rnage is less than zero" );
		return FALSE;
	}

	//now look through the players list of structures to see if this type
	//exists within range
	psTarget = &asStructureStats[index];
	rangeSquared = range * range;
	found = FALSE;
	for(psCurr = apsStructLists[player]; psCurr; psCurr = psCurr->psNext)
	{
		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff <= rangeSquared)
		{

#ifdef HASH_NAMES
			if( psCurr->pStructureType->NameHash == psTarget->NameHash )
#else
			if( strcmp(psCurr->pStructureType->pName,psTarget->pName) == 0 )
#endif
			{
				if (psCurr->status == SS_BUILT)
				{
					found = TRUE;
					break;
				}
			}
		}
	}
	//make sure pass NULL back if not got one
	if (!found)
	{
		psCurr = NULL;
	}

	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (UDWORD)psCurr))
	{
		return FALSE;
	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// generate a random number
BOOL scrRandom(void)
{
	SDWORD		range, result;

	if (!stackPopParams(1, VAL_INT, &range))
	{
		return FALSE;
	}

	if (range == 0)
	{
		result = 0;
	}
	else if (range > 0)
	{
		result = rand() % range;
	}
	else
	{
		result = rand() % (-range);
	}

	if (!stackPushResult(VAL_INT, result))
	{
		return FALSE;
	}

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// randomise the random number seed
BOOL scrRandomiseSeed(void)
{
	srand((UDWORD)clock());

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//explicitly enables a research topic
BOOL scrEnableResearch(void)
{
	SDWORD		player;
	RESEARCH	*psResearch;

	if (!stackPopParams(2, ST_RESEARCH, &psResearch, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrEnableResearch:player number is too high" );
		return FALSE;
	}

	if (!enableResearch(psResearch, player))
	{
		return FALSE;
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
//acts as if the research topic was completed - used to jump into the tree
BOOL scrCompleteResearch(void)
{
	SDWORD		player;
	RESEARCH	*psResearch;
	UDWORD		researchIndex;

	if (!stackPopParams(2, ST_RESEARCH, &psResearch, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrCompleteResearch:player number is too high" );
		return FALSE;
	}


	if(psResearch == NULL)
	{
		ASSERT( FALSE, "scrCompleteResearch: no such research topic" );
		return FALSE;
	}

	researchIndex = psResearch - asResearch;	//TODO: fix if needed
	if (researchIndex > numResearch)
	{
		ASSERT( FALSE, "scrCompleteResearch: invalid research index" );
		return FALSE;
	}

	researchResult(researchIndex, (UBYTE)player, FALSE);


	if(bMultiPlayer && (gameTime > 2 ))
	{
		SendResearch((UBYTE)player,researchIndex );
	}


	return TRUE;
}

// -----------------------------------------------------------------------------------------
// This routine used to start just a reticule button flashing
//   .. now it starts any button flashing (awaiting implmentation from widget library)
BOOL scrFlashOn(void)
{
	SDWORD		button;

	if (!stackPopParams(1, VAL_INT, &button))
	{
		return FALSE;
	}

	// For the time being ... we will perform the old code for the reticule ...
	if (button >= IDRET_OPTIONS && button <= IDRET_CANCEL)
	{
		flashReticuleButton((UDWORD)button);
		return TRUE;
	}


	if(widgGetFromID(psWScreen,button) != NULL)
	{
		widgSetButtonFlash(psWScreen,button);
	}
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// stop a generic button flashing
BOOL scrFlashOff(void)
{
	SDWORD		button;

	if (!stackPopParams(1, VAL_INT, &button))
	{
		return FALSE;
	}

	if (button >= IDRET_OPTIONS && button <= IDRET_CANCEL)
	{
		stopReticuleButtonFlash((UDWORD)button);
		return TRUE;
	}


	if(widgGetFromID(psWScreen,button) != NULL)
	{
		widgClearButtonFlash(psWScreen,button);
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set the initial power level settings for a player
BOOL scrSetPowerLevel(void)
{
	SDWORD		player, power;

	if (!stackPopParams(2, VAL_INT, &power, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetPowerLevel:player number is too high" );
		return FALSE;
	}

	setPlayerPower(power, player);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//add some power for a player
BOOL scrAddPower(void)
{
	SDWORD		player, power;

	if (!stackPopParams(2, VAL_INT, &power, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddPower:player number is too high" );
		return FALSE;
	}

	addPower(player, power);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
/*set the landing Zone position for the map - this is for player 0. Can be
scrapped and replaced by setNoGoAreas, left in for compatibility*/
BOOL scrSetLandingZone(void)
{
	SDWORD		x1, x2, y1, y2;

	if (!stackPopParams(4, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	//check the values - check against max possible since can set in one mission for the next
	//if (x1 > (SDWORD)mapWidth)
	if (x1 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetLandingZone: x1 is greater than max mapWidth" );
		return FALSE;
	}
	//if (x2 > (SDWORD)mapWidth)
	if (x2 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetLandingZone: x2 is greater than max mapWidth" );
		return FALSE;
	}
	//if (y1 > (SDWORD)mapHeight)
	if (y1 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetLandingZone: y1 is greater than max mapHeight" );
		return FALSE;
	}
	//if (y2 > (SDWORD)mapHeight)
	if (y2 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetLandingZone: y2 is greater than max mapHeight" );
		return FALSE;
	}
	//check won't overflow!
	if (x1 > UBYTE_MAX OR y1 > UBYTE_MAX OR x2 > UBYTE_MAX OR y2 > UBYTE_MAX)
	{
		ASSERT( FALSE, "scrSetLandingZone: one coord is greater than %d", UBYTE_MAX );
		return FALSE;
	}

	setLandingZone((UBYTE)x1, (UBYTE)y1, (UBYTE)x2, (UBYTE)y2);

	return TRUE;
}

/*set the landing Zone position for the Limbo droids and adds the Limbo droids
to the world at the location*/
BOOL scrSetLimboLanding(void)
{
	SDWORD		x1, x2, y1, y2;

	if (!stackPopParams(4, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	//check the values - check against max possible since can set in one mission for the next
	//if (x1 > (SDWORD)mapWidth)
	if (x1 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetLimboLanding: x1 is greater than max mapWidth" );
		return FALSE;
	}
	//if (x2 > (SDWORD)mapWidth)
	if (x2 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetLimboLanding: x2 is greater than max mapWidth" );
		return FALSE;
	}
	//if (y1 > (SDWORD)mapHeight)
	if (y1 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetLimboLanding: y1 is greater than max mapHeight" );
		return FALSE;
	}
	//if (y2 > (SDWORD)mapHeight)
	if (y2 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetLimboLanding: y2 is greater than max mapHeight" );
		return FALSE;
	}
	//check won't overflow!
	if (x1 > UBYTE_MAX OR y1 > UBYTE_MAX OR x2 > UBYTE_MAX OR y2 > UBYTE_MAX)
	{
		ASSERT( FALSE, "scrSetLimboLanding: one coord is greater than %d", UBYTE_MAX );
		return FALSE;
	}

	setNoGoArea((UBYTE)x1, (UBYTE)y1, (UBYTE)x2, (UBYTE)y2, LIMBO_LANDING);

    //this calls the Droids from the Limbo list onto the map
    placeLimboDroids();

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//initialises all the no go areas
BOOL scrInitAllNoGoAreas(void)
{
	initNoGoAreas();

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set a no go area for the map - landing zones for the enemy, or player 0
BOOL scrSetNoGoArea(void)
{
	SDWORD		x1, x2, y1, y2, area;

	if (!stackPopParams(5, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2,
		VAL_INT, &area))
	{
		return FALSE;
	}

    if (area == LIMBO_LANDING)
    {
        ASSERT( FALSE, "scrSetNoGoArea: Cannot set the Limbo Landing area with this function" );
        return FALSE;
    }

	//check the values - check against max possible since can set in one mission for the next
	//if (x1 > (SDWORD)mapWidth)
	if (x1 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetNoGoArea: x1 is greater than max mapWidth" );
		return FALSE;
	}
	//if (x2 > (SDWORD)mapWidth)
	if (x2 > (SDWORD)MAP_MAXWIDTH)
	{
		ASSERT( FALSE, "scrSetNoGoArea: x2 is greater than max mapWidth" );
		return FALSE;
	}
	//if (y1 > (SDWORD)mapHeight)
	if (y1 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetNoGoArea: y1 is greater than max mapHeight" );
		return FALSE;
	}
	//if (y2 > (SDWORD)mapHeight)
	if (y2 > (SDWORD)MAP_MAXHEIGHT)
	{
		ASSERT( FALSE, "scrSetNoGoArea: y2 is greater than max mapHeight" );
		return FALSE;
	}
	//check won't overflow!
	if (x1 > UBYTE_MAX OR y1 > UBYTE_MAX OR x2 > UBYTE_MAX OR y2 > UBYTE_MAX)
	{
		ASSERT( FALSE, "scrSetNoGoArea: one coord is greater than %d", UBYTE_MAX );
		return FALSE;
	}

	if (area >= MAX_NOGO_AREAS)
	{
		ASSERT( FALSE, "scrSetNoGoArea: max num of areas is %d", MAX_NOGO_AREAS );
		return FALSE;
	}

	setNoGoArea((UBYTE)x1, (UBYTE)y1, (UBYTE)x2, (UBYTE)y2, (UBYTE)area);

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// set the zoom level for the radar
BOOL scrSetRadarZoom(void)
{
	SDWORD	level;

	if (!stackPopParams(1, VAL_INT, &level))
	{
		return TRUE;
	}

	// MAX_RADARZOOM is different on PC and PSX
	if (level < 0 || level > 2)
	{
		ASSERT( FALSE, "scrSetRadarZoom: zoom level out of range" );
		return FALSE;
	}



	SetRadarZoom((UWORD)level);

	return TRUE;
}

// -----------------------------------------------------------------------------------------
//set how long an offworld mission can last -1 = no limit
BOOL scrSetMissionTime(void)
{
	SDWORD		time;

	if (!stackPopParams(1, VAL_INT, &time))
	{
		return FALSE;
	}

	time *= 100;

	//check not more than one hour - the mission timers cannot cope at present! - (visually)
	//if (time > 60*60*GAME_TICKS_PER_SEC)
	//check not more than 99 mins - the mission timers cannot cope at present! - (visually)
    //we're allowing up to 5 hours now!
    if (time > 5*60*60*GAME_TICKS_PER_SEC)
	{
		ASSERT( FALSE,"The mission timer cannot be set to more than 99!" );
		time = -1;
	}
	//store the value
	mission.time = time;
		// ffs ab    ... but shouldn't this be on the psx ?
    setMissionCountDown();


	//add the timer to the interface
	if (mission.time >= 0)
	{
		mission.startTime = gameTime;
		addMissionTimerInterface();
	}
    else
    {
        //make sure its not up if setting to -1
        intRemoveMissionTimer();
        //make sure the cheat time is not set
        mission.cheatTime = 0;
    }

	return TRUE;
}

// this returns how long is left for the current mission time is 1/100th sec - same units as passed in
BOOL scrMissionTimeRemaining(void)
{
    SDWORD      timeRemaining;

	timeRemaining = mission.time - (gameTime - mission.startTime);

    if (timeRemaining < 0)
    {
        timeRemaining = 0;
    }
    else
    {
        timeRemaining /= 100;
    }

	if(!stackPushResult(VAL_INT, timeRemaining))
	{
		return(FALSE);
	}
	return(TRUE);
}

// -----------------------------------------------------------------------------------------
//set the time delay for reinforcements for an offworld mission
BOOL scrSetReinforcementTime(void)
{
	SDWORD		time;
    DROID       *psDroid;

	if (!stackPopParams(1, VAL_INT, &time))
	{
		return FALSE;
	}

    time *= 100;

	//check not more than one hour - the mission timers cannot cope at present!
	if (time != LZ_COMPROMISED_TIME AND time > 60*60*GAME_TICKS_PER_SEC)
	{
		ASSERT( FALSE,"The transport timer cannot be set to more than 1 hour!" );
		time = -1;
	}

    //not interseted in this check any more -  AB 28/01/99
    //quick check of the value - don't check if time has not been set
	/*if (mission.time > 0 AND time != LZ_COMPROMISED_TIME AND time > mission.time)
	{
		DBMB(("scrSetReinforcementTime: reinforcement time greater than mission time!"));
	}*/
	//store the value
	mission.ETA = time;

	//if offworld or campaign change mission, then add the timer
	//if (mission.type == LDS_MKEEP OR mission.type == LDS_MCLEAR OR
    //    mission.type == LDS_CAMCHANGE)
    if (missionCanReEnforce())
	{
		addTransporterTimerInterface();
	}

    //make sure the timer is not there if the reinforcement time has been set to < 0
    if (time < 0)
    {

        intRemoveTransporterTimer();

        /*only remove the launch if haven't got a transporter droid since the
        scripts set the time to -1 at the between stage if there are not going
        to be reinforcements on the submap  */
        for (psDroid = apsDroidLists[selectedPlayer]; psDroid != NULL; psDroid =
            psDroid->psNext)
        {
            if (psDroid->droidType == DROID_TRANSPORTER)
            {
                break;
            }
        }
        //if not found a transporter, can remove the launch button
        if (psDroid ==  NULL)
        {
            intRemoveTransporterLaunch();
        }
    }

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets all structure limits for a player to a specified value
BOOL scrSetAllStructureLimits(void)
{
	SDWORD				player, limit;
	STRUCTURE_LIMITS	*psStructLimits;
	UDWORD				i;

	if (!stackPopParams(2, VAL_INT, &limit, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetStructureLimits:player number is too high" );
		return FALSE;
	}

	if (limit < 0)
	{
		ASSERT( FALSE, "scrSetStructureLimits: limit is less than zero - %d", limit );
		return FALSE;
	}

	if (limit > LOTS_OF)
	{
		ASSERT( FALSE, "scrSetStructureLimits: limit is too high - %d - must be less than %d",
			limit, LOTS_OF );
		return FALSE;
	}

	//set all the limits to the value specified
	psStructLimits = asStructLimits[player];
	for (i = 0; i < numStructureStats; i++)
	{
		psStructLimits[i].limit = (UBYTE)limit;

		psStructLimits[i].globalLimit = (UBYTE)limit;

	}

	return TRUE;
}


// -----------------------------------------------------------------------------------------
// clear all the console messages
BOOL scrFlushConsoleMessages(void)
{
	flushConsoleMessages();

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Establishes the distance between two points - uses an approximation
BOOL scrDistanceTwoPts( void )
{
SDWORD	x1,y1,x2,y2;
SDWORD	retVal;


	if(!stackPopParams(4,VAL_INT,&x1,VAL_INT,&y1,VAL_INT,&x2,VAL_INT,&y2))
	{
		ASSERT( FALSE,"SCRIPT : Distance between two points - cannot get parameters" );
		return(FALSE);
	}

	/* Approximate the distance */
	retVal = dirtySqrt(x1,y1,x2,y2);

	if(!stackPushResult(VAL_INT,retVal))
	{
		ASSERT( FALSE,"SCRIPT : Distance between two points - cannot return result" );
		return(FALSE);
	}
	return(TRUE);
}

// -----------------------------------------------------------------------------------------
// Returns whether two objects can see each other
BOOL	scrLOSTwoBaseObjects( void )
{
BASE_OBJECT	*psSource,*psDest;
BOOL		bWallsBlock;
BOOL		retVal;

	if(!stackPopParams(3,ST_BASEOBJECT,&psSource,ST_BASEOBJECT,&psDest,VAL_BOOL,&bWallsBlock))
	{
		ASSERT( FALSE,"SCRIPT : scrLOSTwoBaseObjects - cannot get parameters" );
		return(FALSE);
	}

	if(bWallsBlock)
	{
		retVal = visibleObjWallBlock(psSource,psDest);
	}
	else
	{
		retVal = visibleObject(psSource,psDest);
	}

	if(!stackPushResult(VAL_BOOL,retVal))
	{
		ASSERT( FALSE,"SCRIPT : scrLOSTwoBaseObjects - cannot return result" );
		return(FALSE);
	}
	return(TRUE);
}

// -----------------------------------------------------------------------------------------
// Destroys all structures within a certain bounding area.
BOOL	scrDestroyStructuresInArea( void )
{
SDWORD		x1,y1,x2,y2;
UDWORD		typeRef;
UDWORD		player;
STRUCTURE	*psStructure,*psNextS;
FEATURE		*psFeature,*psNextF;
BOOL		bVisible,bTakeFeatures;
SDWORD		sX,sY;

	if(!stackPopParams(8, VAL_INT, &player, VAL_INT, &typeRef, VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2,
						VAL_INT, &y2, VAL_BOOL, &bVisible, VAL_BOOL, &bTakeFeatures))
	{
		ASSERT( FALSE,"SCRIPT : scrDestroyStructuresInArea - Cannot get parameters" );
		return(FALSE);
	}

	if(player>=MAX_PLAYERS)
	{
		ASSERT( FALSE,"Player number too high in scrDestroyStructuresInArea" );
	}

	for(psStructure = apsStructLists[player]; psStructure; psStructure = psNextS)
	{
		/* Keep a copy */
		psNextS = psStructure->psNext;

		sX = psStructure->x;
		sY = psStructure->y;

		if(psStructure->pStructureType->type == typeRef)
		{
			if(sX >= x1 AND sX <=x2 AND sY >= y1 AND sY <= y2)
			{
				if(bVisible)
				{
					destroyStruct(psStructure);
				}
				else
				{
					removeStruct(psStructure, TRUE);
				}
			}
		}
	}

	if(bTakeFeatures)
	{
		for(psFeature = apsFeatureLists[0]; psFeature; psFeature = psNextF)
		{
			/* Keep a copy */
			psNextF = psFeature->psNext;

			sX = psFeature->x;
			sY = psFeature->y;

		  	if( psFeature->psStats->subType == FEAT_BUILDING)
		  //		(psFeature->psStats->subType != FEAT_OIL_DRUM) AND
		  //		(psFeature->psStats->subType != FEAT_OIL_RESOURCE) )

			{
				if(sX >= x1 AND sX <=x2 AND sY >= y1 AND sY <= y2)
				{
					if(bVisible)
					{
						destroyFeature(psFeature);
					}
					else
					{
						removeFeature(psFeature);
					}
				}
			}
		}
	}
	return(TRUE);
}
// -----------------------------------------------------------------------------------------
// Returns a value representing the threat from droids in a given area
BOOL	scrThreatInArea( void )
{
SDWORD	x1,y1,x2,y2;
SDWORD	ldThreat,mdThreat,hdThreat;
UDWORD	playerLooking,playerTarget;
SDWORD	totalThreat;
DROID	*psDroid;
SDWORD	dX,dY;
BOOL	bVisible;

	if(!stackPopParams(10,VAL_INT,&playerLooking,VAL_INT,&playerTarget,VAL_INT,&x1,VAL_INT,&y1,VAL_INT,&x2,VAL_INT,&y2,
		VAL_INT,&ldThreat,VAL_INT,&mdThreat,VAL_INT,&hdThreat, VAL_BOOL, &bVisible))
	{
		ASSERT( FALSE,"SCRIPT : scrThreatInArea - Cannot get parameters" );
		return(FALSE);
	}

	if(playerLooking>=MAX_PLAYERS OR playerTarget >= MAX_PLAYERS)
	{
		ASSERT( FALSE,"Player number too high in scrThreatInArea" );
		return(FALSE);
	}

	totalThreat = 0;

	for(psDroid = apsDroidLists[playerTarget]; psDroid; psDroid = psDroid->psNext)
	{
		if (psDroid->droidType != DROID_WEAPON AND
			psDroid->droidType != DROID_PERSON AND
			psDroid->droidType != DROID_CYBORG AND
			psDroid->droidType != DROID_CYBORG_SUPER)
		{
			continue;
		}

		dX = psDroid->x;
		dY = psDroid->y;
		/* Do we care if the droid is visible or not */
		if(bVisible ? psDroid->visible[playerLooking] : TRUE)
		{
			/* Have we found a droid in this area */
			if(dX >= x1 AND dX <=x2 AND dY >= y1 AND dY <= y2)
			{
				switch ((asBodyStats + psDroid->asBits[COMP_BODY].nStat)->size)
				{
				case SIZE_LIGHT:
					totalThreat += ldThreat;
					break;
				case SIZE_MEDIUM:
					totalThreat += mdThreat;
					break;
				case SIZE_HEAVY:
				case SIZE_SUPER_HEAVY:
					totalThreat += hdThreat;
					break;
				default:
					ASSERT( FALSE, "Weird droid size in threat assessment" );
					break;
				}
			}
		}
	}
//	DBPRINTF(("scrThreatInArea: returning %d\n", totalThreat));
	if(!stackPushResult(VAL_INT,totalThreat))
	{
		ASSERT( FALSE,"SCRIPT : Cannot push result in scrThreatInArea" );
		return(FALSE);
	}
	return(TRUE);
}
// -----------------------------------------------------------------------------------------
// returns the nearest gateway bottleneck to a specified point
BOOL scrGetNearestGateway( void )
{
SDWORD	x,y;
SDWORD	gX,gY;
UDWORD	nearestSoFar;
UDWORD	dist;
GATEWAY	*psGateway;
SDWORD	retX,retY;
SDWORD	*rX,*rY;
BOOL	success;

	if(!stackPopParams(4, VAL_INT, &x, VAL_INT, &y, VAL_REF|VAL_INT, &rX, VAL_REF|VAL_INT, &rY))
	{
		ASSERT( FALSE,"SCRIPT : Cannot get parameters for scrGetNearestGateway" );
		return(FALSE);
	}

	if(x<0 OR x>(SDWORD)mapWidth OR y<0 OR y>(SDWORD)mapHeight)
	{
		ASSERT( FALSE,"SCRIPT : Invalid coordinates in getNearestGateway" );
		return(FALSE);
	}

	if(psGateways == NULL)
	{
		ASSERT( FALSE,"SCRIPT : No gateways found in getNearestGatway" );
		return(FALSE);
	}

	nearestSoFar = UDWORD_MAX;
	retX = retY = 0;
	success = FALSE;
	for(psGateway = psGateways; psGateway; psGateway = psGateway->psNext)
	{
		/* Get gateway midpoint */
		gX = (psGateway->x1 + psGateway->x2)/2;
		gY = (psGateway->y1 + psGateway->y2)/2;

		/* Estimate the distance to it */
		dist = dirtySqrt(x,y,gX,gY);

		/* Is it best we've found? */
		if(dist<nearestSoFar)
		{
			success = TRUE;
			/* Yes, then keep a record of it */
			nearestSoFar = dist;
			retX = gX;
			retY = gY;
		}
	}

	*rX = retX;
	*rY = retY;

	if(!stackPushResult(VAL_BOOL,success))
	{
		ASSERT( FALSE,"SCRIPT : Cannot return result for stackPushResult" );
		return(FALSE);
	}


	return(TRUE);
}
// -----------------------------------------------------------------------------------------
BOOL	scrSetWaterTile(void)
{
UDWORD	tileNum;

	if(!stackPopParams(1,VAL_INT, &tileNum))
	{
		ASSERT( FALSE,"SCRIPT : Cannot get parameter for scrSetWaterTile" );
		return(FALSE);
	}


	if(tileNum > 96)
	{
		ASSERT( FALSE,"SCRIPT : Water tile number too high in scrSetWaterTile" );
		return(FALSE);
	}

	setUnderwaterTile(tileNum);

	return(TRUE);
}
// -----------------------------------------------------------------------------------------
BOOL	scrSetRubbleTile(void)
{
UDWORD	tileNum;

	if(!stackPopParams(1,VAL_INT, &tileNum))
	{
		ASSERT( FALSE,"SCRIPT : Cannot get parameter for scrSetRubbleTile" );
		return(FALSE);
	}


	if(tileNum > 96)
	{
		ASSERT( FALSE,"SCRIPT : Rubble tile number too high in scrSetWaterTile" );
		return(FALSE);
	}

	setRubbleTile(tileNum);

	return(TRUE);
}
// -----------------------------------------------------------------------------------------
BOOL	scrSetCampaignNumber(void)
{
UDWORD	campaignNumber;

	if(!stackPopParams(1,VAL_INT, &campaignNumber))
	{
		ASSERT( FALSE,"SCRIPT : Cannot get parameter for scrSetCampaignNumber" );
		return(FALSE);
	}


	setCampaignNumber(campaignNumber);

	return(TRUE);
}
// -----------------------------------------------------------------------------------------

static BOOL scrGetUnitCount( void )
{
	return TRUE;
}


// -----------------------------------------------------------------------------------------
// Tests whether a structure has a certain module for a player. Tests whether any structure
// has this module if structure is null
BOOL	scrTestStructureModule(void)
{
SDWORD	player,refId;
STRUCTURE	*psStructure,*psStruct;
BOOL	bFound;

	if(!stackPopParams(3,VAL_INT,&player,ST_STRUCTURE,&psStructure,VAL_INT,&refId))
	{
		ASSERT( FALSE,"SCRIPT : Cannot get parameters in scrTestStructureModule" );
		return(FALSE);
	}

	if(player>=MAX_PLAYERS)
	{
		ASSERT( FALSE,"SCRIPT : Player number too high in scrTestStructureModule" );
		return(FALSE);

	}

	/* Nothing yet */
	bFound = FALSE;

	/* Check the specified case first */
	if(psStructure)
	{
		if(structHasModule(psStructure))
		{
			bFound = TRUE;
		}
	}
	/* psStructure was NULL - so test the general case */
	else
	{
		// Search them all, but exit if we get one!!
		for(psStruct = apsStructLists[player],bFound = FALSE;
			psStruct AND !bFound; psStruct = psStruct->psNext)
		{
			if(structHasModule(psStruct))
			{
				bFound = TRUE;
			}
		}
	}

	/* Send back the result */
	if(!stackPushResult(VAL_BOOL,bFound))
	{
		ASSERT( FALSE,"SCRIPT : Cannot push result for scrTestStructureModule" );
		return(FALSE);
	}

	return(TRUE);
}


// -----------------------------------------------------------------------------------------
BOOL	scrForceDamage( void )
{
DROID		*psDroid;
STRUCTURE	*psStructure;
FEATURE		*psFeature;
BASE_OBJECT	*psObj;
UDWORD		damagePercent;
FRACT		divisor;
UDWORD		newVal;

	/* OK - let's get the vars */
	if(!stackPopParams(2,ST_BASEOBJECT,&psObj,VAL_INT,&damagePercent))
	{
		ASSERT( FALSE,"Cannot pop params for scrForceDamage" );
		return(FALSE);
	}

	/* Got to be a percent, so must be less than or equal to 100 */
	if(damagePercent > 100)
	{
		ASSERT( FALSE,"scrForceDamage : You're supposed to be passing in a PERCENTAGE VALUE, \
			instead I got given %d, which is clearly no good, now is it!?", damagePercent );
		return(FALSE);
	}

	/* Get percentage in range [0.1] */
	divisor =  MAKEFRACT(damagePercent) / 100;

	/* See what we're dealing with */
	switch(psObj->type)
	{
	case OBJ_DROID:
		psDroid = (DROID *) psObj;
		newVal = MAKEINT((divisor*psDroid->originalBody));
		psDroid->body = newVal;
		break;
	case OBJ_STRUCTURE:
		psStructure = (STRUCTURE *) psObj;
		newVal = MAKEINT((divisor*structureBody(psStructure)));
		psStructure->body = (UWORD)newVal;
		break;
	case OBJ_FEATURE:
		psFeature = (FEATURE *) psObj;
		/* Some features cannot be damaged */
		if(psFeature->psStats->damageable)
		{
			newVal = MAKEINT((divisor*psFeature->psStats->body));
			psFeature->body = newVal;
		}
		break;
	default:
		ASSERT( FALSE,"Unsupported base object type in scrForceDamage" );
		return(FALSE);
		break;
	}

	return(TRUE);

}
// Kills of a droid without spawning any explosion effects.
// -----------------------------------------------------------------------------------------
BOOL	scrDestroyUnitsInArea( void )
{
DROID	*psDroid,*psNext;
SDWORD	x1,y1,x2,y2;
UDWORD	player;
UDWORD	count=0;

	if(!stackPopParams(5,VAL_INT,&x1,VAL_INT,&y1,VAL_INT,&x2,VAL_INT,&y2,VAL_INT, &player))
	{
		ASSERT( FALSE,"Cannot get params for scrDestroyUnitsInArea" );
		return(FALSE);
	}

	if(player>=MAX_PLAYERS)
	{
		ASSERT( FALSE,"Invalid player number in scrKillDroidsInArea" );
	}

	for(psDroid = apsDroidLists[player]; psDroid; psDroid = psNext)
	{
		psNext = psDroid->psNext;	// get a copy cos pointer will be lost
		if( (psDroid->x > x1) AND (psDroid->x < x2) AND
			(psDroid->y > y1) AND (psDroid->y < y2) )
		{
			/* then it's inside the area */
			destroyDroid(psDroid);
			count++;
		}
	}

	if(!stackPushResult(VAL_INT,count))
	{
		return(FALSE);
	}

	return(TRUE);
}
// -----------------------------------------------------------------------------------------
BOOL	scrRemoveDroid( void )
{
DROID	*psDroid;

	if(!stackPopParams(1,ST_DROID,&psDroid))
	{		ASSERT( FALSE,"Cannot get vars for scrRemoveDroid!" );
		return(FALSE);
	}

	if(psDroid)
	{
		vanishDroid(psDroid);
	}

	return(TRUE);
}
// -----------------------------------------------------------------------------------------
BOOL	structHasModule(STRUCTURE *psStruct)
{
STRUCTURE_STATS	*psStats;
BOOL			bFound;

	/* Fail if the structure isn't built yet */
	if(psStruct->status != SS_BUILT)
	{
		return(FALSE);
	}

	/* Not found yet */
	bFound = FALSE;


	if(psStruct==NULL)
	{
		ASSERT( psStruct!=NULL,"structHasModule - Testing for a module from a NULL struct - huh!?" );
		return(FALSE);
	}

	if(psStruct)
	{
		/* Grab a stats pointer */
		psStats = psStruct->pStructureType;
		if(StructIsFactory(psStruct)
			OR psStats->type == REF_POWER_GEN OR psStats->type == REF_RESEARCH)
		{
			switch(psStats->type)
			{
				case REF_POWER_GEN:
					if (((POWER_GEN *)psStruct->pFunctionality)->capacity)
					{
						bFound = TRUE;
					}
					break;
				case REF_FACTORY:
				case REF_VTOL_FACTORY:
					if (((FACTORY *)psStruct->pFunctionality)->capacity)
					{
						bFound = TRUE;
					}
					break;
				case REF_RESEARCH:
					if (((RESEARCH_FACILITY *)psStruct->pFunctionality)->capacity)

					{
						bFound = TRUE;
					}
					break;
				default:
					//no other structures can have modules attached
					break;
			}
		}
		else
		{
			/* Wrong type of building - cannot have a module */
			bFound = FALSE;
		}

	}
	return(bFound);
}

// -----------------------------------------------------------------------------------------
// give player a template belonging to another.
BOOL scrAddTemplate(void)
{
	DROID_TEMPLATE *psTemplate;
	UDWORD			player;

	if (!stackPopParams(2, ST_TEMPLATE, &psTemplate, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrAddTemplate:player number is too high" );
		return FALSE;
	}

	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),"scrAddTemplate: Invalid template pointer" );

	if(	addTemplate(player,psTemplate))
	{
		if (!stackPushResult(VAL_BOOL,TRUE))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL,FALSE))
		{
			return FALSE;
		}
	}

	return TRUE;
}



// -----------------------------------------------------------------------------------------

// additional structure check
static BOOL structDoubleCheck(BASE_STATS *psStat,UDWORD xx,UDWORD yy)
{
	UDWORD x,y,xTL,yTL,xBR,yBR;
	UBYTE count =0;

	STRUCTURE_STATS *psBuilding = (STRUCTURE_STATS *)psStat;

	xTL = xx-1;
	yTL = yy-1;
	xBR = (xx + psBuilding->baseWidth );
	yBR = (yy + psBuilding->baseBreadth );
	// can you get past it?

	y = yTL;	// top
	for(x = xTL;x!=xBR+1;x++)
	{
		if(fpathGroundBlockingTile(x,y))
		{
			count++;
			break;
		}}

	y = yBR;	// bottom
	for(x = xTL;x!=xBR+1;x++)
	{
		if(fpathGroundBlockingTile(x,y))
		{
			count++;
			break;
		}}

	x = xTL;	// left
	for(y = yTL+1; y!=yBR; y++)
	{
		if(fpathGroundBlockingTile(x,y))
		{
			count++;
			break;
		}}

	x = xBR;	// right
	for(y = yTL+1; y!=yBR; y++)
	{
		if(fpathGroundBlockingTile(x,y))
		{
			count++;
			break;
		}}

	if(count <2)//no more than one blocking side.
	{
		return TRUE;
	}
	return FALSE;

}

// pick a structure location(only used in skirmish game at 27Aug) ajl.
BOOL scrPickStructLocation(void)
{
	SDWORD			*pX,*pY;
	SDWORD			index;
	STRUCTURE_STATS	*psStat;
	UDWORD			numIterations = 30;
	BOOL			found = FALSE;
	UDWORD			startX, startY, incX, incY;
	SDWORD			x=0, y=0;
	UDWORD			player;

	if (!stackPopParams(4, ST_STRUCTURESTAT, &index, VAL_REF|VAL_INT, &pX ,
        VAL_REF|VAL_INT, &pY, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrPickStructLocation:player number is too high" );
		return FALSE;
	}

    // check for wacky coords.
	if(		*pX < 0
		||	*pX > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	*pY < 0
		||	*pY > (SDWORD)(mapHeight<<TILE_SHIFT)
	  )
	{
		goto failedstructloc;
	}

	psStat = &asStructureStats[index];			// get stat.
	startX = *pX >> TILE_SHIFT;					// change to tile coords.
	startY = *pY >> TILE_SHIFT;

	for (incX = 1, incY = 1; incX < numIterations; incX++, incY++)
	{
		if (!found){			//top
			y = startY - incY;
			for(x = startX - incX; x < (SDWORD)(startX + incX); x++){
				if ( validLocation((BASE_STATS*)psStat, x, y, player, FALSE)
					 && structDoubleCheck((BASE_STATS*)psStat,x,y)
					){
					found = TRUE;
					break;
				}}}

		if (!found)	{			//right
			x = startX + incX;
			for(y = startY - incY; y < (SDWORD)(startY + incY); y++){
				if(validLocation((BASE_STATS*)psStat, x, y, player, FALSE)
					 && structDoubleCheck((BASE_STATS*)psStat,x,y)
					){
					found = TRUE;
					break;
				}}}

		if (!found){			//bot
			y = startY + incY;
			for(x = startX + incX; x > (SDWORD)(startX - incX); x--){
				if(validLocation((BASE_STATS*)psStat, x, y, player, FALSE)
					 && structDoubleCheck((BASE_STATS*)psStat,x,y)
					 ){
					found = TRUE;
					break;
				}}}

		if (!found){			//left
			x = startX - incX;
			for(y = startY + incY; y > (SDWORD)(startY - incY); y--){
				if(validLocation((BASE_STATS*)psStat, x, y, player, FALSE)
					 && structDoubleCheck((BASE_STATS*)psStat,x,y)
					 ){
					found = TRUE;
					break;
				}}}

		if (found)
		{
			break;
		}
	}

	if(found)	// did It!
	{
		// back to world coords.
		*pX = (x << TILE_SHIFT) + (psStat->baseWidth * (TILE_UNITS/2));
		*pY = (y << TILE_SHIFT) + (psStat->baseBreadth * (TILE_UNITS/2));

		if (!stackPushResult(VAL_BOOL, TRUE))		// success!
		{
			return FALSE;
		}

		return TRUE;
	}
	else
	{
failedstructloc:
		if (!stackPushResult(VAL_BOOL,FALSE))		// failed!
		{
			return FALSE;
		}
	}
	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Sets the transporter entry and exit points for the map
BOOL scrSetTransporterExit(void)
{
	SDWORD	iPlayer, iExitTileX, iExitTileY;

	if (!stackPopParams(3, VAL_INT, &iPlayer, VAL_INT, &iExitTileX, VAL_INT, &iExitTileY))
	{
		return FALSE;
	}

	missionSetTransporterExit( iPlayer, iExitTileX, iExitTileY );

	return TRUE;
}

// -----------------------------------------------------------------------------------------
// Fly transporters in at start of map
BOOL scrFlyTransporterIn(void)
{
	SDWORD	iPlayer, iEntryTileX, iEntryTileY;
	BOOL	bTrackTransporter;

	if (!stackPopParams(4, VAL_INT, &iPlayer, VAL_INT, &iEntryTileX, VAL_INT, &iEntryTileY,
							VAL_BOOL, &bTrackTransporter))
	{
		return FALSE;
	}

	missionSetTransporterEntry( iPlayer, iEntryTileX, iEntryTileY );
	missionFlyTransportersIn( iPlayer, bTrackTransporter );

	return TRUE;
}

// -----------------------------------------------------------------------------------------



/*
 ** scrGetGameStatus
 *
 *  FILENAME: C:\Deliverance\SrcPSX\ScriptFuncs.c
 *
 *  PARAMETERS: The parameter passed must be one of the STATUS_ variable
 *
 *  DESCRIPTION: Returns various BOOL options in the game	e.g. If the reticule is open
 *      - You should use the externed variable intMode for other game mode options
 *        e.g. in the intelligence screen or desgin screen)
 *
 *  RETURNS:
 *
 */
BOOL scrGetGameStatus(void)
{
	SDWORD GameChoice;
	BOOL result;

	if (!stackPopParams(1, VAL_INT, &GameChoice))
	{
		return FALSE;
	}

//	DBPRINTF(("getgamestatus choice=%d\n",GameChoice));

	result=FALSE;		// the default result is false

	switch (GameChoice)
	{

		case STATUS_ReticuleIsOpen:
			if(widgGetFromID(psWScreen,IDRET_FORM) != NULL) result=TRUE;
			break;

		case STATUS_BattleMapViewEnabled:
//			if (driveTacticalActive()==TRUE) result=TRUE;


			if (result==TRUE)
			{
				debug( LOG_NEVER, "battle map active" );
			}
			else
			{
				debug( LOG_NEVER, "battle map notactive" );
			}


			break;
		case STATUS_DeliveryReposInProgress:
			if (DeliveryReposValid()==TRUE) result=TRUE;
			break;

		default:
		ASSERT( FALSE,"ScrGetGameStatus. Invalid STATUS_ variable" );
		break;
	}

	if (!stackPushResult(VAL_BOOL, result))
	{
		return FALSE;
	}
	return TRUE;
}

//get the colour number used by a player
BOOL scrGetPlayerColour(void)
{
	SDWORD		player, colour;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetPlayerColour: player number is too high" );
		return FALSE;
	}

    colour = (SDWORD)getPlayerColour(player);

	if (!stackPushResult(VAL_INT, colour))
	{
		return FALSE;
	}

	return TRUE;
}

//get the colour name of the player ("green", "black" etc)
BOOL scrGetPlayerColourName(void)
{
	SDWORD		player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS || player < 0)
	{
		ASSERT( FALSE, "scrGetPlayerColourName: wrong player index" );
		return FALSE;
	}

	if (!stackPushResult(VAL_STRING, (SDWORD)getPlayerColourName(player)))
	{
		debug(LOG_ERROR, "scrGetPlayerColourName(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

//set the colour number to use for a player
BOOL scrSetPlayerColour(void)
{
	SDWORD		player, colour;

	if (!stackPopParams(2, VAL_INT, &colour, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetPlayerColour:player number is too high" );
		return FALSE;
	}

	if (colour >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetPlayerColour:colour number is too high" );
		return FALSE;
	}

    //not the end of the world if this doesn't work so don't check the return code
    (void)setPlayerColour(player, colour);

	return TRUE;
}

//set all droids in an area to belong to a different player - returns the number of droids changed
BOOL scrTakeOverDroidsInArea(void)
{
	SDWORD		fromPlayer, toPlayer, x1, x2, y1, y2, numChanged;
    DROID       *psDroid, *psNext;

	if (!stackPopParams(6, VAL_INT, &fromPlayer, VAL_INT, &toPlayer,
        VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (fromPlayer >= MAX_PLAYERS OR toPlayer >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea:player number is too high" );
		return FALSE;
	}

	if (x1 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: x1 is greater than max mapWidth" );
		return FALSE;
	}

    if (x2 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: x2 is greater than max mapWidth" );
		return FALSE;
	}

    if (y1 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: y1 is greater than max mapHeight" );
		return FALSE;
	}

    if (y2 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: y2 is greater than max mapHeight" );
		return FALSE;
	}

    numChanged = 0;
    for (psDroid = apsDroidLists[fromPlayer]; psDroid != NULL; psDroid = psNext)
    {
        psNext = psDroid->psNext;
        //check if within area specified
        if (psDroid->x >= x1 AND psDroid->x <= x2 AND
            psDroid->y >= y1 AND psDroid->y <= y2)
        {
            //give the droid away
            if (giftSingleDroid(psDroid, toPlayer))
            {
                numChanged++;
            }
        }
    }

	if (!stackPushResult(VAL_INT, numChanged))
	{
		return FALSE;
	}

    return TRUE;
}

/*this takes over a single droid and passes a pointer back to the new one*/
BOOL scrTakeOverSingleDroid(void)
{
	SDWORD			playerToGain;
    DROID           *psDroidToTake, *psNewDroid;

    if (!stackPopParams(2, ST_DROID, &psDroidToTake, VAL_INT, &playerToGain))
    {
		return FALSE;
    }

	if (playerToGain >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrTakeOverSingleUnit:player number is too high" );
		return FALSE;
	}

    if (psDroidToTake == NULL)
    {
        ASSERT( FALSE, "scrTakeOverSingleUnit: Null unit" );
        return FALSE;
    }

	ASSERT( PTRVALID(psDroidToTake, sizeof(DROID)),
		"scrTakeOverSingleUnit: Invalid unit pointer" );

    psNewDroid = giftSingleDroid(psDroidToTake, playerToGain);

	if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)psNewDroid))
	{
		return FALSE;
    }
	return TRUE;
}

// set all droids in an area of a certain experience level or less to belong to
// a different player - returns the number of droids changed
BOOL scrTakeOverDroidsInAreaExp(void)
{
	SDWORD		fromPlayer, toPlayer, x1, x2, y1, y2, numChanged, level, maxUnits;
    DROID       *psDroid, *psNext;

	if (!stackPopParams(8, VAL_INT, &fromPlayer, VAL_INT, &toPlayer,
        VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2, VAL_INT, &level, VAL_INT, &maxUnits))
	{
		return FALSE;
	}

	if (fromPlayer >= MAX_PLAYERS OR toPlayer >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea:player number is too high" );
		return FALSE;
	}

	if (x1 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: x1 is greater than max mapWidth" );
		return FALSE;
	}

    if (x2 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: x2 is greater than max mapWidth" );
		return FALSE;
	}

    if (y1 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: y1 is greater than max mapHeight" );
		return FALSE;
	}

    if (y2 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverUnitsInArea: y2 is greater than max mapHeight" );
		return FALSE;
	}

    numChanged = 0;
    for (psDroid = apsDroidLists[fromPlayer]; psDroid != NULL; psDroid = psNext)
    {
        psNext = psDroid->psNext;
        //check if within area specified
        if ((psDroid->droidType != DROID_CONSTRUCT) &&
			(psDroid->droidType != DROID_REPAIR) &&
            (psDroid->droidType != DROID_CYBORG_CONSTRUCT) &&
            (psDroid->droidType != DROID_CYBORG_REPAIR) &&
//			((SDWORD)getDroidLevel(psDroid) <= level) AND
			((SDWORD)psDroid->numKills <= level) AND
			psDroid->x >= x1 AND psDroid->x <= x2 AND
            psDroid->y >= y1 AND psDroid->y <= y2)
        {
            //give the droid away
            if (giftSingleDroid(psDroid, toPlayer))
            {
                numChanged++;
            }
        }

		if (numChanged >= maxUnits)
		{
			break;
		}
    }

	if (!stackPushResult(VAL_INT, numChanged))
	{
		return FALSE;
	}

    return TRUE;
}

/*this takes over a single structure and passes a pointer back to the new one*/
BOOL scrTakeOverSingleStructure(void)
{
	SDWORD			playerToGain;
    STRUCTURE       *psStructToTake, *psNewStruct;
    UDWORD          structureInc;

    if (!stackPopParams(2, ST_STRUCTURE, &psStructToTake, VAL_INT, &playerToGain))
    {
		return FALSE;
    }

	if (playerToGain >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrTakeOverSingleStructure:player number is too high" );
		return FALSE;
	}

    if (psStructToTake == NULL)
    {
        ASSERT( FALSE, "scrTakeOverSingleStructure: Null structure" );
        return FALSE;
    }

	ASSERT( PTRVALID(psStructToTake, sizeof(STRUCTURE)),
		"scrTakeOverSingleStructure: Invalid structure pointer" );

    structureInc = psStructToTake->pStructureType->ref - REF_STRUCTURE_START;
    if (playerToGain == (SDWORD)selectedPlayer AND StructIsFactory(psStructToTake) AND
        asStructLimits[playerToGain][structureInc].currentQuantity >= MAX_FACTORY)
    {
		debug( LOG_NEVER, "scrTakeOverSingleStructure - factory ignored for selectedPlayer\n" );
        psNewStruct = NULL;
    }
    else
    {
        psNewStruct = giftSingleStructure(psStructToTake, (UBYTE)playerToGain, TRUE);
        if (psNewStruct)
        {
            //check the structure limits aren't compromised
            if (asStructLimits[playerToGain][structureInc].currentQuantity >
                asStructLimits[playerToGain][structureInc].limit)
            {
                asStructLimits[playerToGain][structureInc].limit = asStructLimits[
                    playerToGain][structureInc].currentQuantity;
            }
            //for each structure taken - add graphical effect if the selectedPlayer
            if (playerToGain == (SDWORD)selectedPlayer)
            {
                assignSensorTarget((BASE_OBJECT *)psNewStruct);
            }
        }
    }

	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (SDWORD)psNewStruct))
	{
		return FALSE;
    }
	return TRUE;
}

//set all structures in an area to belong to a different player - returns the number of droids changed
//will not work on factories for the selectedPlayer
BOOL scrTakeOverStructsInArea(void)
{
	SDWORD		fromPlayer, toPlayer, x1, x2, y1, y2, numChanged;
    STRUCTURE   *psStruct, *psNext, *psNewStruct;
    UDWORD      structureInc;

	if (!stackPopParams(6, VAL_INT, &fromPlayer, VAL_INT, &toPlayer,
        VAL_INT, &x1, VAL_INT, &y1, VAL_INT, &x2, VAL_INT, &y2))
	{
		return FALSE;
	}

	if (fromPlayer >= MAX_PLAYERS OR toPlayer >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrTakeOverStructsInArea:player number is too high" );
		return FALSE;
	}

	if (x1 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverStructsInArea: x1 is greater than max mapWidth" );
		return FALSE;
	}

    if (x2 > (SDWORD)(MAP_MAXWIDTH << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverStructsInArea: x2 is greater than max mapWidth" );
		return FALSE;
	}

    if (y1 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverStructsInArea: y1 is greater than max mapHeight" );
		return FALSE;
	}

    if (y2 > (SDWORD)(MAP_MAXHEIGHT << TILE_SHIFT))
	{
		ASSERT( FALSE, "scrTakeOverStructsInArea: y2 is greater than max mapHeight" );
		return FALSE;
	}

    numChanged = 0;
    for (psStruct = apsStructLists[fromPlayer]; psStruct != NULL; psStruct = psNext)
    {
        psNext = psStruct->psNext;
        //check if within area specified
        if (psStruct->x >= x1 AND psStruct->x <= x2 AND
            psStruct->y >= y1 AND psStruct->y <= y2)
        {
            //changed this so allows takeOver is have less than 5 factories
            //don't work on factories for the selectedPlayer
            structureInc = psStruct->pStructureType->ref - REF_STRUCTURE_START;
            if (toPlayer == (SDWORD)selectedPlayer AND StructIsFactory(psStruct) AND
                asStructLimits[toPlayer][structureInc].currentQuantity >= MAX_FACTORY)
            {
				debug( LOG_NEVER, "scrTakeOverStructsInArea - factory ignored for selectedPlayer\n" );
            }
            else
            {
                //give the structure away
                psNewStruct = giftSingleStructure(psStruct, (UBYTE)toPlayer, TRUE);
                if (psNewStruct)
                {
                    numChanged++;
                    //check the structure limits aren't compromised
                    //structureInc = psNewStruct->pStructureType->ref - REF_STRUCTURE_START;
                    if (asStructLimits[toPlayer][structureInc].currentQuantity >
                        asStructLimits[toPlayer][structureInc].limit)
                    {
                        asStructLimits[toPlayer][structureInc].limit = asStructLimits[
                            toPlayer][structureInc].currentQuantity;
                    }
                    //for each structure taken - add graphical effect if the selectedPlayer
                    if (toPlayer == (SDWORD)selectedPlayer)
                    {
                        assignSensorTarget((BASE_OBJECT *)psNewStruct);
                    }
                }
            }
        }
    }

	if (!stackPushResult(VAL_INT, numChanged))
	{
		return FALSE;
	}

    return TRUE;
}

//set Flag for defining what happens to the droids in a Transporter
BOOL scrSetDroidsToSafetyFlag(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}

    setDroidsToSafetyFlag(bState);

	return TRUE;
}

//set Flag for defining whether the coded countDown is called
BOOL scrSetPlayCountDown(void)
{
	BOOL bState;

	if (!stackPopParams(1, VAL_BOOL, &bState))
	{
		return FALSE;
	}


    setPlayCountDown((UBYTE)bState);


	return TRUE;
}

//get the number of droids currently onthe map for a player
BOOL scrGetDroidCount(void)
{
	SDWORD		player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetUnitCount:player number is too high" );
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, getNumDroids(player)))
	{
		return FALSE;
	}

	return TRUE;
}


// fire a weapon stat at an object
BOOL scrFireWeaponAtObj(void)
{
	SDWORD			wIndex;
	BASE_OBJECT		*psTarget;
	WEAPON			sWeapon;

	if (!stackPopParams(2, ST_WEAPON, &wIndex, ST_BASEOBJECT, &psTarget))
	{
		return FALSE;
	}

	if (psTarget == NULL)
	{
		ASSERT( FALSE,"scrFireWeaponAtObj: Null target pointer" );
		return FALSE;
	}

	memset(&sWeapon, 0, sizeof(WEAPON));
	sWeapon.nStat = wIndex;

	// send the projectile using the selectedPlayer so that it can always be seen
	proj_SendProjectile(&sWeapon, NULL, selectedPlayer, psTarget->x,psTarget->y,psTarget->z, psTarget, TRUE, FALSE);

	return TRUE;
}

// fire a weapon stat at a location
BOOL scrFireWeaponAtLoc(void)
{
	SDWORD			wIndex, x,y;
	WEAPON			sWeapon;

	if (!stackPopParams(3, ST_WEAPON, &wIndex, VAL_INT, &x, VAL_INT, &y))
	{
		return FALSE;
	}

	memset(&sWeapon, 0, sizeof(WEAPON));
	sWeapon.nStat = wIndex;

	// send the projectile using the selectedPlayer so that it can always be seen
	proj_SendProjectile(&sWeapon, NULL, selectedPlayer, x,y,map_Height(x,y), NULL, TRUE, FALSE);

	return TRUE;
}

// set the number of kills for a droid
BOOL scrSetDroidKills(void)
{
	DROID	*psDroid;
	SDWORD	kills;

	if (!stackPopParams(2, ST_DROID, &psDroid, VAL_INT, &kills))
	{
		return TRUE;
	}

	if ((psDroid == NULL) ||
		(psDroid->type != OBJ_DROID))
	{
		ASSERT( FALSE, "scrSetUnitKills: NULL/invalid unit pointer" );
		return FALSE;
	}

	psDroid->numKills = (UWORD)kills;

	return TRUE;
}

// reset the visibility for a player
BOOL scrResetPlayerVisibility(void)
{
	SDWORD			player, i;
	BASE_OBJECT		*psObj;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		return FALSE;
	}

	if (player < 0 || player > MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrResetPlayerVisibility: invalid player" );
		return FALSE;
	}

	for(i=0; i< MAX_PLAYERS; i++)
	{
		if (i == player)
		{
			continue;
		}

		for(psObj = (BASE_OBJECT *)apsDroidLists[i]; psObj; psObj = psObj->psNext)
		{
			psObj->visible[player] = 0;
		}

		for(psObj = (BASE_OBJECT *)apsStructLists[i]; psObj; psObj = psObj->psNext)
		{
			psObj->visible[player] = 0;
		}
	}

	for(psObj = (BASE_OBJECT *)apsFeatureLists[0]; psObj; psObj = psObj->psNext)
	{
		psObj->visible[player] = 0;
	}

	clustResetVisibility(player);

	return TRUE;
}


// set the vtol return pos for a player
BOOL scrSetVTOLReturnPos(void)
{
	SDWORD		player, tx,ty;

	if (!stackPopParams(3, VAL_INT, &player, VAL_INT, &tx, VAL_INT, &ty))
	{
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetVTOLReturnPos: invalid player" );
			return FALSE;
	}

	asVTOLReturnPos[player].x = (tx * TILE_UNITS) + TILE_UNITS/2;
	asVTOLReturnPos[player].y = (ty * TILE_UNITS) + TILE_UNITS/2;

	return TRUE;
}

//called via the script in a Limbo Expand level to set the level to plain ol' expand
BOOL scrResetLimboMission(void)
{
    //check currently on a Limbo expand mission
    if (!missionLimboExpand())
    {
        ASSERT( FALSE, "scrResetLimboMission: current mission type invalid" );
        return FALSE;
    }

    //turn it into an expand mission
    resetLimboMission();

    return TRUE;
}



// skirmish only.
BOOL scrIsVtol(void)
{
	DROID *psDroid;
	BOOL	result;

	if (!stackPopParams(1, ST_DROID, &psDroid))
	{
		return TRUE;
	}

	if(psDroid == NULL)
	{
		ASSERT( FALSE,"scrIsVtol: null droid passed in." );
	}

	result = vtolDroid(psDroid) ;
	if (!stackPushResult(VAL_BOOL,result))
	{
		return FALSE;
	}
	return TRUE;
}




// do the setting up of the template list for the tutorial.
BOOL scrTutorialTemplates(void)
{
	DROID_TEMPLATE	*psCurr, *psPrev;
	char			pName[MAX_NAME_SIZE];

	// find ViperLtMGWheels
	strcpy(pName,"ViperLtMGWheels");
	if (!getResourceName(pName))
	{
		debug( LOG_ERROR, "tutorial template setup failed" );
		abort();
		return FALSE;
	}

	getDroidResourceName(pName);


	for (psCurr = apsDroidTemplates[selectedPlayer],psPrev = NULL;
			psCurr != NULL;
			psCurr = psCurr->psNext)
	{
		if (strcmp(pName,psCurr->aName)==0)
		{
			if (psPrev)
			{
				psPrev->psNext = psCurr->psNext;
			}
			else
			{
				apsDroidTemplates[selectedPlayer] = psCurr->psNext;
			}
			//quit looking cos found
			break;
		}
		psPrev = psCurr;
	}

	// Delete the template.
	if(psCurr)
	{
		HEAP_FREE(psTemplateHeap, psCurr);
	}
	else
	{
		debug( LOG_ERROR, "tutorial template setup failed" );
		abort();
		return FALSE;
	}
	return TRUE;
}







//-----------------------------------------
//New functions
//-----------------------------------------

//compare two strings (0 means they are different)
BOOL scrStrcmp(void)
{
	char	*ssval1=NULL;
	char	*ssval2=NULL;

	if (!stackPopParams(2, VAL_STRING, &ssval1, VAL_STRING, &ssval2))
	{
		debug(LOG_ERROR, "scrStrcmp(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, !strcmp(ssval1, ssval2)))
	{
		debug(LOG_ERROR, "scrStrcmp: failed to push result");
		return FALSE;
	}

	return TRUE;
}

/* Output a string to console */
BOOL scrConsole(void)
{
	char	*ssval=NULL;

	if (!stackPopParams(1, VAL_STRING, &ssval))
	{
		debug(LOG_ERROR, "scrConsole(): stack failed");
		return FALSE;
	}

	addConsoleMessage(ssval,DEFAULT_JUSTIFY);

	return TRUE;
}

BOOL scrDebug[MAX_PLAYERS];

//turn on debug messages
BOOL scrDbgMsgOn(void)
{
	BOOL	bOn;
	SDWORD	player;

	if (!stackPopParams(2, VAL_INT, &player, VAL_BOOL, &bOn))
	{
		debug(LOG_ERROR, "scrDbgMsgOn(): stack failed");
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrDbgMsgOn(): wrong player number");
		return FALSE;
	}

	scrDebug[player] = bOn;

	return TRUE;
}

BOOL scrMsg(void)
{
	SDWORD	playerTo,playerFrom;
	char	*ssval=NULL;
	char tmp[255];

	if (!stackPopParams(3, VAL_STRING, &ssval, VAL_INT, &playerFrom, VAL_INT, &playerTo))
	{
		debug(LOG_ERROR, "scrMsg(): stack failed");
		return FALSE;
	}

	if(playerFrom < 0 || playerFrom >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrMsg(): playerFrom out of range");
		return FALSE;
	}

	if(playerTo < 0 || playerTo >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrMsg(): playerTo out of range");
		return FALSE;
	}

	sendAIMessage(ssval, playerFrom, playerTo);


	//show the message we sent on our local console as well (even in skirmish, if player plays as this AI)
	if(playerFrom == selectedPlayer)
	{
		sprintf(tmp,"[%d-%d] : %s",playerFrom, playerTo, ssval);											// add message
		addConsoleMessage(tmp, RIGHT_JUSTIFY);
	}

	return TRUE;
}

BOOL scrDbg(void)
{
	char	*ssval=NULL;
	SDWORD	player;


	if (!stackPopParams(2, VAL_STRING, &ssval, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrDbg(): stack failed");
		return FALSE;
	}

	if(scrDebug[player])
	{
		char	sTmp[255];
		sprintf(sTmp,"%d) %s",player,ssval);
		addConsoleMessage(sTmp,DEFAULT_JUSTIFY);
	}

	return TRUE;
}

BOOL scrDebugFile(void)
{
	char	*ssval=NULL;

	if (!stackPopParams(1, VAL_STRING, &ssval))
	{
		debug(LOG_ERROR, "scrDebugFile(): stack failed");
		return FALSE;
	}

	debug(LOG_SCRIPT, ssval);

	return TRUE;
}

static	UDWORD			playerToEnumDroid;
static	UDWORD			playerVisibleDroid;
static	UDWORD			enumDroidCount;

/* Prepare the droid iteration */
BOOL scrInitEnumDroids(void)
{
	SDWORD	targetplayer,playerVisible;

	if ( !stackPopParams(2,  VAL_INT, &targetplayer, VAL_INT, &playerVisible) )
	{
		//DbgMsg("scrInitEnumDroids() - failed to pop params");
		return FALSE;
	}

	playerToEnumDroid	= (UDWORD)targetplayer;
	playerVisibleDroid	= (UDWORD)playerVisible;
	enumDroidCount = 0;		//returned 0 droids so far
	return TRUE;
}

/* Get next droid */
BOOL scrEnumDroid(void)
{
	UDWORD			count;
	DROID		 *psDroid;
	BOOL			found;

	count = 0;
	for(psDroid=apsDroidLists[playerToEnumDroid];psDroid && count<enumDroidCount;count++)
	{
		psDroid = psDroid->psNext;
	}


	//search the players' list of droid to see if one exists and is visible
	found = FALSE;
	while(psDroid)
	{
		if(psDroid->visible[playerVisibleDroid])
		{
			if (!stackPushResult((INTERP_TYPE)ST_DROID,(UDWORD) psDroid))			//	push result
			{
				return FALSE;
			}

			enumDroidCount++;
			return TRUE;
		}

		enumDroidCount++;
		psDroid = psDroid->psNext;
	}

	// push NULLDROID, since didn't find any
	if (!stackPushResult((INTERP_TYPE)ST_DROID, (UDWORD)NULL))
	{
		debug(LOG_ERROR, "scrEnumDroid() - push failed");
		return FALSE;
	}

	return TRUE;
}

//Return the template factory is currently building
BOOL scrFactoryGetTemplate(void)
{
	SDWORD			structure;
	STRUCTURE		*psStructure = NULL;
	DROID_TEMPLATE	*psTemplate = NULL;

	if (!stackPopParams(1, ST_STRUCTURE, &structure))
	{
		debug(LOG_ERROR, "scrFactoryGetTemplate() - stackPopParams failed");
		return FALSE;
	}

	psStructure = (STRUCTURE *)structure;

	if (psStructure == NULL)
	{
		debug(LOG_ERROR, "scrFactoryGetTemplate() - NULL factory object");
		ASSERT( FALSE, "scrFactoryGetTemplate: NULL factory object" );
		return FALSE;
	}

	ASSERT( PTRVALID(psStructure, sizeof(STRUCTURE)),
		"scrFactoryGetTemplate: Invalid structure pointer" );
	ASSERT( (psStructure->pStructureType->type == REF_FACTORY OR
		psStructure->pStructureType->type == REF_CYBORG_FACTORY OR
		psStructure->pStructureType->type == REF_VTOL_FACTORY),
		"scrFactoryGetTemplate: structure is not a factory" );

	if(!StructIsFactory(psStructure))
	{
		debug(LOG_ERROR, "scrFactoryGetTemplate: structure not a factory.");
		return FALSE;
	}

	psTemplate = (DROID_TEMPLATE *)((FACTORY*)psStructure->pFunctionality)->psSubject;

	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),
		"scrFactoryGetTemplate: Invalid template pointer" );

	if (!stackPushResult((INTERP_TYPE)ST_TEMPLATE, (UDWORD)psTemplate))
	{
		debug(LOG_ERROR, "scrFactoryGetTemplate: stackPushResult failed");
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumTemplatesInProduction(void)
{
	SDWORD			player,numTemplates = 0;
	DROID_TEMPLATE	*psTemplate;
    STRUCTURE		*psStruct;
	STRUCTURE		*psList;
	BASE_STATS		*psBaseStats;

	if (!stackPopParams(2, ST_TEMPLATE, &psTemplate, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrNumTemplatesInProduction: stackPopParams failed");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrNumTemplatesInProduction: player number is too high");
		ASSERT( FALSE, "scrNumTemplatesInProduction: player number is too high" );
		return FALSE;
	}

	ASSERT( PTRVALID(psTemplate, sizeof(DROID_TEMPLATE)),
		"scrNumTemplatesInProduction: Invalid template pointer" );

	psBaseStats = (BASE_STATS *)psTemplate; //Convert

	psList = apsStructLists[player];

	for (psStruct = psList; psStruct != NULL; psStruct = psStruct->psNext)
	{
		if (StructIsFactory(psStruct))
		{
			FACTORY *psFactory = (FACTORY *)psStruct->pFunctionality;

			//if this is the template currently being worked on
			if (psBaseStats == psFactory->psSubject)
			{
				numTemplates++;
			}
		}
	}

	if (!stackPushResult(VAL_INT, numTemplates))
	{
		debug(LOG_ERROR, "scrNumTemplatesInProduction: stackPushResult failed");
		return FALSE;
	}

	return TRUE;
}

// Returns number of units based on a component a certain player has
BOOL scrNumDroidsByComponent(void)
{
	SDWORD				player,lookingPlayer,comp;
	UDWORD				numFound;
	INTERP_VAL			sVal;
	DROID				*psDroid;

	if (!stackPopParams(2, VAL_INT, &player, VAL_INT, &lookingPlayer))
	{
		debug(LOG_ERROR, "scrNumDroidsByComponent(): stack failed");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrNumDroidsByComponent(): player number is too high");
		ASSERT( FALSE, "scrNumDroidsByComponent:player number is too high" );
		return FALSE;
	}

	if (!stackPop(&sVal))
	{
		debug(LOG_ERROR, "scrNumDroidsByComponent(): failed to pop component");
		return FALSE;
	}

	numFound = 0;

	comp = (SDWORD)sVal.v.ival;	 //cache access

	//check droids
	for(psDroid = apsDroidLists[player]; psDroid; psDroid = psDroid->psNext)
	{
		if(psDroid->visible[lookingPlayer])		//can see this droid?
		{
			switch(sVal.type)
			{
			case ST_BODY:
				if (psDroid->asBits[COMP_BODY].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_PROPULSION:
				if (psDroid->asBits[COMP_PROPULSION].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_ECM:
				if (psDroid->asBits[COMP_ECM].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_SENSOR:
				if (psDroid->asBits[COMP_SENSOR].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_CONSTRUCT:
				if (psDroid->asBits[COMP_CONSTRUCT].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_REPAIR:
				if (psDroid->asBits[COMP_REPAIRUNIT].nStat == comp)
				{
					numFound++;
				}
				break;
			case ST_WEAPON:
				if (psDroid->asWeaps[0].nStat == comp)
				{
					numFound++;
					break;
				}
				break;
			default:
				debug(LOG_ERROR, "scrNumDroidsByComponent(): unknown component type");
				ASSERT( FALSE, "scrNumDroidsByComponent: unknown component type" );
				return FALSE;
			}
		}
	}

	if (!stackPushResult(VAL_INT, numFound))
	{
		debug(LOG_ERROR, "scrNumDroidsByComponent(): stackPushResult failed");
		return FALSE;
	}

	return TRUE;
}

BOOL scrGetStructureLimit(void)
{
	SDWORD				player,limit;
	UDWORD				structInc;
	STRUCTURE_LIMITS	*psStructLimits;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &structInc, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrGetStructureLimit(): stackPopParams failed");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrGetStructureLimit(): player number is too high");
		ASSERT( FALSE, "scrSetStructureLimits: player number is too high" );
		return FALSE;}

	if (structInc > numStructureStats)
	{
		debug(LOG_ERROR, "scrGetStructureLimit(): tructure stat is too high - %d", structInc);
		ASSERT( FALSE, "scrSetStructureLimits: Structure stat is too high - %d", structInc );
		return FALSE;}

	psStructLimits = asStructLimits[player];
	limit = (SDWORD)psStructLimits[structInc].limit;

	if (!stackPushResult(VAL_INT, limit))
	{
		debug(LOG_ERROR, "scrGetStructureLimit(): stackPushResult failed");
		return FALSE;
	}

	return TRUE;
}

// Returns TRUE if limit for the passed structurestat is reached, otherwise returns FALSE
BOOL scrStructureLimitReached(void)
{
	SDWORD				player;
	BOOL				bLimit = FALSE;
	UDWORD				structInc;
	STRUCTURE_LIMITS	*psStructLimits;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &structInc, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrStructureLimitReached(): stackPopParams failed");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrStructureLimitReached(): player number is too high");
		ASSERT( FALSE, "scrSetStructureLimits: player number is too high" );
		return FALSE;
	}

	if (structInc > numStructureStats)
	{
		debug(LOG_ERROR, "scrStructureLimitReached(): Structure stat is too high - %d", structInc);
		ASSERT( FALSE, "scrSetStructureLimits: Structure stat is too high - %d", structInc );
		return FALSE;}

	psStructLimits = asStructLimits[player];

	if(psStructLimits[structInc].currentQuantity >= psStructLimits[structInc].limit) bLimit = TRUE;

	if (!stackPushResult(VAL_BOOL, bLimit))
	{
		debug(LOG_ERROR, "scrStructureLimitReached(): stackPushResult failed");
		return FALSE;
	}

	return TRUE;
}

// How many structures of a given type a player has
BOOL scrGetNumStructures(void)
{
	SDWORD				player,numStructures;
	UDWORD				structInc;
	STRUCTURE_LIMITS	*psStructLimits;

	if (!stackPopParams(2, ST_STRUCTURESTAT, &structInc, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrSetStructureLimits: failed to pop");
		return FALSE;}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrSetStructureLimits:player number is too high");
		return FALSE;}

	if (structInc > numStructureStats)
	{
		debug(LOG_ERROR, "scrSetStructureLimits: Structure stat is too high");
		return FALSE;}

	psStructLimits = asStructLimits[player];
	numStructures = (SDWORD)psStructLimits[structInc].currentQuantity;

	if (!stackPushResult(VAL_INT, numStructures))
	{
		return FALSE;
	}

	return TRUE;
}

// Return player's unit limit
BOOL scrGetUnitLimit(void)
{
	SDWORD				player,limit;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrGetUnitLimit: failed to pop");
		return FALSE;}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetStructureLimits:player number is too high" );
		return FALSE;}

	limit = getMaxDroids(player);

	if (!stackPushResult(VAL_INT, limit))
	{
		return FALSE;
	}

	return TRUE;
}

// Return minimum of 2 vals
BOOL scrMin(void)
{
	SDWORD				val1,val2;

	if (!stackPopParams(2, VAL_INT, &val1, VAL_INT, &val2))
	{
		return FALSE;}

		if (!stackPushResult(VAL_INT, MIN(val2, val1)))
	{
		return FALSE;
	}

	return TRUE;
}

// Return maximum of 2 vals
BOOL scrMax(void)
{
	SDWORD				val1,val2;

	if (!stackPopParams(2, VAL_INT, &val1, VAL_INT, &val2))
	{
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, MAX(val1, val2)))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL ThreatInRange(SDWORD player, SDWORD range, SDWORD rangeX, SDWORD rangeY, BOOL bVTOLs)
{
	UDWORD				i,structType,tx,ty;
	STRUCTURE			*psStruct;
	DROID				*psDroid;

	tx = rangeX >> TILE_SHIFT;
	ty = rangeY >> TILE_SHIFT;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))
		{
			continue;
		}

		//check structures
		for(psStruct = apsStructLists[i]; psStruct; psStruct=psStruct->psNext)
		{
			if(psStruct->visible[player])	//if can see it
			{
				structType = psStruct->pStructureType->type;

				switch(structType)		//dangerous to get near these structures
				{
					case REF_DEFENSE:
					case REF_CYBORG_FACTORY:
					case REF_FACTORY:
					case REF_VTOL_FACTORY:
					case REF_REARM_PAD:

					if((range < 0) || ((dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT)
						<< TILE_SHIFT) < range))	//enemy in range
					{
						return TRUE;
					}

					break;
				}
			}
		}

		//check droids
		for(psDroid = apsDroidLists[i]; psDroid; psDroid = psDroid->psNext)
		{
			if(psDroid->visible[player])		//can see this droid?
			{
				if (psDroid->droidType != DROID_WEAPON &&
					psDroid->droidType != DROID_PERSON &&
					psDroid->droidType != DROID_CYBORG &&
					psDroid->droidType != DROID_CYBORG_SUPER)
				{
					continue;
				}

				//if VTOLs are excluded, skip them
				if(!bVTOLs && ((asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == LIFT) || (psDroid->droidType == DROID_TRANSPORTER)))
				{
					continue;
				}

				if((range < 0) || ((dirtySqrt(tx, ty , psDroid->x >> TILE_SHIFT, psDroid->y >> TILE_SHIFT)
					<< TILE_SHIFT) < range))	//enemy in range
				{
					return TRUE;
				}
			}
		}
	}

	return FALSE;
}

//find unrevealed tile closest to pwLooker within the range of wRange
BOOL scrFogTileInRange(void)
{
	SDWORD		pwLookerX,pwLookerY,tBestX,tBestY,threadRange;
	SDWORD		wRangeX,wRangeY,tRangeX,tRangeY,wRange,player;
	UDWORD		tx,ty,i,j,wDist,wBestDist;
	MAPTILE		*psTile;
	BOOL		ok = FALSE;
	SDWORD		*wTileX,*wTileY;

	if (!stackPopParams(9, VAL_REF|VAL_INT, &wTileX, VAL_REF|VAL_INT, &wTileY,
		VAL_INT, &pwLookerX, VAL_INT, &pwLookerY, VAL_INT, &wRangeX, VAL_INT, &wRangeY,
		VAL_INT, &wRange, VAL_INT, &player, VAL_INT, &threadRange))
	{
		debug(LOG_ERROR, "scrFogTileInRange: failed to pop");
		return FALSE;}

    //Check coords
	if(		pwLookerX < 0
		||	pwLookerX > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	pwLookerY < 0
		||	pwLookerY > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR, "scrFogTileInRange: coords off map");
		return FALSE;
	}

	tRangeX = wRangeX >> TILE_SHIFT;				//cache to tile coords, for faster calculations
	tRangeY = wRangeY >> TILE_SHIFT;

	tx = pwLookerX >> TILE_SHIFT;					// change to tile coords.
	ty = pwLookerY >> TILE_SHIFT;

	wBestDist = 99999;
	tBestX = -1; tBestY = -1;

	for(i=0; i<mapWidth;i++)
	{
		for(j=0; j<mapHeight; j++)
		{
			psTile = mapTile(i,j);
		   	if(!TEST_TILE_VISIBLE(player,psTile))	//not vis
		  	{
				//within base range
				if((wRange <= 0 ) || ((dirtySqrt(tRangeX, tRangeY, i, j) << TILE_SHIFT) < wRange))		//dist in world units between baseX/baseY and the tile
				{
					//calc dist between this tile and looker
					wDist = (dirtySqrt(tx, ty, i, j) << TILE_SHIFT);

					//closer than last one?
					if(wDist < wBestDist)
					{
						//tmpX = i;
						//tmpY = j;
						//if(pickATileGen(&tmpX, &tmpY, 4,zonedPAT))	//can reach (don't need many passes)
						if(zonedPAT(i,j))	//Can reach this tile
						{
							//if((tmpX == i) && (tmpY == j))	//can't allow to change coords, otherwise might send to the same unrevealed tile next time
															//and units will stuck forever
							//{
							if((threadRange <= 0) || (!ThreatInRange(player, threadRange, i << TILE_SHIFT, j << TILE_SHIFT, FALSE)))
							{
									wBestDist = wDist;
									tBestX = i;
									tBestY = j;
									ok = TRUE;
							}
							//}
						}
					}
				}
		  	}
		}
	}

	if(ok)	//something found
	{
		*wTileX = tBestX<<TILE_SHIFT;
		*wTileY = tBestY<<TILE_SHIFT;

		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			debug(LOG_ERROR, "scrFogTileInRange: stackPushResult failed (found)");
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			debug(LOG_ERROR, "scrFogTileInRange: stackPushResult failed (not found)");
			return FALSE;
		}
	}

	return TRUE;
}

BOOL scrMapRevealedInRange(void)
{
	SDWORD		wRangeX,wRangeY,tRangeX,tRangeY,wRange,player;
	UDWORD		i,j;
	MAPTILE		*psTile;

	if (!stackPopParams(4, VAL_INT, &wRangeX, VAL_INT, &wRangeY,
		VAL_INT, &wRange, VAL_INT, &player))
	{
		debug(LOG_ERROR,  "scrMapRevealedInRange: failed to pop");
		return FALSE;
	}

    //Check coords
	if(		wRangeX < 0
		||	wRangeX > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	wRangeY < 0
		||	wRangeY > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR,  "scrMapRevealedInRange: coords off map");
		return FALSE;
	}

	tRangeX = wRangeX >> TILE_SHIFT;				//cache to tile coords, for faster calculations
	tRangeY = wRangeY >> TILE_SHIFT;

	for(i=0; i<mapWidth;i++)
	{
		for(j=0; j<mapHeight; j++)
		{
			psTile = mapTile(i,j);
		   	if(TEST_TILE_VISIBLE(player,psTile))	//not vis
		  	{
				//within range
				if((dirtySqrt(tRangeX, tRangeY, i, j) << TILE_SHIFT) < wRange)		//dist in world units between x/y and the tile
				{
					if (!stackPushResult(VAL_BOOL, TRUE))
					{
						return FALSE;
					}

					return TRUE;
				}
		  	}
		}
	}

	//nothing found
	if (!stackPushResult(VAL_BOOL, FALSE))
	{
		return FALSE;
	}

	return TRUE;
}

//return number of reserach topics that are left to be researched
//for a certain technology to become available
BOOL scrNumResearchLeft(void)
{
	RESEARCH			*psResearch;
	SDWORD				player,result;
	UWORD				cur,index,tempIndex;
	SWORD				top;

	UWORD				Stack[400];

	BOOL				found;
	PLAYER_RESEARCH		*pPlayerRes;


	if (!stackPopParams(2, VAL_INT, &player, ST_RESEARCH, &psResearch ))
	{
		debug(LOG_ERROR,  "scrNumResearchLeft(): stack failed");
		return FALSE;
	}

	if(psResearch == NULL)
	{
		ASSERT( FALSE, "scrNumResearchLeft(): no such research topic" );
		return FALSE;
	}

	pPlayerRes = asPlayerResList[player];
	index = psResearch - asResearch;	//TODO: fix if needed

	if (index >= numResearch)
	{
		ASSERT( FALSE, "scrNumResearchLeft(): invalid research index" );
		return FALSE;
	}

	found = FALSE;

	if(beingResearchedByAlly(index, player))
	{
		result = 1;
	}
	else if(IsResearchCompleted(&pPlayerRes[index]))
	{
		result = 0;
	}
	else if(IsResearchStarted(&pPlayerRes[index]))
	{
		result = 1;
	}
	else if(IsResearchPossible(&pPlayerRes[index]) || IsResearchCancelled(&pPlayerRes[index]))
	{
		result = 1;
	}
	else if(skTopicAvail(index,player))
	{
		result = 1;
	}
	else
	{
		result = 1;		//init, count the top research topic as 1
		top = -1;

		cur = 0;				//start with first index's PR
		tempIndex = -1;
		while(TRUE)			//do
		{
			if(cur >= asResearch[index].numPRRequired)		//this one has no PRs or end of PRs reached
			{
				top = top - 2;
				if(top < (-1))
				{
					break;		//end of stack
				}
				index = Stack[top + 2];	//if index = -1, then exit
				cur = Stack[top + 1];		//go to next PR of the last node

			}
			else		//end of PRs not reached
			{
				result += asResearch[index].numPRRequired;		//add num of PRs this topic has

				tempIndex = asResearch[index].pPRList[cur];		//get cur node's index

				//decide if has to check its PRs
				if(!IsResearchCompleted(&pPlayerRes[tempIndex]) &&	//don't touch if completed already
					!skTopicAvail(index,player) &&					//has no unresearched PRs left if available
					!beingResearchedByAlly(index, player))			//will become available soon anyway
				{
					if(asResearch[tempIndex].numPRRequired > 0)	//node has any nodes itself
					{
						Stack[top+1] = cur;								//so can go back to it further
						Stack[top+2] = index;
						top = top + 2;

						index = tempIndex;		//go 1 level further
						cur = -1;									//start with first PR of this PR next time
					}
				}
			}

			cur++;				//try next node of the main node
			if((cur >= asResearch[index].numPRRequired) && (top <= (-1)))	//nothing left
			{
				break;
			}

		}
	}


	if (!stackPushResult(VAL_INT, result))
	{
		return FALSE;
	}

	return TRUE;
}

//check if any of the ally is researching this topic
BOOL beingResearchedByAlly(SDWORD resIndex, SDWORD player)
{
	STRUCTURE *psOtherStruct;
	SDWORD	i;
	BASE_STATS *Stat;

	Stat = (BASE_STATS*)(asResearch + resIndex);

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if(i != player && aiCheckAlliances(player,i))
		{
			//check each research facility to see if they are doing this topic.
			for(psOtherStruct=apsStructLists[i];psOtherStruct;psOtherStruct=psOtherStruct->psNext)
			{
				if(   psOtherStruct->pStructureType->type == REF_RESEARCH
						&& psOtherStruct->status == SS_BUILT
						&& ((RESEARCH_FACILITY *)psOtherStruct->pFunctionality)->psSubject
						 )
				{

					if(((RESEARCH_FACILITY *)psOtherStruct->pFunctionality)->psSubject->ref == Stat->ref)
					{
						return TRUE;
					}
				}
			}

		}
	}

	return FALSE;
}

// TRUE if player has completed this research
BOOL scrResearchCompleted(void)
{
	RESEARCH			*psResearch;
	SDWORD				player;
	UWORD				index;
	PLAYER_RESEARCH		*pPlayerRes;

	if (!stackPopParams(2,ST_RESEARCH, &psResearch, VAL_INT, &player ))
	{
		debug(LOG_ERROR,   "scrResearchCompleted(): stack failed");
		return FALSE;
	}

	if(psResearch == NULL)
	{
		ASSERT( FALSE, ": no such research topic" );
		return FALSE;
	}

	pPlayerRes = asPlayerResList[player];
	index = psResearch - asResearch;	//TODO: fix if needed

	if (index >= numResearch)
	{
		ASSERT( FALSE, "scrResearchCompleted: invalid research index" );
		return FALSE;
	}

	if(IsResearchCompleted(&pPlayerRes[index]))
	{
		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}
	}

	return TRUE;
}

// TRUE if player has already started researching it
BOOL scrResearchStarted(void)
{
	RESEARCH			*psResearch;
	SDWORD				player;
	UWORD				index;
	PLAYER_RESEARCH		*pPlayerRes;

	if (!stackPopParams(2,ST_RESEARCH, &psResearch, VAL_INT, &player ))
	{
		debug(LOG_ERROR,  "scrResearchStarted(): stack failed");
		return FALSE;
	}

	if(psResearch == NULL)
	{
		ASSERT( FALSE, ": no such research topic" );
		return FALSE;
	}

	pPlayerRes = asPlayerResList[player];
	index = psResearch - asResearch;	//TODO: fix if needed

	if (index >= numResearch)
	{
		ASSERT( FALSE, "scrResearchCompleted: invalid research index" );
		return FALSE;
	}

	if(IsResearchStarted(&pPlayerRes[index]))
	{
		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}
	}

	return TRUE;
}

//returns TRUE if location is dangerous
BOOL scrThreatInRange(void)
{
	SDWORD				player,range,rangeX,rangeY;
	BOOL				threat,bVTOLs;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,  "scrThreatInRange(): stack failed");
		return FALSE;
	}

	threat = ThreatInRange(player, range, rangeX, rangeY, bVTOLs);
	if (!stackPushResult(VAL_BOOL, threat))
	{
		return FALSE;
	}

	return TRUE;
}


BOOL scrNumEnemyWeapObjInRange(void)
{
	SDWORD				lookingPlayer,range,rangeX,rangeY,i;
	UDWORD				numEnemies = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(5, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,  "scrNumEnemyWeapObjInRange(): stack failed");
		return FALSE;
	}


	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[lookingPlayer][i] == ALLIANCE_FORMED) || (i == lookingPlayer))	//skip allies and myself
		{
			continue;
		}

		numEnemies = numEnemies + numPlayerWeapDroidsInRange(i, lookingPlayer, range, rangeX, rangeY, bVTOLs);
		numEnemies = numEnemies + numPlayerWeapStructsInRange(i, lookingPlayer, range, rangeX, rangeY);
	}

	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR, "scrNumEnemyWeapObjInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

UDWORD numPlayerWeapDroidsInRange(SDWORD player, SDWORD lookingPlayer, SDWORD range, SDWORD rangeX, SDWORD rangeY, BOOL bVTOLs)
{
	UDWORD				tx,ty,numEnemies;
	DROID				*psDroid;

	tx = rangeX >> TILE_SHIFT;
	ty = rangeY >> TILE_SHIFT;

	numEnemies = 0;

	//check droids
	for(psDroid = apsDroidLists[player]; psDroid; psDroid = psDroid->psNext)
	{
		if(psDroid->visible[lookingPlayer])		//can see this droid?
		{
			if (psDroid->droidType != DROID_WEAPON &&
				psDroid->droidType != DROID_PERSON &&
				psDroid->droidType != DROID_CYBORG &&
				psDroid->droidType != DROID_CYBORG_SUPER)
			{
				continue;
			}

			//if VTOLs are excluded, skip them
			if(!bVTOLs && ((asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == LIFT) || (psDroid->droidType == DROID_TRANSPORTER)))
			{
				continue;
			}

			if((range < 0) || ((dirtySqrt(tx, ty , psDroid->x >> TILE_SHIFT, psDroid->y >> TILE_SHIFT)
				<< TILE_SHIFT) < range))	//enemy in range
			{
				numEnemies++;
			}
		}
	}

	return numEnemies;
}



UDWORD numPlayerWeapStructsInRange(SDWORD player, SDWORD lookingPlayer, SDWORD range, SDWORD rangeX, SDWORD rangeY)
{
	UDWORD				tx,ty,numEnemies;
	STRUCTURE			*psStruct;

	tx = rangeX >> TILE_SHIFT;
	ty = rangeY >> TILE_SHIFT;

	numEnemies = 0;

	//check structures
	for(psStruct = apsStructLists[player]; psStruct; psStruct=psStruct->psNext)
	{
		if(psStruct->visible[lookingPlayer])	//if can see it
		{
			if(psStruct->pStructureType->type == REF_DEFENSE)
			{
				if((range < 0) || ((dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT)
					<< TILE_SHIFT) < range))	//enemy in range
				{
					numEnemies++;
				}
			}
		}
	}

	return numEnemies;
}

BOOL scrNumEnemyWeapDroidsInRange(void)
{
	SDWORD				lookingPlayer,range,rangeX,rangeY,i;
	UDWORD				numEnemies = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(5, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,  "scrNumEnemyWeapDroidsInRange(): stack failed");
		return FALSE;
	}

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[lookingPlayer][i] == ALLIANCE_FORMED) || (i == lookingPlayer))	//skip allies and myself
		{
			continue;
		}

		numEnemies = numEnemies + numPlayerWeapDroidsInRange(i, lookingPlayer, range, rangeX, rangeY, bVTOLs);
	}

	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR,  "scrNumEnemyWeapDroidsInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}



BOOL scrNumEnemyWeapStructsInRange(void)
{
	SDWORD				lookingPlayer,range,rangeX,rangeY,i;
	UDWORD				numEnemies = 0;

	if (!stackPopParams(4, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL))
	{
		debug(LOG_ERROR,  "scrNumEnemyWeapStructsInRange(): stack failed");
		return FALSE;
	}

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[lookingPlayer][i] == ALLIANCE_FORMED) || (i == lookingPlayer))	//skip allies and myself
		{
			continue;
		}

		numEnemies = numEnemies + numPlayerWeapStructsInRange(i, lookingPlayer, range, rangeX, rangeY);
	}

	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR, "scrNumEnemyWeapStructsInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumFriendlyWeapObjInRange(void)
{
	SDWORD				player,range,rangeX,rangeY,i;
	UDWORD				numFriends = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,  "scrNumFriendlyWeapObjInRange(): stack failed");
		return FALSE;
	}

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))	//skip enemies
		{
			numFriends = numFriends + numPlayerWeapDroidsInRange(i, player, range, rangeX, rangeY, bVTOLs);
			numFriends = numFriends + numPlayerWeapStructsInRange(i, player, range, rangeX, rangeY);
		}
	}

	if (!stackPushResult(VAL_INT, numFriends))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumFriendlyWeapDroidsInRange(void)
{
	SDWORD				lookingPlayer,range,rangeX,rangeY,i;
	UDWORD				numEnemies = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(5, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,  "scrNumFriendlyWeapDroidsInRange(): stack failed");
		return FALSE;
	}

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[lookingPlayer][i] == ALLIANCE_FORMED) || (i == lookingPlayer))
		{
			numEnemies = numEnemies + numPlayerWeapDroidsInRange(i, lookingPlayer, range, rangeX, rangeY, bVTOLs);
		}
	}

	//numEnemies = numEnemyWeapObjInRange(player, range, rangeX, rangeY, bVTOLs);
	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR, "scrNumFriendlyWeapDroidsInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}



BOOL scrNumFriendlyWeapStructsInRange(void)
{
	SDWORD				lookingPlayer,range,rangeX,rangeY,i;
	UDWORD				numEnemies = 0;

	if (!stackPopParams(4, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL))
	{
		debug(LOG_ERROR, "scrNumFriendlyWeapStructsInRange(): stack failed");
		return FALSE;
	}

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[lookingPlayer][i] == ALLIANCE_FORMED) || (i == lookingPlayer))	//skip enemies
		{
			numEnemies = numEnemies + numPlayerWeapStructsInRange(i, lookingPlayer, range, rangeX, rangeY);
		}
	}

	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR,"scrNumFriendlyWeapStructsInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumPlayerWeapObjInRange(void)
{
	SDWORD				player,lookingPlayer,range,rangeX,rangeY;
	UDWORD				numEnemies = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(6, VAL_INT, &player, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR,"scrNumPlayerWeapObjInRange(): stack failed");
		return FALSE;
	}

	numEnemies = numEnemies + numPlayerWeapDroidsInRange(player, lookingPlayer, range, rangeX, rangeY, bVTOLs);
	numEnemies = numEnemies + numPlayerWeapStructsInRange(player, lookingPlayer, range, rangeX, rangeY);

	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR, "scrNumPlayerWeapObjInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumEnemyObjInRange(void)
{
	SDWORD				player,range,rangeX,rangeY;
	UDWORD				numEnemies = 0;
	BOOL				bVTOLs;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range, VAL_BOOL, &bVTOLs))
	{
		debug(LOG_ERROR, "scrNumEnemyObjInRange(): stack failed");
		return FALSE;
	}

	numEnemies = numEnemyObjInRange(player, range, rangeX, rangeY, bVTOLs);
	if (!stackPushResult(VAL_INT, numEnemies))
	{
		debug(LOG_ERROR, "scrNumEnemyObjInRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

UDWORD numEnemyObjInRange(SDWORD player, SDWORD range, SDWORD rangeX, SDWORD rangeY, BOOL bVTOLs)
{
	UDWORD				i,tx,ty,numEnemies;
	STRUCTURE			*psStruct;
	DROID				*psDroid;

	tx = rangeX >> TILE_SHIFT;
	ty = rangeY >> TILE_SHIFT;

	numEnemies = 0;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))
		{
			continue;
		}

		//check structures
		for(psStruct = apsStructLists[i]; psStruct; psStruct=psStruct->psNext)
		{
			if(psStruct->visible[player])	//if can see it
			{
				//if(psStruct->pStructureType->type == REF_DEFENSE)
				//{
					if((range < 0) || ((dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT)
						<< TILE_SHIFT) < range))	//enemy in range
					{
						numEnemies++;
					}
				//}
			}
		}

		//check droids
		for(psDroid = apsDroidLists[i]; psDroid; psDroid = psDroid->psNext)
		{
			if(psDroid->visible[player])		//can see this droid?
			{
				//if VTOLs are excluded, skip them
				if(!bVTOLs && ((asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == LIFT) || (psDroid->droidType == DROID_TRANSPORTER)))
				{
					continue;
				}

				if((range < 0) || ((dirtySqrt(tx, ty , psDroid->x >> TILE_SHIFT, psDroid->y >> TILE_SHIFT)
					<< TILE_SHIFT) < range))	//enemy in range
				{
					numEnemies++;
				}
			}
		}
	}

	return numEnemies;
}

/* Similiar to structureBuiltInRange(), but also returns true if structure is not finished */
BOOL scrNumStructsByStatInRange(void)
{
	SDWORD		player, lookingPlayer, index, x, y, range;
	SDWORD		rangeSquared,NumStruct;
	STRUCTURE	*psCurr;
	SDWORD		xdiff, ydiff;
	STRUCTURE_STATS *psTarget;

	if (!stackPopParams(6, ST_STRUCTURESTAT, &index, VAL_INT, &x, VAL_INT, &y,
		VAL_INT, &range, VAL_INT, &lookingPlayer, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrNumStructsByStatInRange(): stack failed");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange:player number is too high" );
		return FALSE;
	}

	if (x < (SDWORD)0 OR (x >> TILE_SHIFT) > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : invalid X coord" );
		return FALSE;
	}
	if (y < (SDWORD)0 OR (y >> TILE_SHIFT) > (SDWORD)mapHeight)
	{
		ASSERT( FALSE,"scrStructureBuiltInRange : invalid Y coord" );
		return FALSE;
	}
	if (index < (SDWORD)0 OR index > (SDWORD)numStructureStats)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : Invalid structure stat" );
		return FALSE;
	}
	if (range < (SDWORD)0)
	{
		ASSERT( FALSE, "scrStructureBuiltInRange : Rnage is less than zero" );
		return FALSE;
	}

	NumStruct = 0;

	//now look through the players list of structures to see if this type
	//exists within range
	psTarget = &asStructureStats[index];
	rangeSquared = range * range;
	for(psCurr = apsStructLists[player]; psCurr; psCurr = psCurr->psNext)
	{
		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff <= rangeSquared)
		{

#ifdef HASH_NAMES
			if( psCurr->pStructureType->NameHash == psTarget->NameHash )
#else
			if( strcmp(psCurr->pStructureType->pName,psTarget->pName) == 0 )
#endif
			{
				if(psCurr->visible[lookingPlayer])		//can we see it?
				{
					NumStruct++;
				}
			}
		}
	}

	if (!stackPushResult(VAL_INT, NumStruct))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumStructsByStatInArea(void)
{
	SDWORD		player, lookingPlayer, index, x1, y1, x2, y2;
	SDWORD		NumStruct;
	STRUCTURE	*psCurr;

	STRUCTURE_STATS		*psStats;

	if (!stackPopParams(7, ST_STRUCTURESTAT, &index, VAL_INT, &x1, VAL_INT, &y1,
		VAL_INT, &x2, VAL_INT, &y2, VAL_INT, &lookingPlayer, VAL_INT, &player))
	{
		debug(LOG_ERROR,"scrNumStructsByStatInArea: failed to pop");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrNumStructsByStatInArea: player number too high");
		ASSERT( FALSE, "scrStructureBuiltInRange:player number is too high" );
		return FALSE;
	}


	if (index < (SDWORD)0 OR index > (SDWORD)numStructureStats)
	{
		debug(LOG_ERROR, "scrNumStructsByStatInArea: invalid structure stat");
		ASSERT( FALSE, "scrStructureBuiltInRange : Invalid structure stat" );
		return FALSE;
	}

	psStats = (STRUCTURE_STATS *)(asStructureStats + index);

	ASSERT( PTRVALID(psStats, sizeof(STRUCTURE_STATS)),
			"scrNumStructsByStatInArea: Invalid structure pointer" );

	NumStruct = 0;

	for (psCurr = apsStructLists[player]; psCurr != NULL;
		psCurr = psCurr->psNext)
	{
		if (psCurr->pStructureType == psStats)
		{
			if(psCurr->visible[lookingPlayer])		//can we see it?
			{
				if(psCurr->x < x1) continue;		//not in bounds
				if(psCurr->y < y1) continue;		//not in bounds
				if(psCurr->x > x2) continue;		//not in bounds
				if(psCurr->y > y2) continue;		//not in bounds
				NumStruct++;
			}
		}
	}

	if (!stackPushResult(VAL_INT, NumStruct))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumStructsByTypeInRange(void)
{
	SDWORD		targetPlayer, lookingPlayer, type, x, y, range;
	SDWORD		rangeSquared,NumStruct;
	STRUCTURE	*psCurr;
	SDWORD		xdiff, ydiff;

	if (!stackPopParams(6, VAL_INT, &lookingPlayer, VAL_INT, &targetPlayer,
		VAL_INT, &type, VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		debug(LOG_ERROR,"scrNumStructsByTypeInRange: failed to pop");
		return FALSE;
	}

	if (lookingPlayer >= MAX_PLAYERS || targetPlayer >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumStructsByTypeInRange:player number is too high" );
		return FALSE;
	}

	if (x < (SDWORD)0 OR (x >> TILE_SHIFT) > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "scrNumStructsByTypeInRange : invalid X coord" );
		return FALSE;
	}

	if (y < (SDWORD)0 OR (y >> TILE_SHIFT) > (SDWORD)mapHeight)
	{
		ASSERT( FALSE,"scrNumStructsByTypeInRange : invalid Y coord" );
		return FALSE;
	}

	if (range < (SDWORD)0)
	{
		ASSERT( FALSE, "scrNumStructsByTypeInRange : Rnage is less than zero" );
		return FALSE;
	}

	NumStruct = 0;

	//now look through the players list of structures to see if this type
	//exists within range
	rangeSquared = range * range;
	for(psCurr = apsStructLists[targetPlayer]; psCurr; psCurr = psCurr->psNext)
	{
		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff <= rangeSquared)
		{
			if((type < 0) ||(psCurr->pStructureType->type == type))
			{
				if(psCurr->visible[lookingPlayer])		//can we see it?
				{
					NumStruct++;
				}
			}
		}
	}

	if (!stackPushResult(VAL_INT, NumStruct))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrNumFeatByTypeInRange(void)
{
	SDWORD		lookingPlayer, type, x, y, range;
	SDWORD		rangeSquared,NumFeat;
	FEATURE		*psCurr;
	SDWORD		xdiff, ydiff;

	if (!stackPopParams(5, VAL_INT, &lookingPlayer,
		VAL_INT, &type, VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		debug(LOG_ERROR, "scrNumFeatByTypeInRange(): failed to pop");
		return FALSE;
	}

	if (lookingPlayer >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrNumFeatByTypeInRange:player number is too high" );
		return FALSE;
	}

	if (x < (SDWORD)0 OR (x >> TILE_SHIFT) > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "scrNumFeatByTypeInRange : invalid X coord" );
		return FALSE;
	}

	if (y < (SDWORD)0 OR (y >> TILE_SHIFT) > (SDWORD)mapHeight)
	{
		ASSERT( FALSE,"scrNumFeatByTypeInRange : invalid Y coord" );
		return FALSE;
	}

	if (range < (SDWORD)0)
	{
		ASSERT( FALSE, "scrNumFeatByTypeInRange : Rnage is less than zero" );
		return FALSE;
	}

	NumFeat = 0;

	//now look through the players list of structures to see if this type
	//exists within range
	rangeSquared = range * range;
	for(psCurr = apsFeatureLists[0]; psCurr; psCurr = psCurr->psNext)
	{
		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff <= rangeSquared)
		{
			if((type < 0) ||(psCurr->psStats->subType == type))	//like FEAT_OIL_RESOURCE
			{
				if(psCurr->visible[lookingPlayer])		//can we see it?
				{
					NumFeat++;
				}
			}
		}
	}

	if (!stackPushResult(VAL_INT, NumFeat))
	{
		return FALSE;
	}

	return TRUE;
}

//returns num of visible structures of a certain player in range (only visible ones)
BOOL scrNumStructsButNotWallsInRangeVis(void)
{
	SDWORD		player, lookingPlayer, x, y, range;
	SDWORD		rangeSquared,NumStruct;
	STRUCTURE	*psCurr;
	SDWORD		xdiff, ydiff;

	if (!stackPopParams(5, VAL_INT, &x, VAL_INT, &y,
		VAL_INT, &range, VAL_INT, &lookingPlayer, VAL_INT, &player))
	{
		debug(LOG_ERROR,"scrNumStructsButNotWallsInRangeVis: failed to pop");
		return FALSE;
	}

	if ((player >= MAX_PLAYERS) || (lookingPlayer >= MAX_PLAYERS))
	{
		ASSERT( FALSE, "scrNumStructsButNotWallsInRangeVis:player number is too high" );
		return FALSE;
	}

	if (x < (SDWORD)0 OR (x >> TILE_SHIFT) > (SDWORD)mapWidth)
	{
		ASSERT( FALSE, "scrNumStructsButNotWallsInRangeVis : invalid X coord" );
		return FALSE;
	}
	if (y < (SDWORD)0 OR (y >> TILE_SHIFT) > (SDWORD)mapHeight)
	{
		ASSERT( FALSE,"scrNumStructsButNotWallsInRangeVis : invalid Y coord" );
		return FALSE;
	}
	if (range < (SDWORD)0)
	{
		ASSERT( FALSE, "scrNumStructsButNotWallsInRangeVis : Rnage is less than zero" );
		return FALSE;
	}

	NumStruct = 0;

	//now look through the players list of structures
	rangeSquared = range * range;
	for(psCurr = apsStructLists[player]; psCurr; psCurr = psCurr->psNext)
	{
		if ((psCurr->pStructureType->type != REF_WALL) &&
		(psCurr->pStructureType->type != REF_WALLCORNER))
		{
			if(psCurr->visible[lookingPlayer])		//can we see it?
			{
				xdiff = (SDWORD)psCurr->x - x;
				ydiff = (SDWORD)psCurr->y - y;
				if (xdiff*xdiff + ydiff*ydiff <= rangeSquared)
				{
					NumStruct++;
				}
			}
		}
	}

	if (!stackPushResult(VAL_INT, NumStruct))
	{
		return FALSE;
	}

	return TRUE;
}

// Only returns structure if it is visible
BOOL scrGetStructureVis(void)
{
	SDWORD				player, lookingPlayer, index;
	STRUCTURE			*psStruct;
	UDWORD				structType;
	BOOL				found;

	if (!stackPopParams(3, ST_STRUCTURESTAT, &index, VAL_INT, &player, VAL_INT, &lookingPlayer))
	{
		debug(LOG_ERROR,"scrGetStructureVis: failed to pop");
		return FALSE;
	}

	if ((player >= MAX_PLAYERS) || (lookingPlayer >= MAX_PLAYERS))
	{
		ASSERT( FALSE, "scrGetStructureVis:player number is too high" );
		return FALSE;
	}

	structType = asStructureStats[index].ref;

	//search the players' list of built structures to see if one exists
	found = FALSE;
	for (psStruct = apsStructLists[player]; psStruct != NULL; psStruct =
		psStruct->psNext)
	{
		if (psStruct->pStructureType->ref == structType)
		{
			if(psStruct->visible[lookingPlayer])
			{
				found = TRUE;
				break;
			}
		}
	}

	//make sure pass NULL back if not got one
	if (!found)
	{
		psStruct = NULL;
	}

	if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (UDWORD)psStruct))
	{
		return FALSE;
	}

	return TRUE;
}

//returns num of visible structures of a certain player in range
BOOL scrChooseValidLoc(void)
{
	SDWORD sendY, sendX, *x, *y, player, threatRange;
	UDWORD tx,ty;

	if (!stackPopParams(6, VAL_REF|VAL_INT, &x, VAL_REF|VAL_INT, &y,
		VAL_INT, &sendX, VAL_INT, &sendY, VAL_INT, &player, VAL_INT, &threatRange))
	{
		debug(LOG_ERROR,"scrChooseValidLoc: failed to pop");
		return FALSE;
	}

    //Check coords
	if(		sendX < 0
		||	sendX > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	sendY < 0
		||	sendY > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR, "scrChooseValidLoc: coords off map");
		return FALSE;
	}

	tx = (sendX >> TILE_SHIFT);
	ty = (sendY >> TILE_SHIFT);

	if(pickATileGenThreat(&tx, &ty, LOOK_FOR_EMPTY_TILE, threatRange, player, zonedPAT))
	{
		*x = (tx << TILE_SHIFT);
		*y = (ty << TILE_SHIFT);
		if (!stackPushResult(VAL_BOOL, TRUE))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}
	}

	return TRUE;
}

//returns closest enemy object
BOOL scrGetClosestEnemy(void)
{
	SDWORD				x,y,tx,ty, player, range,i;
	UDWORD				dist,bestDist;
	BOOL				weaponOnly, bVTOLs, bFound = FALSE;	//only military objects?
	BASE_OBJECT			*psObj;
	STRUCTURE			*psStruct;
	DROID				*psDroid;

	if (!stackPopParams(6, VAL_INT, &x, VAL_INT, &y,
		 VAL_INT, &range,  VAL_BOOL, &weaponOnly, VAL_BOOL, &bVTOLs, VAL_INT, &player))
	{
		debug(LOG_ERROR,"scrGetClosestEnemy: stack failed");
		return FALSE;
	}

    //Check coords
	if(		x < 0
		||	x > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	y < 0
		||	y > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR,"scrGetClosestEnemy: coords off map");
		return FALSE;
	}

	tx = x >> TILE_SHIFT;
	ty = y >> TILE_SHIFT;

	bestDist = 99999;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))
		{
			continue;
		}


		//check droids
		for(psDroid = apsDroidLists[i]; psDroid; psDroid = psDroid->psNext)
		{
			if(psDroid->visible[player])		//can see this droid?
			{
				//if only weapon droids and don't have it, then skip
				if (weaponOnly &&
				(	psDroid->droidType != DROID_WEAPON &&
					psDroid->droidType != DROID_PERSON &&
					psDroid->droidType != DROID_CYBORG &&
					psDroid->droidType != DROID_CYBORG_SUPER))
				{
					continue;
				}

				//if VTOLs are excluded, skip them
				if(!bVTOLs && ((asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == LIFT) || (psDroid->droidType == DROID_TRANSPORTER)))
				{
					continue;
				}

				dist = dirtySqrt(tx, ty , psDroid->x >> TILE_SHIFT, psDroid->y >> TILE_SHIFT) << TILE_SHIFT;
				if(dist < bestDist)
				{
					if((range < 0) || (dist < range))	//enemy in range
					{
						bestDist = dist;
						bFound = TRUE;
						psObj = (BASE_OBJECT*)psDroid;
					}
				}
			}
		}


		//check structures
		for(psStruct = apsStructLists[i]; psStruct; psStruct=psStruct->psNext)
		{
			if(psStruct->visible[player])	//if can see it
			{
				//only need defenses?
				if(weaponOnly && ((psStruct->pStructureType->type != REF_DEFENSE) || (psStruct->status != SS_BUILT) ))	//non-weapon-structures	or not finished
				{
					continue;
				}

				dist = dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT) << TILE_SHIFT;
				if(dist < bestDist)
				{
					if((range < 0) || (dist < range))	//in range
					{
						bestDist = dist;
						bFound = TRUE;
						psObj = (BASE_OBJECT*)psStruct;
					}
				}
			}
		}

	}

	if(bFound)
	{
		if (!stackPushResult((INTERP_TYPE)ST_BASEOBJECT, (SDWORD)psObj))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult((INTERP_TYPE)ST_BASEOBJECT, (SDWORD)NULL))
		{
			return FALSE;
		}
	}

	return TRUE;
}

//How many droids can it still fit?
BOOL scrTransporterCapacity(void)
{
	DROID			*psDroid;

	if (!stackPopParams(1, ST_DROID, &psDroid))
	{
		debug(LOG_ERROR, "scrTransporterCapacity(): failed to pop params");
		return FALSE;
	}

	if(psDroid == NULL)
	{
		debug(LOG_ERROR,"scrTransporterCapacity(): NULLOBJECT passed");
		return FALSE;
	}

	if(psDroid->droidType != DROID_TRANSPORTER)
	{
		debug(LOG_ERROR, "scrTransporterCapacity(): passed droid is not a transporter");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, calcRemainingCapacity(psDroid)))
	{
		debug(LOG_ERROR, "scrHasIndirectWeapon(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

//is it?
BOOL scrTransporterFlying(void)
{
	DROID			*psDroid;

	if (!stackPopParams(1, ST_DROID, &psDroid))
	{
		debug(LOG_ERROR, "scrTransporterFlying(): failed to pop params");
		return FALSE;
	}

	if(psDroid == NULL)
	{
		debug(LOG_ERROR,"scrTransporterFlying(): NULLOBJECT passed");
		return FALSE;
	}

	if(psDroid->droidType != DROID_TRANSPORTER)
	{
		debug(LOG_ERROR,"scrTransporterFlying(): passed droid is not a transporter");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, (SDWORD)transporterFlying(psDroid)))
	{
		debug(LOG_ERROR,"scrTransporterFlying(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

BOOL scrUnloadTransporter(void)
{
	DROID			*psDroid;
	SDWORD			x,y;

	if (!stackPopParams(3, ST_DROID, &psDroid, VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR,"scrUnloadTransporter(): failed to pop params");
		return FALSE;
	}

	if(psDroid == NULL)
	{
		debug(LOG_ERROR,"scrUnloadTransporter(): NULLOBJECT passed");
		return FALSE;
	}

	if(psDroid->droidType != DROID_TRANSPORTER)
	{
		debug(LOG_ERROR,"scrUnloadTransporter(): passed droid is not a transporter");
		return FALSE;
	}

	unloadTransporter(psDroid,x,y, FALSE);

	return TRUE;
}

//return true if droid is a member of any group
BOOL scrHasGroup(void)
{
	DROID			*psDroid;
	BOOL			retval;

	if (!stackPopParams(1, ST_DROID, &psDroid))
	{
		debug( LOG_ERROR,"scrHasGroup: failed to pop" );
		return FALSE;
	}

	if (psDroid == NULL)
	{
		debug( LOG_ERROR, "scrHasGroup: droid is NULLOBJECT" );
		return FALSE;
	}

	if (psDroid->psGroup != NULL)
	{
		retval = TRUE;
	}
	else
	{
		retval = FALSE;
	}

	if (!stackPushResult(VAL_BOOL, retval))
	{
		return FALSE;
	}

	return TRUE;
}

/* Range is in world units! */
BOOL scrObjWeaponMaxRange(void)
{
	BASE_OBJECT			*psObj;
	WEAPON_STATS		*psStats;
	DROID				*psDroid;
	STRUCTURE			*psStruct;

	if (!stackPopParams(1, ST_BASEOBJECT, &psObj))
	{
		debug(LOG_ERROR, "scrObjWeaponMaxRange: stack failed");
		return FALSE;
	}

	//check if valid type
	if(psObj->type == OBJ_DROID)
	{
		psDroid = (DROID*)psObj;
		if (psDroid->asWeaps[0].nStat != 0)
		{
			psStats = asWeaponStats + psDroid->asWeaps[0].nStat;
			if (!stackPushResult(VAL_INT, psStats->longRange))
			{
				return FALSE;
			}

			return TRUE;
		}
	}
	else if(psObj->type == OBJ_STRUCTURE)
	{
		psStruct = (STRUCTURE*)psObj;
		if (psStruct->asWeaps[0].nStat != 0)
		{
			psStats = asWeaponStats + psStruct->asWeaps[0].nStat;
			if (!stackPushResult(VAL_INT, psStats->longRange))
			{
				return FALSE;
			}

			return TRUE;
		}
	}

	if (!stackPushResult(VAL_INT, (-1)))
	{
		debug(LOG_ERROR,"scrObjWeaponMaxRange: wrong object type");
		return FALSE;
	}

	return TRUE;
}

BOOL scrObjHasWeapon(void)
{
	BASE_OBJECT			*psObj;
	DROID				*psDroid;
	STRUCTURE			*psStruct;

	if (!stackPopParams(1, ST_BASEOBJECT, &psObj))
	{
		debug(LOG_ERROR, "scrObjHasWeapon: stack failed");
		return FALSE;
	}

	//check if valid type
	if(psObj->type == OBJ_DROID)
	{
		psDroid = (DROID*)psObj;
		if (psDroid->asWeaps[0].nStat != 0)
		{
			if (!stackPushResult(VAL_BOOL, TRUE))
			{
				return FALSE;
			}

			return TRUE;
		}
	}
	else if(psObj->type == OBJ_STRUCTURE)
	{
		psStruct = (STRUCTURE*)psObj;
		if (psStruct->asWeaps[0].nStat != 0)
		{
			if (!stackPushResult(VAL_BOOL, TRUE))
			{
				return FALSE;
			}

			return TRUE;
		}
	}

	if (!stackPushResult(VAL_BOOL, FALSE))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrObjectHasIndirectWeapon(void)
{
	WEAPON_STATS	*psWeapStats;
	BOOL			bIndirect;
	BASE_OBJECT		*psObj;

	if (!stackPopParams(1, ST_BASEOBJECT, &psObj))
	{
		debug(LOG_ERROR, "scrHasIndirectWeapon(): failed to pop params");
		return FALSE;
	}

	if (psObj == NULL)
	{
		debug(LOG_ERROR,"scrHasIndirectWeapon(): NULLOBJECT passed");
		return FALSE;
	}

	bIndirect = FALSE;
	if(psObj->type == OBJ_DROID)
	{
		if (((DROID *)psObj)->asWeaps[0].nStat > 0)
		{
			psWeapStats = asWeaponStats + ((DROID *)psObj)->asWeaps[0].nStat;
			bIndirect = !proj_Direct(psWeapStats);
		}
	}
	else if(psObj->type == OBJ_STRUCTURE)
	{
		if (((STRUCTURE *)psObj)->asWeaps[0].nStat > 0)
		{
			psWeapStats = asWeaponStats + ((STRUCTURE *)psObj)->asWeaps[0].nStat;
			bIndirect = !proj_Direct(psWeapStats);
		}
	}

	if (!stackPushResult(VAL_BOOL, bIndirect))
	{
		debug(LOG_ERROR,"scrHasIndirectWeapon(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

//returns closest droid by type
BOOL scrGetClosestEnemyDroidByType(void)
{
	SDWORD				x,y,tx,ty, player, range,i,type;
	UDWORD				dist,bestDist;
	BOOL				bFound = FALSE;	//only military objects?
	BOOL				bVTOLs;
	DROID				*psDroid,*foundDroid;

	if (!stackPopParams(6, VAL_INT, &x, VAL_INT, &y,
		 VAL_INT, &range,  VAL_INT, &type, VAL_BOOL, &bVTOLs, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrGetClosestEnemyDroidByType: stack failed");
		return FALSE;
	}

    //Check coords
	if(		x < 0
		||	x > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	y < 0
		||	y > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR,"scrGetClosestEnemyDroidByType: coords off map");
		return FALSE;
	}

	tx = x >> TILE_SHIFT;
	ty = y >> TILE_SHIFT;

	bestDist = 99999;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))
		{
			continue;
		}

		//check droids
		for(psDroid = apsDroidLists[i]; psDroid; psDroid = psDroid->psNext)
		{
			//if VTOLs are excluded, skip them (don't check for transporter this time)
			if(!bVTOLs && (asPropulsionStats[psDroid->asBits[COMP_PROPULSION].nStat].propulsionType == LIFT) )
			{
				continue;
			}

			if(psDroid->visible[player])		//can see this droid?
			{
				//skip?
				if ((type != (-1)) && (psDroid->droidType != type))
				{
					continue;
				}

				dist = dirtySqrt(tx, ty , psDroid->x >> TILE_SHIFT, psDroid->y >> TILE_SHIFT) << TILE_SHIFT;
				if(dist < bestDist)
				{
					if(dist < range)	//enemy in range
					{
						bestDist = dist;
						bFound = TRUE;
						foundDroid = psDroid;
					}
				}
			}
		}
	}

	if(bFound)
	{
		if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)foundDroid))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)NULL))
		{
			return FALSE;
		}
	}

	return TRUE;
}

//returns closest structure by type
BOOL scrGetClosestEnemyStructByType(void)
{
	SDWORD				x,y,tx,ty, player, range,i,type,dist;
	UDWORD				bestDist;
	BOOL				bFound = FALSE;	//only military objects?
	STRUCTURE			*psStruct,*foundStruct;

	if (!stackPopParams(5, VAL_INT, &x, VAL_INT, &y,
		 VAL_INT, &range,  VAL_INT, &type, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrGetClosestEnemyStructByType: stack failed");
		return FALSE;
	}

    //Check coords
	if(		x < 0
		||	x > (SDWORD)(mapWidth<<TILE_SHIFT)
		||	y < 0
		||	y > (SDWORD)(mapHeight<<TILE_SHIFT) )
	{
		debug(LOG_ERROR,"scrGetClosestEnemyStructByType: coords off map");
		return FALSE;
	}

	tx = x >> TILE_SHIFT;
	ty = y >> TILE_SHIFT;

	bestDist = 99999;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if((alliances[player][i] == ALLIANCE_FORMED) || (i == player))
		{
			continue;
		}

		//check structures
		for(psStruct = apsStructLists[i]; psStruct; psStruct=psStruct->psNext)
		{
			if(psStruct->visible[player])	//if can see it
			{
				//only need defenses?
				if((type != (-1)) && (psStruct->pStructureType->type != type))	//non-weapon-structures
				{
					continue;
				}

				dist = dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT) << TILE_SHIFT;
				if(dist < bestDist)
				{
					if((range < 0) || (dist < range))	//in range or no range check
					{
						bestDist = dist;
						bFound = TRUE;
						foundStruct = psStruct;
					}
				}
			}
		}

	}

	if(bFound)
	{
		if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (SDWORD)foundStruct))
		{
			return FALSE;
		}
	}
	else
	{
		if (!stackPushResult((INTERP_TYPE)ST_STRUCTURE, (SDWORD)NULL))
		{
			return FALSE;
		}
	}

	return TRUE;
}



//Approx point of intersection of a circle and a line with start loc being circle's center point
BOOL scrCirclePerimPoint(void)
{
	SDWORD				basex,basey,*grx,*gry,radius;
	UDWORD				dist;
	float				factor,tempx,tempy;

	if (!stackPopParams(5, VAL_INT, &basex, VAL_INT, &basey, VAL_REF|VAL_INT, &grx, VAL_REF|VAL_INT, &gry,
		 VAL_INT, &radius))
	{
		debug(LOG_ERROR,"scrCirclePerimPoint(): stack failed");
		return FALSE;
	}

	if(radius == 0)
	{
		debug(LOG_ERROR,"scrCirclePerimPoint: radius == 0.");
		return TRUE;
	}

	tempx = (float)(*grx - basex);	//x len (signed!)
	tempy = (float)(*gry - basey);

	dist = dirtySqrt(basex,basey,*grx,*gry);		//len

	factor =  (float)((float)dist / (float)radius);			//by what factor is dist > radius?

	//if point was inside of the circle, don't modify passed parameter
	if(factor == 0)
	{
		printf_console("scrCirclePerimPoint: division by zero.");
		return TRUE;
	}

	//calc new len
	tempx = tempx / factor;
	tempy = tempy / factor;

	//now add new len to the center coords
	*grx = basex + (SDWORD)tempx;
	*gry = basey + (SDWORD)tempy;

	return TRUE;
}

//send my vision to AI
BOOL scrGiftRadar(void)
{
	SDWORD	playerFrom, playerTo;
	BOOL	playMsg;

	if (!stackPopParams(3, VAL_INT, &playerFrom, VAL_INT, &playerTo, VAL_BOOL, &playMsg))
	{
		debug(LOG_ERROR,"scrGiftRadar(): stack failed");
		return FALSE;
	}

	if (playerFrom >= MAX_PLAYERS || playerTo >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrGiftRadar: player out of range");
		return FALSE;
	}

	giftRadar(playerFrom,playerTo,TRUE);

	if(playMsg)
		audio_QueueTrack(ID_SENSOR_DOWNLOAD);

	return TRUE;
}

BOOL scrNumAllies(void)
{
	SDWORD			player,numAllies,i;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrNumAllies: failed to pop");
		return FALSE;
	}

	if (player < 0)
	{
		debug(LOG_ERROR, "scrNumAllies: player < 0");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrNumAllies: player index too high");
		return FALSE;
	}

	numAllies = 0;
	for(i=0;i<MAX_PLAYERS;i++)
	{
		if(i != player)
		{
			if(alliances[i][player] == ALLIANCE_FORMED)
			{
				numAllies++;
			}
		}
	}


	if (!stackPushResult(VAL_INT, numAllies))
	{
		return FALSE;
	}

	return TRUE;
}


//num aa defenses in range
BOOL scrNumAAinRange(void)
{
	SDWORD				player,lookingPlayer,range,rangeX,rangeY;
	SDWORD				tx,ty;
	UDWORD				numFound = 0;
	STRUCTURE	*psStruct;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &lookingPlayer, VAL_INT, &rangeX,
		VAL_INT, &rangeY, VAL_INT, &range))
	{
		debug(LOG_ERROR,"scrNumAAinRange(): stack failed");
		return FALSE;
	}

	tx = rangeX >> TILE_SHIFT;
	ty = rangeY >> TILE_SHIFT;

	numFound = 0;

	//check structures
	for(psStruct = apsStructLists[player]; psStruct; psStruct=psStruct->psNext)
	{
		if(psStruct->visible[lookingPlayer])	//if can see it
		{
			if((psStruct->pStructureType->type == REF_DEFENSE) &&
				(asWeaponStats[psStruct->asWeaps[0].nStat].surfaceToAir == SHOOT_IN_AIR) )
			{
				if((range < 0) || ((dirtySqrt(tx, ty, psStruct->x >> TILE_SHIFT, psStruct->y >> TILE_SHIFT)
					<< TILE_SHIFT) < range))	//enemy in range
				{
					numFound++;
				}
			}
		}
	}

	if (!stackPushResult(VAL_INT, numFound))
	{
		debug(LOG_ERROR,"scrNumAAinRange(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

//select droid
BOOL scrSelectDroid(void)
{
	BOOL	bSelect;
	DROID	*psDroid;

	if (!stackPopParams(2, ST_DROID, &psDroid, VAL_BOOL, &bSelect))
	{
		debug(LOG_ERROR, "scrSelectDroid(): stack failed");
		return FALSE;
	}

	if(psDroid == NULL)
	{
		debug(LOG_ERROR,"scrSelectDroid(): droid is NULLOBJECT");
		return FALSE;
	}

	psDroid->selected = bSelect;

	return TRUE;
}

//select droid group
BOOL scrSelectGroup(void)
{
	BOOL		bSelect;
	DROID_GROUP	*psGroup;
	DROID		*psCurr;

	if (!stackPopParams(2, ST_GROUP, &psGroup, VAL_BOOL, &bSelect))
	{
		debug(LOG_ERROR, "scrSelectGroup(): stack failed");
		return FALSE;
	}

	for(psCurr = psGroup->psList; psCurr; psCurr=psCurr->psGrpNext)
	{
		psCurr->selected = bSelect;
	}

	return TRUE;
}

BOOL scrModulo(void)
{
	SDWORD				num1,num2;

	if (!stackPopParams(2, VAL_INT, &num1, VAL_INT, &num2))
	{
		debug(LOG_ERROR,"scrModulo(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, (num1 % num2)))
	{
		debug(LOG_ERROR,"scrModulo(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

BOOL scrPlayerLoaded(void)
{
	SDWORD				player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrPlayerLoaded(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, (BOOL)(game.skDiff[player])))
	{
		debug(LOG_ERROR,"scrPlayerLoaded(): failed to push result");
		return FALSE;
	}

	return TRUE;
}


		/********************************/
		/*		AI Experience Stuff		*/
		/********************************/

//Returns enemy base x and y for a certain player
BOOL scrLearnPlayerBaseLoc(void)
{
	SDWORD				playerStoring,enemyPlayer, x, y;

	if (!stackPopParams(4, VAL_INT, &playerStoring, VAL_INT, &enemyPlayer,
						VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrLearnPlayerBaseLoc(): stack failed");
		return FALSE;
	}

	if((playerStoring >= MAX_PLAYERS) || (enemyPlayer >= MAX_PLAYERS))
	{
		debug(LOG_ERROR, "scrLearnPlayerBaseLoc: player index too high.");
		return FALSE;
	}

	if((playerStoring < 0) || (enemyPlayer < 0))
	{
		debug(LOG_ERROR, "scrLearnPlayerBaseLoc: player index too low.");
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth<<TILE_SHIFT) ||
		 (y < 0) || (y >= (SDWORD)mapHeight<<TILE_SHIFT))
	{
		debug(LOG_ERROR, "scrLearnPlayerBaseLoc: coords off map");
		return FALSE;
	}

	baseLocation[playerStoring][enemyPlayer][0] = x;
	baseLocation[playerStoring][enemyPlayer][1] = y;

	printf_console("Learned player base.");

	if (!stackPushResult(VAL_BOOL, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

//Saves enemy base x and y for a certain player
BOOL scrRecallPlayerBaseLoc(void)
{
	SDWORD				playerStoring,enemyPlayer, *x, *y;

	if (!stackPopParams(4, VAL_INT, &playerStoring, VAL_INT, &enemyPlayer,
						VAL_REF|VAL_INT, &x, VAL_REF|VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrRecallPlayerBaseLoc(): stack failed");
		return FALSE;
	}

	if((playerStoring >= MAX_PLAYERS) || (enemyPlayer >= MAX_PLAYERS))
	{
		debug(LOG_ERROR, "scrRecallPlayerBaseLoc: player index too high.");
		return FALSE;
	}

	if((playerStoring < 0) || (enemyPlayer < 0))
	{
		debug(LOG_ERROR, "scrRecallPlayerBaseLoc: player index too low.");
		return FALSE;
	}

	if(!CanRememberPlayerBaseLoc(playerStoring, enemyPlayer))		//return FALSE if this one not set yet
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}

		return TRUE;
	}

	*x = baseLocation[playerStoring][enemyPlayer][0];
	*y = baseLocation[playerStoring][enemyPlayer][1];

	if (!stackPushResult(VAL_BOOL, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

/* Checks if player base loc is stored */
BOOL scrCanRememberPlayerBaseLoc(void)
{
	SDWORD				playerStoring,enemyPlayer;

	if (!stackPopParams(2, VAL_INT, &playerStoring, VAL_INT, &enemyPlayer))
	{
		debug(LOG_ERROR, "scrCanRememberPlayerBaseLoc(): stack failed");
		return FALSE;
	}

	if((playerStoring >= MAX_PLAYERS) || (enemyPlayer >= MAX_PLAYERS))
	{
		debug(LOG_ERROR,"scrCanRememberPlayerBaseLoc: player index too high.");
		return FALSE;
	}

	if((playerStoring < 0) || (enemyPlayer < 0))
	{
		debug(LOG_ERROR,"scrCanRememberPlayerBaseLoc: player index too low.");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, CanRememberPlayerBaseLoc(playerStoring, enemyPlayer)))
	{
		return FALSE;
	}

	return TRUE;
}

/* Stores the place where we were attacked at */
BOOL scrLearnBaseDefendLoc(void)
{
	SDWORD				playerStoring,enemyPlayer, x, y;

	if (!stackPopParams(4, VAL_INT, &playerStoring, VAL_INT, &enemyPlayer,
						VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrLearnBaseDefendLoc(): stack failed");
		return FALSE;
	}

	if((playerStoring >= MAX_PLAYERS) || (enemyPlayer >= MAX_PLAYERS))
	{
		debug(LOG_ERROR,"scrLearnBaseDefendLoc: player index too high.");
		return FALSE;
	}

	if((playerStoring < 0) || (enemyPlayer < 0))
	{
		debug(LOG_ERROR,"scrLearnBaseDefendLoc: player index too low.");
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth<<TILE_SHIFT) ||
		 (y < 0) || (y >= (SDWORD)mapHeight<<TILE_SHIFT))
	{
		debug(LOG_ERROR,"scrLearnBaseDefendLoc: coords off map");
		return FALSE;
	}

	StoreBaseDefendLoc(x, y, playerStoring);

	if (!stackPushResult(VAL_BOOL, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

/* Stores the place where we were attacked at */
BOOL scrLearnOilDefendLoc(void)
{
	SDWORD				playerStoring,enemyPlayer, x, y;

	if (!stackPopParams(4, VAL_INT, &playerStoring, VAL_INT, &enemyPlayer,
						VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrLearnOilDefendLoc(): stack failed");
		return FALSE;
	}

	if((playerStoring >= MAX_PLAYERS) || (enemyPlayer >= MAX_PLAYERS))
	{
		debug(LOG_ERROR,"scrLearnOilDefendLoc: player index too high.");
		return FALSE;
	}

	if((playerStoring < 0) || (enemyPlayer < 0))
	{
		debug(LOG_ERROR,"scrLearnOilDefendLoc: player index too low.");
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth<<TILE_SHIFT) ||
		 (y < 0) || (y >= (SDWORD)mapHeight<<TILE_SHIFT))
	{
		debug(LOG_ERROR,"scrLearnOilDefendLoc: coords off map");
		return FALSE;
	}

	StoreOilDefendLoc(x, y, playerStoring);

	if (!stackPushResult(VAL_BOOL, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

/* Returns -1 if this location is not stored yet, otherwise returns index */
BOOL scrGetBaseDefendLocIndex(void)
{
	SDWORD				playerStoring, x, y;

	if (!stackPopParams(3, VAL_INT, &playerStoring, VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrGetBaseDefendLocIndex(): stack failed");
		return FALSE;
	}

	if(playerStoring >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrGetBaseDefendLocIndex: player index too high.");
		return FALSE;
	}

	if(playerStoring < 0)
	{
		debug(LOG_ERROR, "scrGetBaseDefendLocIndex: player index too low.");
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth<<TILE_SHIFT) ||
		 (y < 0) || (y >= (SDWORD)mapHeight<<TILE_SHIFT))
	{
		debug(LOG_ERROR, "scrGetBaseDefendLocIndex: coords off map");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, GetBaseDefendLocIndex(x,y,playerStoring)))
	{
		return FALSE;
	}

	return TRUE;
}

/* Returns -1 if this location is not stored yet, otherwise returns index */
BOOL scrGetOilDefendLocIndex(void)
{
	SDWORD				playerStoring, x, y;

	if (!stackPopParams(3, VAL_INT, &playerStoring, VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrGetOilDefendLocIndex(): stack failed");
		return FALSE;
	}

	if(playerStoring >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "scrGetOilDefendLocIndex: player index too high.");

		return FALSE;
	}

	if(playerStoring < 0)
	{
		debug(LOG_ERROR, "scrGetOilDefendLocIndex: player index too low.");
		return FALSE;
	}

	if ( (x < 0) || (x >= (SDWORD)mapWidth<<TILE_SHIFT) ||
		 (y < 0) || (y >= (SDWORD)mapHeight<<TILE_SHIFT))
	{
		debug(LOG_ERROR, "scrGetOilDefendLocIndex: coords off map");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, GetOilDefendLocIndex(x,y,playerStoring)))
	{
		return FALSE;
	}

	return TRUE;
}

/* Returns number of available locations */
BOOL scrGetBaseDefendLocCount(void)
{
	if (!stackPushResult(VAL_INT, MAX_BASE_DEFEND_LOCATIONS))
	{
		debug(LOG_ERROR, "scrGetBaseDefendLocCount: push failed");
		return FALSE;
	}

	return TRUE;
}

/* Returns number of available locations*/
BOOL scrGetOilDefendLocCount(void)
{
	if (!stackPushResult(VAL_INT, MAX_OIL_DEFEND_LOCATIONS))
	{
		debug(LOG_ERROR, "scrGetOilDefendLocCount: push failed");
		return FALSE;
	}

	return TRUE;
}

/* Returns a locations and its priority */
BOOL scrRecallBaseDefendLoc(void)
{
	SDWORD				player, *x, *y, *prior,index;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &index,
						VAL_REF|VAL_INT, &x, VAL_REF|VAL_INT, &y, VAL_REF|VAL_INT, &prior))
	{
		debug(LOG_ERROR, "scrRecallBaseDefendLoc(): stack failed");
		return FALSE;
	}

	if(player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrRecallBaseDefendLoc: player index too high.");
		return FALSE;
	}

	if(index < 0 || index >= MAX_BASE_DEFEND_LOCATIONS)
	{
		debug(LOG_ERROR,"scrRecallBaseDefendLoc: wrong index.");
		return FALSE;
	}

	if(player < 0)
	{
		debug(LOG_ERROR,"scrRecallBaseDefendLoc: player index too low.");
		return FALSE;
	}

	//check if can recall at this location
	if(!CanRememberPlayerBaseDefenseLoc(player, index))
	{
		if (!stackPushResult(VAL_INT, FALSE))
		{
			return FALSE;
		}

		return TRUE;
	}

	*x = baseDefendLocation[player][index][0];
	*y = baseDefendLocation[player][index][1];

	*prior = baseDefendLocPrior[player][index];

	if (!stackPushResult(VAL_INT, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

/* Returns number of available locations */
BOOL scrRecallOilDefendLoc(void)
{
	SDWORD				player, *x, *y, *prior,index;

	if (!stackPopParams(5, VAL_INT, &player, VAL_INT, &index,
						VAL_REF|VAL_INT, &x, VAL_REF|VAL_INT, &y, VAL_REF|VAL_INT, &prior))
	{
		debug(LOG_ERROR, "scrRecallOilDefendLoc(): stack failed");
		return FALSE;
	}

	if(player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrRecallOilDefendLoc: player index too high.");
		return FALSE;
	}

	if(index < 0 || index >= MAX_OIL_DEFEND_LOCATIONS)
	{
		debug(LOG_ERROR,"scrRecallOilDefendLoc: wrong index: %d.", index);
		return FALSE;
	}

	if(player < 0)
	{
		debug(LOG_ERROR,"scrRecallOilDefendLoc: player index too low.");
		return FALSE;
	}

	//check if can recall at this location
	if(!CanRememberPlayerOilDefenseLoc(player, index))
	{
		if (!stackPushResult(VAL_INT, FALSE))
		{
			return FALSE;
		}

		return TRUE;
	}

	*x = oilDefendLocation[player][index][0];
	*y = oilDefendLocation[player][index][1];

	*prior = oilDefendLocPrior[player][index];

	if (!stackPushResult(VAL_INT, TRUE))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrSavePlayerAIExperience(void)
{
	SDWORD				player;
	BOOL				bNotify;

	if (!stackPopParams(2, VAL_INT, &player, VAL_BOOL, &bNotify))
	{
		debug(LOG_ERROR, "scrSavePlayerAIExperience(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, SavePlayerAIExperience(player, bNotify)))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrLoadPlayerAIExperience(void)
{
	SDWORD				player;
	BOOL				bNotify;

	if (!stackPopParams(2, VAL_INT, &player, VAL_BOOL, &bNotify))
	{
		debug(LOG_ERROR, "scrLoadPlayerAIExperience(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, LoadPlayerAIExperience(player, bNotify)))
	{
		return FALSE;
	}

	return TRUE;
}


/* Add a beacon (blip) */
BOOL addHelpBlip(SDWORD locX, SDWORD locY, SDWORD forPlayer, SDWORD sender, char * textMsg)
{
	UDWORD			height;
	MESSAGE			*psMessage;
	VIEWDATA		*pTempData;

	//debug(LOG_WZ, "addHelpBlip: forPlayer=%d, sender=%d", forPlayer,sender);

	if (forPlayer >= MAX_PLAYERS)
	{
		debug(LOG_ERROR, "addHelpBlip: player number is too high");
		return FALSE;
	}

	//add the beacon for the sender so he can see where he put it
	//but only if he's not already adding this one for himself
	//if(forPlayer != sender)
	//	addHelpBlip(locX, locY, sender, sender, textMsg);

	//find the message if was already added previously
	psMessage = findHelpMsg(forPlayer, sender);
	if (psMessage)
	{
		//remove it
		//debug(LOG_WZ, "addHelpBlip: removing previous message from sender=%d",sender);
		removeMessage(psMessage, forPlayer);
	}

	//create new message
	psMessage = addMessage(MSG_PROXIMITY, FALSE, forPlayer);
	if (psMessage)
	{
		//debug(LOG_WZ, "created new blip for player %d from %d", forPlayer, sender);

		//set the data
		pTempData = HelpViewData(sender, textMsg, locX, locY);

		psMessage->pViewData = (MSG_VIEWDATA *)pTempData;

		//check the z value is at least the height of the terrain
		height = map_Height(((VIEW_PROXIMITY *)pTempData->pData)->x,
			((VIEW_PROXIMITY *)pTempData->pData)->y);

		//if (((VIEW_PROXIMITY *)pTempData->pData)->z < height)
		//{
			((VIEW_PROXIMITY *)pTempData->pData)->z = height;
		//}

	}
	else
	{
		debug(LOG_WZ, "addHelpBlip: addMessage() failed");
	}

	//Received a blip message from a player callback
	//store and call later
	//-------------------------------------------------
	//call beacon callback only if not adding for ourselves
	if(forPlayer != sender)
	{
		if(!msgStackPush(CALL_BEACON,sender,forPlayer,textMsg,locX,locY,NULL))
		{
			debug(LOG_ERROR, "addHelpBlip() - msgStackPush - stack failed");
			return FALSE;
		}
	}

	return TRUE;
}

BOOL sendBeaconToPlayer(SDWORD locX, SDWORD locY, SDWORD forPlayer, SDWORD sender, char * beaconMsg)
{
	if(sender == forPlayer || myResponsibility(forPlayer))	//if destination player is on this machine
	{
		debug(LOG_WZ,"sending beacon to player %d (local player) from %d", forPlayer, sender);
		return addHelpBlip(locX, locY, forPlayer, sender, beaconMsg);
	}
	else
	{
		debug(LOG_WZ,"sending beacon to player %d (remote player) from %d", forPlayer, sender);
		return sendBeaconToPlayerNet(locX, locY, forPlayer, sender, beaconMsg);
	}
}

//prepare viewdata for help blip
VIEWDATA *HelpViewData(SDWORD sender, char *textMsg, UDWORD LocX, UDWORD LocY)
{
	VIEWDATA			*psViewData;
	char				name[MAX_STR_LENGTH];
	SDWORD				audioID;
	UDWORD				numText;


	//allocate message space
	psViewData = (VIEWDATA *)MALLOC(sizeof(VIEWDATA));
	if (psViewData == NULL)
	{
		debug(LOG_ERROR,"prepairHelpViewData() - Unable to allocate memory for viewdata");
		return NULL;
	}

	memset(psViewData, 0, sizeof(VIEWDATA));

	numText = 1;

	psViewData->numText=(UBYTE)numText;

	//allocate storage for the name
	name[0] = 'h';
	name[1] = 'e';
	name[2] = 'l';
	name[3] = 'p';
	name[4] = '\0';
 	psViewData->pName = (char *)MALLOC((strlen(name))+1);
	if (psViewData->pName == NULL)
	{
		debug(LOG_ERROR,"prepairHelpViewData() - ViewData Name - Out of memory");
		return NULL;
	}

	strcpy(psViewData->pName,name);

	//allocate space for text strings
	psViewData->ppTextMsg = (char **) MALLOC(sizeof(char *));

	//store text message pointer
	psViewData->ppTextMsg[0] = textMsg;

	//store message type
	psViewData->type = VIEW_HELP;	//was VIEW_PROX

	//allocate memory for blip location etc
	psViewData->pData = (VIEW_PROXIMITY *) MALLOC(sizeof(VIEW_PROXIMITY));

	if (psViewData->pData == NULL)
	{
		debug(LOG_ERROR,"prepairHelpViewData() - Unable to allocate memory");
		return NULL;
	}


	//store audio
	audioID = NO_SOUND;
	((VIEW_PROXIMITY *)psViewData->pData)->audioID = audioID;

	//store blip location
	if (LocX < 0)
	{
		debug(LOG_ERROR,"prepairHelpViewData() - Negative X coord for prox message");
		return NULL;
	}

	if (LocY < 0)
	{
		debug(LOG_ERROR,"prepairHelpViewData() - Negative X coord for prox message");
		return NULL;
	}

	((VIEW_PROXIMITY *)psViewData->pData)->x = (UDWORD)LocX;
	((VIEW_PROXIMITY *)psViewData->pData)->y = (UDWORD)LocY;

	//store prox message type
	((VIEW_PROXIMITY *)psViewData->pData)->proxType = PROX_ENEMY; //PROX_ENEMY for now

	//remember who sent this msg, so we could remove this one, when same player sends a new help-blip msg
	((VIEW_PROXIMITY *)psViewData->pData)->sender = sender;

	//remember when the message was created so can remove it after some time
	((VIEW_PROXIMITY *)psViewData->pData)->timeAdded = gameTime;

	return psViewData;
}

/* Looks through the players list of messages to find VIEW_HELP (one per player!) pointer */
MESSAGE * findHelpMsg(UDWORD player, SDWORD sender)
{
	MESSAGE					*psCurr;

	for (psCurr = apsMessages[player]; psCurr != NULL; psCurr = psCurr->psNext)
	{
		//look for VIEW_HELP, should only be 1 per player
		if (psCurr->type == MSG_PROXIMITY)
		{
			//((VIEW_PROXIMITY *)((VIEWDATA *)psCurr->pViewData)->pData)->proxType
			if(((VIEWDATA *)psCurr->pViewData)->type == VIEW_HELP)
			{
				debug(LOG_WZ, "findHelpMsg: %d ALREADY HAS A MESSAGE STORED", player);
				//debug(LOG_ERROR,"stored sender = %d, looking for %d", ((VIEW_PROXIMITY *)((VIEWDATA *)psCurr->pViewData)->pData)->sender, sender);
				//if((VIEW_PROXIMITY *)psCurr->pViewData)
				if(((VIEW_PROXIMITY *)((VIEWDATA *)psCurr->pViewData)->pData)->sender == sender)
				{
					debug(LOG_WZ, "findHelpMsg: %d ALREADY HAS A MESSAGE STORED from %d", player, sender);
					return psCurr;
				}
			}
		}
	}

	//not found the message so return NULL
	return NULL;
}

/* Add beacon (radar blip) */
BOOL scrDropBeacon(void)
{
	SDWORD			forPlayer,sender;
	char			*ssval=NULL,ssval2[255];
	UDWORD			locX,locY,locZ;

	if (!stackPopParams(6, VAL_STRING, &ssval , VAL_INT, &forPlayer,
				VAL_INT, &sender, VAL_INT, &locX, VAL_INT, &locY, VAL_INT, &locZ))
	{
		debug(LOG_ERROR,"scrDropBeacon failed to pop parameters");
		return FALSE;
	}

	if(!addHelpBlip(locX, locY, sender, sender, ssval))
		debug(LOG_ERROR,"scrDropBeacon: addHelpBlip failed");

	sprintf(ssval2, "%s : %s", getPlayerName(sender), ssval);	//temporary solution

	return sendBeaconToPlayer(locX, locY, forPlayer, sender, ssval2);
}

/* Remove help message from the map */
BOOL scrRemoveHelpMessage(void)
{
	MESSAGE			*psMessage;
	SDWORD			player, sender;

	if (!stackPopParams(2, VAL_INT, &player, VAL_INT, &sender))
	{
		debug(LOG_ERROR,"scrRemoveMessage: failed to pop parameters");
		return FALSE;
	}

	if (player >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrRemoveMessage:player number is too high");
		return FALSE;
	}

	if (sender >= MAX_PLAYERS)
	{
		debug(LOG_ERROR,"scrRemoveMessage:sender number is too high");
		return FALSE;
	}

	//find the message
	psMessage = findHelpMsg(player, sender);
	if (psMessage)
	{
		//delete it
		removeMessage(psMessage, player);
	}
	//else
	//{
	//	ASSERT((FALSE, "scrRemoveMessage:cannot find message - %s",
	//		psViewData->pName));
	//	return FALSE;
	//}

	return TRUE;
}

BOOL scrClosestDamagedGroupDroid(void)
{
	DROID_GROUP	*psGroup;
	DROID		*psDroid,*psClosestDroid;
	SDWORD		x,y,healthLeft,wBestDist,wDist,maxRepairedBy,player;

	if (!stackPopParams(6, VAL_INT, &player, ST_GROUP, &psGroup, VAL_INT, &healthLeft,
		VAL_INT, &x, VAL_INT, &y, VAL_INT, &maxRepairedBy))
	{
		debug(LOG_ERROR, "scrClosestDamagedGroupDroid: failed to pop");
		return FALSE;
	}

	wBestDist = 999999;
	psClosestDroid = NULL;
	for(psDroid = psGroup->psList;psDroid; psDroid = psDroid->psGrpNext)
	{
		if((psDroid->body * 100 / psDroid->originalBody) <= healthLeft)	//in%
		{
			wDist = (dirtySqrt(psDroid->x, psDroid->y, x, y) >> TILE_SHIFT);	//in tiles
			if(wDist < wBestDist)
			{
				if((maxRepairedBy < 0) || (getNumRepairedBy(psDroid, player) <= maxRepairedBy))
				{
					psClosestDroid = psDroid;
					wBestDist = wDist;
				}
			}
		}
	}

	if (!stackPushResult((INTERP_TYPE)ST_DROID, (SDWORD)psClosestDroid))
	{
		return FALSE;
	}

	return TRUE;
}

SDWORD getNumRepairedBy(DROID *psDroidToCheck, SDWORD player)
{
	DROID		*psDroid;
	SDWORD		numRepaired = 0;

	for(psDroid = apsDroidLists[player]; psDroid; psDroid = psDroid->psNext)
	{
		if((psDroid->droidType != DROID_REPAIR) && (psDroid->droidType != DROID_CYBORG_REPAIR))
		{
			continue;
		}

		if((psDroid->psTarget != NULL) && (psDroid->psTarget->type == OBJ_DROID))
		{
			if(((DROID *)psDroid->psTarget) == psDroidToCheck)
				numRepaired++;
		}
	}

	return numRepaired;
}

/* Uses printf_console() for console debug output right now */
BOOL scrMsgBox(void)
{
	char	*ssval=NULL;

	if (!stackPopParams(1, VAL_STRING, &ssval))
	{
		debug(LOG_ERROR, "scrMsgBox(): stack failed");
		return FALSE;
	}

	//dbg_console("DEBUG: %s", ssval);

	printf_console("DEBUG: %s",ssval);

	return TRUE;
}


// Check for a struct being within a certain range of a position (must be visible)
BOOL scrStructInRangeVis(void)
{
	SDWORD		range, player,lookingPlayer, x,y;
	BOOL		found;

	if (!stackPopParams(5, VAL_INT, &lookingPlayer, VAL_INT, &player , VAL_INT, &x, VAL_INT, &y, VAL_INT, &range))
	{
		debug(LOG_ERROR, "scrStructInRangeVis: failed to pop");
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT(FALSE, "scrStructInRange: invalid player number");
		return FALSE;
	}

	found = objectInRangeVis((BASE_OBJECT *)apsStructLists[player], x,y, range, lookingPlayer);

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

// check for a base object being in range of a point
BOOL objectInRangeVis(BASE_OBJECT *psList, SDWORD x, SDWORD y, SDWORD range, SDWORD lookingPlayer)
{
	BASE_OBJECT		*psCurr;
	SDWORD			xdiff, ydiff, rangeSq;

	// See if there is a droid in range
	rangeSq = range * range;
	for(psCurr = psList; psCurr; psCurr = psCurr->psNext)
	{
		if(psCurr->type == OBJ_STRUCTURE)
		{
			if(!((STRUCTURE *)psCurr)->visible[lookingPlayer])
				continue;
		}

		if(psCurr->type == OBJ_DROID)
		{
			if(!((DROID *)psCurr)->visible[lookingPlayer])
				continue;
		}

		// skip partially build structures
		//if ( (psCurr->type == OBJ_STRUCTURE) &&
		//	 (((STRUCTURE *)psCurr)->status != SS_BUILT) )
		//{
		//	continue;
		//}

		// skip flying vtols
		if ( (psCurr->type == OBJ_DROID) &&
			vtolDroid((DROID *)psCurr) &&
			((DROID *)psCurr)->sMove.Status != MOVEINACTIVE )
		{
			continue;
		}

		xdiff = (SDWORD)psCurr->x - x;
		ydiff = (SDWORD)psCurr->y - y;
		if (xdiff*xdiff + ydiff*ydiff < rangeSq)
		{
			return TRUE;
		}
	}

	return FALSE;
}

/* Go after a certain research */
BOOL scrPursueResearch(void)
{
	RESEARCH			*psResearch;
	SDWORD				structure, player;
	UWORD				cur,index,tempIndex,foundIndex;
	SWORD				top;

	UWORD				Stack[400];

	BOOL				found;
	PLAYER_RESEARCH		*pPlayerRes;

	char				sTemp[128];
	STRUCTURE			*psBuilding;
	RESEARCH_FACILITY	*psResFacilty;

	RESEARCH			*pResearch;

	if (!stackPopParams(3,ST_STRUCTURE, &structure, VAL_INT, &player, ST_RESEARCH, &psResearch ))
	{
		debug(LOG_ERROR, "scrPursueResearch(): stack failed");
		return FALSE;
	}

	if(psResearch == NULL)
	{
		ASSERT(FALSE, ": no such research topic");
		return FALSE;
	}


	psBuilding	=	(STRUCTURE *) structure;
	psResFacilty =	(RESEARCH_FACILITY*)psBuilding->pFunctionality;

	if(psResFacilty->psSubject != NULL)		// not finshed yet
	{
		if (!stackPushResult(VAL_BOOL, FALSE))
		{
			return FALSE;
		}
		return TRUE;
	}

	pPlayerRes = asPlayerResList[player];
	index = psResearch - asResearch;

	if (index >= numResearch)
	{
		ASSERT(FALSE, "scrPursueResearch: invalid research index");
		return FALSE;
	}

	found = FALSE;

	if(beingResearchedByAlly(index, player))		//an ally is already researching it
	{
		found = FALSE;
	}
	else if(IsResearchCompleted(&pPlayerRes[index]))
	{
		found = FALSE;
		//DbgMsg("Research already completed: %d", index);
	}
	else if(IsResearchStarted(&pPlayerRes[index]))
	{
		found = FALSE;
		//DbgMsg("Research already in progress, %d", index);
	}
	else if(IsResearchPossible(&pPlayerRes[index]) || IsResearchCancelled(&pPlayerRes[index]))
	{
		foundIndex = index;
		found = TRUE;
		//DbgMsg("Research possible or cancelled: %d", index);
	}
	else if(skTopicAvail(index,player))
	{
		foundIndex = index;
		found = TRUE;
		//DbgMsg("Research available: %d",index);
	}
	else
	{
		//DbgMsg("starting to search for: %d, %s", index, asResearch[index].pName);
		top = -1;

		cur = 0;				//start with first index's PR
		tempIndex = -1;
		while(TRUE)	//do
		{
			//DbgMsg("Going on with %d, numPR: %d, %s", index, asResearch[index].numPRRequired, asResearch[index].pName);

			if(cur >= asResearch[index].numPRRequired)		//node has nodes?
			{
				//DbgMsg("cur >= numPRRequired : %d (%d >= %d)", index, cur, asResearch[index].numPRRequired);

				top = top - 2;
				if(top < (-1))
				{
					//DbgMsg("Nothing on stack");
					break;		//end of stack
				}
				index = Stack[top + 2];	//if index = -1, then exit
				cur = Stack[top + 1];		//go to next PR of the last node, since this one didn't work

			}
			else		//end of nodes not reached
			{
				tempIndex = asResearch[index].pPRList[cur];		//get cur node's index
				//DbgMsg("evaluating node: %d, (cur = %d), %s", tempIndex, cur, asResearch[tempIndex].pName);

				if(skTopicAvail(tempIndex,player) && (!beingResearchedByAlly(tempIndex, player)))	//<NEW> - ally check added
				{
					//DbgMsg("avail: %d (cur=%d), %s", tempIndex, cur, asResearch[tempIndex].pName);
					found = TRUE;
					foundIndex = tempIndex;		//done
					break;
				}
				else if((IsResearchCompleted(&pPlayerRes[tempIndex])==FALSE) && (IsResearchStarted(&pPlayerRes[tempIndex])==FALSE))		//not avail and not busy with it, can check this PR's PR
				{
					//DbgMsg("node not complete, not started: %d, (cur=%d), %s", tempIndex,cur, asResearch[tempIndex].pName);
					if(asResearch[tempIndex].numPRRequired > 0)	//node has any nodes itself
					{
						//DbgMsg("node has nodes, so selecting as main node: %d, %s", tempIndex, asResearch[tempIndex].pName);

						Stack[top+1] = cur;								//so can go back to it further
						Stack[top+2] = index;
						top = top + 2;

						index = tempIndex;		//go 1 level further
						cur = -1;									//start with first PR of this PR next time
					}
					else		//has no PRs, choose it (?)
					{
						if(!beingResearchedByAlly(tempIndex, player))	//<NEW> ally check added
						{
							//DbgMsg("PR has no PRs, choosing it: %d (cur=%d), %s", tempIndex, cur, asResearch[tempIndex].pName);
							found = TRUE;
							foundIndex = tempIndex;	//done
							break;
						}
					}
				}
			}

			cur++;				//try next node of the main node
			if((cur >= asResearch[index].numPRRequired) && (top <= (-1)))	//nothing left
			{
				//DbgMsg("END");
				break;
			}

		}//while((cur < asResearch[index].numPRRequired) && (top >= (-1)));
	}

	if(found)
	{
		if(foundIndex < numResearch)
		{
			pResearch = (asResearch + foundIndex);
			pPlayerRes				= asPlayerResList[player]+ foundIndex;
			psResFacilty->psSubject = (BASE_STATS*)pResearch;		  //set the subject up

			if (IsResearchCancelled(pPlayerRes))
			{
				psResFacilty->powerAccrued = pResearch->researchPower;//set up as if all power available for cancelled topics
			}
			else
			{
				psResFacilty->powerAccrued = 0;
			}

			MakeResearchStarted(pPlayerRes);
			psResFacilty->timeStarted = ACTION_START_TIME;
			psResFacilty->timeStartHold = 0;
			psResFacilty->timeToResearch = pResearch->researchPoints / 	psResFacilty->researchPoints;
			if (psResFacilty->timeToResearch == 0)
			{
				psResFacilty->timeToResearch = 1;
			}

			sprintf(sTemp,"player:%d starts topic: %s",player, asResearch[foundIndex].pName );
			NETlogEntry(sTemp,0,0);
		}
	}

	if (!stackPushResult(VAL_BOOL, found))
	{
		return FALSE;
	}

	return TRUE;
}

BOOL scrGetStructureType(void)
{
	STRUCTURE			*psStruct;

	if (!stackPopParams(1, ST_STRUCTURE, &psStruct))
	{
		debug(LOG_ERROR, "scrGetStructureType(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, psStruct->pStructureType->type))
	{
		debug(LOG_ERROR, "scrGetStructureType(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

/* Get player name from index */
BOOL scrGetPlayerName(void)
{
	SDWORD	player;

	if (!stackPopParams(1, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrGetPlayerName(): stack failed");
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrGetPlayerName: invalid player number" );
		return FALSE;
	}

	if (!stackPushResult(VAL_STRING, (SDWORD)getPlayerName((UDWORD)player)))
	{
		debug(LOG_ERROR, "scrGetPlayerName(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

/* Set player name */
BOOL scrSetPlayerName(void)
{
	SDWORD	player;
	char	*sName = NULL;

	if (!stackPopParams(2, VAL_INT, &player, VAL_STRING, &sName))
	{
		debug(LOG_ERROR, "scrSetPlayerName(): stack failed");
		return FALSE;
	}

	if (player < 0 || player >= MAX_PLAYERS)
	{
		ASSERT( FALSE, "scrSetPlayerName: invalid player number" );
		return FALSE;
	}

	if (!stackPushResult(VAL_BOOL, (SDWORD)setPlayerName(player,sName)))
	{
		debug(LOG_ERROR, "scrSetPlayerName(): failed to push result");
		return FALSE;
	}

	return TRUE;
}



SDWORD guessPlayerFromMessage(char **str)
{
	SDWORD	player,count=0;
	char	*endOfPlayerList;
	char	playerName[255];
	SDWORD	storage=0;
	BOOL	bOK=FALSE;

	ASSERT(MAX_PLAYERS <= 8, "guessPlayerFromMessage: MAX_PLAYERS too high");


	endOfPlayerList = *str;
	player = 0;

	debug(LOG_SCRIPT, "now checking string='%s'",*str);

	while( sscanf(*str, "%[^',:;?! ]%*[,';?! ]%n", playerName, &count)	//didn't reach the end	//first field: what to stop on; second field: what to read (skip) afterwards
		&& player >= 0										//last string was a player name
		//&& *str < copyStr + strlen(copyStr)
		)
	{
		if(count==0)	/* nothing read ? */
			break;

		debug(LOG_SCRIPT, "playerName='%s' count=%d",playerName, count);

		*str += count;
		player = getPlayerFromString(playerName);

		debug(LOG_SCRIPT, "player=%d",player);

		if(player >= 0 && player < MAX_PLAYERS)
		{
			bOK = TRUE;
			endOfPlayerList = *str;		//remember where target player list ends
			storage = storage | playerFlag[player];		//set player flag
			debug(LOG_SCRIPT, "storage=%d",storage);
		}

		debug(LOG_SCRIPT, "             ");
		debug(LOG_SCRIPT, "now checking string = '%s'",*str);
	}

	*str = endOfPlayerList;	//skip player list

	return storage;		//Must be 0 if no players
}

SDWORD getPlayerFromString(char *playerName)
{
	UDWORD	playerIndex;
	char	sPlayerNumber[255];

	for( playerIndex=0; playerIndex<MAX_PLAYERS; playerIndex++ )
	{
		/* check name */
		debug(LOG_SCRIPT, "checking  (%s,%s)",getPlayerName(playerIndex), playerName);
		if (strncasecmp(getPlayerName(playerIndex),playerName, 255) == 0)
		{
			debug(LOG_SCRIPT, "matched, returning %d", playerIndex);
			return playerIndex;
		}

		/* check color */
		debug(LOG_SCRIPT, "checking (%s,%s)",getPlayerColourName(playerIndex), playerName);
		if (strncasecmp(getPlayerColourName(playerIndex),playerName, 255) == 0)
		{
			debug(LOG_SCRIPT, "matched, returning %d", playerIndex);
			return playerIndex;
		}

		/* check player number */
		sprintf(sPlayerNumber,"%d",playerIndex);
		debug(LOG_SCRIPT, "checking (%s,%s)",sPlayerNumber, playerName);
		if (strncasecmp(sPlayerNumber,playerName, 255) == 0)
		{
			debug(LOG_SCRIPT, "matched, returning %d", playerIndex);
			return playerIndex;
		}

	}

	return -1;
}

BOOL scrGetTargetPlayers(void)
{
	char	*myMsg = NULL,*freeMsg = NULL;
	char	**ssval = NULL;
	SDWORD	players=0;

	if (!stackPopParams(1,  VAL_REF | VAL_STRING, &ssval))
	{
		debug(LOG_ERROR, "scrGetTargetPlayers(): stack failed");
		return FALSE;
	}

	if(*ssval == NULL)
	{
		debug(LOG_ERROR, "scrGetTargetPlayers(): passed string was not initialized");
		return FALSE;
	}

	debug(LOG_SCRIPT, "scrGetTargetPlayers: ssval='%s'", *ssval);

	myMsg = (char*)MALLOC(255);
	freeMsg = myMsg;

	strcpy(myMsg,*ssval);

	debug(LOG_SCRIPT, "scrGetTargetPlayers: myMsg='%s'", myMsg);

	players = guessPlayerFromMessage(&myMsg);

	debug(LOG_SCRIPT, "scrGetTargetPlayers: myMsg new='%s'", myMsg);

	strcpy(*ssval, myMsg);

	debug(LOG_SCRIPT, "scrGetTargetPlayers: ssval='%s'", *ssval);

	if (!stackPushResult(VAL_INT, players))
	{
		debug(LOG_ERROR, "scrGetTargetPlayers(): failed to push result");
		return FALSE;
	}

	FREE(freeMsg);

	return TRUE;

}

BOOL scrMatch(void)
{
	char	*sToParse = NULL, *sToMatch = NULL;
	char	*sOrigToParse = NULL, *sOrigToMatch = NULL;
	char	*wordNeed = NULL, *wordFound = NULL;
	SDWORD	players=0,readCountParse=0,readCountMatch=0;
	SDWORD	fieldAssignedParse=0,fieldAssignedMatch=0;
	BOOL	ok = TRUE,bEndParse=FALSE,bEndMatch=FALSE;
	SDWORD	*nResult;

	if (!stackPopParams(3, VAL_STRING, &sOrigToParse, VAL_STRING, &sOrigToMatch, VAL_REF|VAL_INT, &nResult))
	{
		debug(LOG_ERROR, "scrMatch(): stack failed");
		return FALSE;
	}

	if(sOrigToParse == NULL)
	{
		debug(LOG_ERROR, "scrMatch(): message to parse is null");
		return FALSE;
	}

	if(sOrigToMatch == NULL)
	{
		debug(LOG_ERROR, "scrMatch(): string to match is null");
		return FALSE;
	}

	sToParse = sOrigToParse;
	sToMatch = sOrigToMatch;

	debug(LOG_SCRIPT, " ");
	debug(LOG_SCRIPT, "sOrigToParse='%s'", sOrigToParse);
	debug(LOG_SCRIPT, "sOrigToMatch='%s'", sOrigToMatch);

	wordFound = (char*)MALLOC(255);
	wordNeed = (char*)MALLOC(255);

	*nResult = -1;

	while(ok)
	{
		/* get next word */
		fieldAssignedParse = getFirstWord(sToParse,&wordFound,&readCountParse);
		sToParse = sToParse + readCountParse;			/* next time start with next word */
		if(readCountParse == 0)										/* sscanf returns 0 when last word is read */
			sToParse = sToParse + strlen(wordFound);

		/* get next word */
		fieldAssignedMatch = getFirstWord(sToMatch,&wordNeed,&readCountMatch);
		sToMatch = sToMatch + readCountMatch;			/* next time start with next word */
		if(readCountMatch == 0)										/* sscanf returns 0 when last word is read */
			sToMatch = sToMatch + strlen(wordNeed);	

		debug(LOG_SCRIPT, "wordFound '%s'", wordFound);
		debug(LOG_SCRIPT, "wordNeed '%s'", wordNeed);

		/* failed if *one* of the strings ended */
		if((fieldAssignedParse > 0 && fieldAssignedMatch <= 0)
			|| (fieldAssignedMatch > 0 && fieldAssignedParse <= 0))
		{
			debug(LOG_SCRIPT, "exit condition FAILED");
			ok = FALSE;
			break;
		}
		else if(fieldAssignedParse <= 0 && fieldAssignedMatch <= 0)		/* no more words left in either of them */
		{
			debug(LOG_SCRIPT, "exit condition SUCCESS");
			ok = TRUE;
			break;
		}

		/*
		 *	now compare both words
		 */

		if (strncasecmp(wordNeed,"<player>", 255) == 0)		/* if we are looking for player */
		{
			debug(LOG_SCRIPT, "matching <player>");
			*nResult = getPlayerFromString(wordFound);

			if(*nResult == -1)	/* failed to match player, stop */
			{
				debug(LOG_SCRIPT, "failed to match <player>");
				ok = FALSE;
			}
			else
			{
				debug(LOG_SCRIPT, "matched <player>");
			}
		}
		else if (strncasecmp(wordNeed,wordFound,255) != 0)	/* just compare words to see if they match */
		{
			debug(LOG_SCRIPT, "words did not match");
			ok = FALSE;
		}

		debug(LOG_SCRIPT, " ");
	}

	debug(LOG_SCRIPT, "END");

	FREE(wordFound);
	FREE(wordNeed);

	if (!stackPushResult(VAL_BOOL, ok))
	{
		debug(LOG_ERROR, "scrGetTargetPlayers(): failed to push result");
		return FALSE;
	}

	return TRUE;
}

SDWORD getFirstWord(char *sText, char **sWord, SDWORD *readCount)
{
	SDWORD	count=0,fieldsAssigned=0;

	debug(LOG_SCRIPT, "--getWord: now checking string='%s'",sText);

	ASSERT(*sWord != NULL, "getFirstWord: sWord is NULL");

	strcpy(*sWord,"");		/* clear */

	fieldsAssigned = sscanf(sText, "%[^',:;?! ]%*[,';?! ]%n", *sWord, &count);

	debug(LOG_SCRIPT, "--getWord: matched='%s', count=%d, fieldsAssigned=%d",*sWord, count, fieldsAssigned);

	*readCount = count;

	return fieldsAssigned;
}


/* Checks if a particular bit is set in an integer */
BOOL scrBitSet(void)
{
	SDWORD				val1,val2;

	if (!stackPopParams(2, VAL_INT, &val1, VAL_INT, &val2))
	{
		debug(LOG_ERROR, "scrBitSet(): failed to pop");
		return FALSE;
	}

	ASSERT(val2 < MAX_PLAYERS && val2 >= 0, "scrBitSet(): wrong player index (%d)", val2);

	if (!stackPushResult(VAL_BOOL, val1 & playerFlag[val2] ))
	{
		return FALSE;
	}

	return TRUE;
}

/* Can we create and break alliaces? */
BOOL scrAlliancesLocked(void)
{
	BOOL		bResult = TRUE;

	if(bMultiPlayer && (game.alliance == ALLIANCES))
		bResult = FALSE;

	if (!stackPushResult(VAL_BOOL, bResult))
	{
		debug(LOG_ERROR, "scrAlliancesLocked(): failed to push result");
		return FALSE;
	}
	
	return TRUE;
}

BOOL scrASSERT(void)
{
	BOOL				bExpression;
	char			*sMsg = NULL;
	SDWORD			player;
	char			sTmp[255];

	if (!stackPopParams(3, VAL_BOOL, &bExpression, VAL_STRING, &sMsg, VAL_INT, &player))
	{
		debug(LOG_ERROR, "scrASSERT(): stack failed");
		return FALSE;
	}

#ifdef DEBUG
	/* Just pass the expression and message from script */
	sprintf(sTmp,"%d) %s",player,sMsg);
	ASSERT(bExpression, sTmp);
#else
	if(scrDebug[player])
	{
		if(!bExpression)
		{
			sprintf(sTmp,"%d) %s",player,sMsg);
			addConsoleMessage(sTmp,RIGHT_JUSTIFY);
		}
	}
#endif

	return TRUE;
}

/* Visualize radius at position */
BOOL scrShowRangeAtPos(void)
{
	SDWORD		x,y,radius;

	if (!stackPopParams(3, VAL_INT, &x, VAL_INT, &y, VAL_INT, &radius))
	{
		debug(LOG_ERROR, "scrShowRangeAtPos(): stack failed");
		return FALSE;
	}

	//Turn on/off drawing
	showRangeAtPos(x,y,radius);

	return TRUE;
}

BOOL scrToPow(void)
{
	SDWORD		x,y;

	if (!stackPopParams(2, VAL_INT, &x, VAL_INT, &y))
	{
		debug(LOG_ERROR, "scrToPow(): stack failed");
		return FALSE;
	}

	if (!stackPushResult(VAL_INT, (SDWORD)pow((double)x,(int)y) ))
	{
		debug(LOG_ERROR, "scrToPow(): failed to push result");
		return FALSE;
	}
	
	return TRUE;
}

/* Show/Hide multiplayer debug menu */
BOOL scrDebugMenu(void)
{
	SDWORD		menuUp;

	if (!stackPopParams(1, VAL_BOOL, &menuUp))
	{
		debug(LOG_ERROR, "scrDebugMenu(): stack failed");
		return FALSE;
	}

	(void)addDebugMenu(menuUp);
	
	return TRUE;
}

/* Set debug menu output string */
BOOL scrSetDebugMenuEntry(void)
{
	SDWORD		index;
	char	*sEntry = NULL;
	BOOL		bAddingNew = FALSE;

	if (!stackPopParams(2, VAL_STRING, &sEntry, VAL_INT, &index))
	{
		debug(LOG_ERROR, "scrSetDebugMenuEntry(): stack failed");
		return FALSE;
	}

	/* New one? */
	if(!strcmp(debugMenuEntry[index],""))
	{
		bAddingNew = TRUE;
	}

	/* Set */
	strcpy(debugMenuEntry[index], sEntry);

	/* Re-open it if already open to recalculate height */
	if(DebugMenuUp && bAddingNew)
	{
		intCloseDebugMenuNoAnim();
		(void)addDebugMenu(TRUE);
	}

	return TRUE;
}
