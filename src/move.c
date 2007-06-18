/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
	Copyright (C) 2005-2007  Warzone Resurrection Project

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
/*
 * Move.c
 *
 * Routines for moving units about the map
 *
 */

#include <stdio.h>
#include <string.h>

#include "lib/framework/frame.h"
#include "lib/framework/trig.h"

//#define DEBUG_DRIVE_SPEED

#include "objects.h"
#include "move.h"
#include "findpath.h"
#include "visibility.h"
#include "map.h"
#include "fpath.h"
#include "loop.h"
#include "lib/gamelib/gtime.h"
#include "lib/sound/audio.h"
#include "lib/sound/audio_id.h"
#include "geometry.h"
#include "lib/gamelib/animobj.h"
#include "anim_id.h"
#include "formationdef.h"
#include "formation.h"
#include "action.h"
#include "display3d.h"
#include "order.h"
#include "astar.h"
#include "combat.h"
#include "mapgrid.h"
#include "display.h"	// needed for widgetsOn flag.
#include "effects.h"
#include "lib/framework/fractions.h"
#include "power.h"
#include "scores.h"

#include "optimisepath.h"

//#include "multigifts.h"
#include "drive.h"

#ifdef ARROWS
#include "arrow.h"
#endif


#include "lib/netplay/netplay.h"
#include "multiplay.h"
#include "multigifts.h"





//static BOOL DebugP=FALSE;


/* system definitions */

#define	DROID_RUN_SOUND			1

#define	FORMATIONS_DISABLE		0

/* max and min vtol heights above terrain */
#define	VTOL_HEIGHT_MIN				250
#define	VTOL_HEIGHT_LEVEL			300
#define	VTOL_HEIGHT_MAX				350

/* minimum distance for cyborgs to jump */
#define	CYBORG_MIN_JUMP_DISTANCE	500
#define	CYBORG_MIN_JUMP_HEIGHT		50
#define	CYBORG_MAX_JUMP_HEIGHT		200

/* Radius of a droid for collision detection */
#define DROID_SIZE		64

// Maximum size of an object for collision
#define OBJ_MAXRADIUS	(TILE_UNITS * 4)

// How close to a way point the next target starts to phase in
#define WAYPOINT_DIST	(TILE_UNITS *2)
#define WAYPOINT_DSQ	(WAYPOINT_DIST*WAYPOINT_DIST)

// Accuracy for the boundary vector
#define BOUND_ACC		1000

/* Width and length of the droid collision box */
#define HITBOX_WIDTH	128
#define HITBOX_LENGTH	(HITBOX_WIDTH * 3)
/* Angle covered by hit box at far end */
#define HITBOX_ANGLE	36

/* How close to a way point a droid has to get before going to the next */
#define DROID_HIT_DIST	16

// how long a shuffle can propagate before they all stop
#define MOVE_SHUFFLETIME	10000

// Length of time a droid has to be stationery to be considered blocked
#define BLOCK_TIME		6000
#define SHUFFLE_BLOCK_TIME	2000
// How long a droid has to be stationary before stopping trying to move
#define BLOCK_PAUSETIME	1500
#define BLOCK_PAUSERELEASE 500
// How far a droid has to move before it is no longer 'stationary'
#define BLOCK_DIST		64
// How far a droid has to rotate before it is no longer 'stationary'
#define BLOCK_DIR		90
// The min and max ratios of target/obstruction distances for an obstruction
// to be on the target
#define BLOCK_MINRATIO	FRACTCONST(99,100)
#define BLOCK_MAXRATIO	FRACTCONST(101,100)
// The result of the dot product for two vectors to be the same direction
#define BLOCK_DOTVAL	FRACTCONST(99,100)

// How far out from an obstruction to start avoiding it
#define AVOID_DIST		(TILE_UNITS*2)

/* Number of game units/sec for base speed */
#define BASE_SPEED		1

/* Number of degrees/sec for base turn rate */
#define BASE_TURN		1

/* What the base speed is intialised to */
#define BASE_SPEED_INIT		FRACTCONST(BASE_SPEED, BASE_DEF_RATE)

/* What the frame rate is assumed to be at start up */
#define BASE_DEF_RATE	25

/* What the base turn rate is intialised to */
#define BASE_TURN_INIT		FRACTCONST(BASE_TURN, BASE_DEF_RATE)

// maximum and minimum speed to approach a final way point
#define MAX_END_SPEED		300
#define MIN_END_SPEED		60

// distance from final way point to start slowing
#define END_SPEED_RANGE		(3 * TILE_UNITS)


// times for rerouting
#define REROUTE_BASETIME	200
#define REROUTE_RNDTIME		400

// how long to pause after firing a FOM_NO weapon
#define FOM_MOVEPAUSE		1500

// distance to consider droids for a shuffle
#define SHUFFLE_DIST		(3*TILE_UNITS/2)
// how far to move for a shuffle
#define SHUFFLE_MOVE		(2*TILE_UNITS/2)

/***********************************************************************************/
/*                      Slope defines                                              */

// angle after which the droid starts to turn back down a slope
#define SLOPE_TURN_ANGLE	50

// max and min ranges of roll for controlling how much to turn
#define SLOPE_MIN_ROLL		5
#define SLOPE_MAX_ROLL		30

// base amount to turn
#define SLOPE_DIR_CHANGE	20

/***********************************************************************************/
/*             Tracked model defines                                               */

// The magnitude of direction change required for a droid to spin on the spot
#define TRACKED_SPIN_ANGLE		(TRIG_DEGREES/8)
// The speed at which tracked droids spin
#define TRACKED_SPIN_SPEED		200
// The speed at which tracked droids turn while going forward
#define TRACKED_TURN_SPEED		60
// How fast a tracked droid accelerates
#define TRACKED_ACCEL			250
// How fast a tracked droid decelerates
#define TRACKED_DECEL			800
// How fast a tracked droid decelerates
#define TRACKED_SKID_DECEL		600
// How fast a wheeled droid decelerates
#define WHEELED_SKID_DECEL		350
// How fast a hover droid decelerates
#define HOVER_SKID_DECEL		120


/************************************************************************************/
/*             Person model defines                                                 */

// The magnitude of direction change required for a person to spin on the spot
#define PERSON_SPIN_ANGLE		(TRIG_DEGREES/8)
// The speed at which people spin
#define PERSON_SPIN_SPEED		500
// The speed at which people turn while going forward
#define PERSON_TURN_SPEED		250
// How fast a person accelerates
#define PERSON_ACCEL			250
// How fast a person decelerates
#define PERSON_DECEL			450


/************************************************************************************/
/*             VTOL model defines                                                 */

// The magnitude of direction change required for a vtol to spin on the spot
#define VTOL_SPIN_ANGLE			(TRIG_DEGREES)
// The speed at which vtols spin (ignored now!)
#define VTOL_SPIN_SPEED			100
// The speed at which vtols turn while going forward (ignored now!)
#define VTOL_TURN_SPEED			100
// How fast vtols accelerate
#define VTOL_ACCEL				200
// How fast vtols decelerate
#define VTOL_DECEL				200
// How fast vtols 'skid'
#define VTOL_SKID_DECEL			600


/* The current base speed for this frame and averages for the last few seconds */
float	baseSpeed;
#define	BASE_FRAMES			10
UDWORD	baseTimes[BASE_FRAMES];

/* The current base turn rate */
float	baseTurn;

// The next object that should get the router when a lot of units are
// in a MOVEROUTE state
DROID	*psNextRouteDroid;

/* Function prototypes */
BOOL	tileInRange(UDWORD tileX, UDWORD tileY, DROID *psDroid);
void	fillNewBlocks(DROID *psDroid);
void	fillInitialView(DROID *psDroid);
void	moveUpdatePersonModel(DROID *psDroid, SDWORD speed, SDWORD direction);
// Calculate the boundary vector
void	moveCalcBoundary(DROID *psDroid);
/* Turn a vector into an angle - returns a float (!) */
static float vectorToAngle(float vx, float vy);

/* Calculate the angle between two normalised vectors */
#define VECTOR_ANGLE(vx1,vy1, vx2,vy2) \
	trigInvCos(FRACTmul(vx1, vx2) + FRACTmul(vy1,vy2))

// Abbreviate some of the float defines
#define MKF(x)		MAKEFRACT(x)
#define MKI(x)		MAKEINT(x)
#define Fmul(x,y)	FRACTmul(x,y)
#define Fdiv(x,y)	FRACTdiv(x,y)

//typedef enum MOVESOUNDTYPE	{ MOVESOUNDSTART, MOVESOUNDIDLE, MOVESOUNDMOVEOFF,
//								MOVESOUNDMOVE, MOVESOUNDSTOPHISS, MOVESOUNDSHUTDOWN };

extern UDWORD	selectedPlayer;


static BOOL	g_bFormationSpeedLimitingOn = TRUE;




/* Initialise the movement system */
BOOL moveInitialise(void)
{
	UDWORD i;

	// Initialise the base speed counters
	baseSpeed = BASE_SPEED_INIT;
	baseTurn = BASE_TURN_INIT;
	for (i=0; i< BASE_FRAMES; i++)
	{
		baseTimes[i] = GAME_TICKS_PER_SEC / BASE_DEF_RATE;
	}

	psNextRouteDroid = NULL;

	return TRUE;
}

/* Update the base speed for all movement */
void moveUpdateBaseSpeed(void)
{
//	UDWORD	totalTime=0, numFrames=0, i;
	UDWORD	totalTime=0, i;

	// Update the list of frame times
	for(i=0; i<BASE_FRAMES-1; i++)
	{
		baseTimes[i]= baseTimes[i+1];
	}
	baseTimes[BASE_FRAMES-1] = frameTime;

	// Add up the time for the last few frames
	for(i=0; i<BASE_FRAMES; i++)
	{
		totalTime += baseTimes[i];
	}

	// Set the base speed
	// here is the original calculation before the fract stuff
	// baseSpeed = (totalTime * BASE_SPEED) / (GAME_TICKS_PER_SEC * BASE_FRAMES);
	baseSpeed = FRACTdiv( FRACTmul(MAKEFRACT(BASE_SPEED), MAKEFRACT(totalTime)),
						  FRACTmul(MAKEFRACT(GAME_TICKS_PER_SEC), MAKEFRACT(BASE_FRAMES)) );

	// Set the base turn rate
	baseTurn = FRACTdiv( FRACTmul(MAKEFRACT(BASE_TURN), MAKEFRACT(totalTime)),
						 FRACTmul(MAKEFRACT(GAME_TICKS_PER_SEC), MAKEFRACT(BASE_FRAMES)) );


	// reset the astar counters
	astarResetCounters();

	// check the waiting droid pointer
	if (psNextRouteDroid != NULL)
	{
		if ((psNextRouteDroid->died) ||
			((psNextRouteDroid->sMove.Status != MOVEROUTE) &&
			 (psNextRouteDroid->sMove.Status != MOVEROUTESHUFFLE)))
		{
			debug( LOG_MOVEMENT, "Waiting droid %d (player %d) reset\n", psNextRouteDroid->id, psNextRouteDroid->player);
			psNextRouteDroid = NULL;
		}
	}
}

/* Set a target location for a droid to move to */
// Now returns a BOOL based on the success of the routing
// returns TRUE if the routing was successful ... if FALSE then the calling code should not try to route here again for a while
static BOOL _moveDroidToBase(DROID	*psDroid, UDWORD x, UDWORD y, BOOL bFormation)
{
	FPATH_RETVAL		retVal = FPR_OK;
	SDWORD				fmx1,fmy1, fmx2,fmy2;

	ASSERT( psDroid != NULL,
		"moveUnitTo: Invalid unit pointer" );


	if(bMultiPlayer && (psDroid->sMove.Status != MOVEWAITROUTE))
	{
		if(SendDroidMove(psDroid,x,y,bFormation) == FALSE)
		{// dont make the move since we'll recv it anyway
			return FALSE;
		}
	}


//	DBPRINTF(("movedroidto (%d,%d) -> (%d,%d)\n",psDroid->x,psDroid->y,x,y);

#if FORMATIONS_DISABLE
	retVal = FPR_OK;
	fpathSetDirectRoute(psDroid, (SDWORD)x, (SDWORD)y);
#else
    //in multiPlayer make Transporter move like the vtols
	if ( psDroid->droidType == DROID_TRANSPORTER && game.maxPlayers == 0)
	{
		fpathSetDirectRoute((BASE_OBJECT *)psDroid, (SDWORD)x, (SDWORD)y);
		psDroid->sMove.Status = MOVENAVIGATE;
		psDroid->sMove.Position=0;
		psDroid->sMove.psFormation = NULL;
		return TRUE;
	}
	else if (vtolDroid(psDroid) || (game.maxPlayers > 0 && psDroid->
        droidType == DROID_TRANSPORTER))
	{
		fpathSetDirectRoute((BASE_OBJECT *)psDroid, (SDWORD)x, (SDWORD)y);
		retVal = FPR_OK;
	}
	else
	{
		retVal = fpathRoute((BASE_OBJECT *)psDroid, &(psDroid->sMove), (SDWORD)x,(SDWORD)y);
	}
#endif
	// ----
	// ----

	/* check formations */
	if ( retVal == FPR_OK )
	{
		debug( LOG_MOVEMENT, "unit(%d): base Speed %d, speed %d\n",
			 psDroid->id, psDroid->baseSpeed, MAKEINT(psDroid->sMove.speed));

		// bit of a hack this - john
		// if astar doesn't have a complete route, it returns a route to the nearest clear tile.
		// the location of the clear tile is in DestinationX,DestinationY.
		// reset x,y to this position so the formation gets set up correctly
		x = psDroid->sMove.DestinationX;
		y = psDroid->sMove.DestinationY;

		psDroid->sMove.Status = MOVENAVIGATE;
		psDroid->sMove.Position=0;
		psDroid->sMove.fx = MAKEFRACT(psDroid->x);
		psDroid->sMove.fy = MAKEFRACT(psDroid->y);

		psDroid->sMove.fz = MAKEFRACT(psDroid->z);


		// reset the next route droid
		if (psDroid == psNextRouteDroid)
		{
			debug( LOG_MOVEMENT, "Waiting droid %d (player %d) got route\n", psDroid->id, psDroid->player);
			psNextRouteDroid = NULL;
		}

//		DBPRINTF(("moveDroidTo: form %p id %d\n",psDroid->sMove.psFormation, psDroid->id));

		// leave any old formation
		if (psDroid->sMove.psFormation)
		{
			formationLeave(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid);
			psDroid->sMove.psFormation = NULL;
		}

#if !FORMATIONS_DISABLE
		if (bFormation)
		{
			// join a formation if it exists at the destination
			if (formationFind(&psDroid->sMove.psFormation, (SDWORD)x,(SDWORD)y))
			{
				formationJoin(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid);
			}
			else
			{
				// align the formation with the last path of the route
				fmx2 = psDroid->sMove.asPath[psDroid->sMove.numPoints -1].x;
				fmy2 = psDroid->sMove.asPath[psDroid->sMove.numPoints -1].y;
				fmx2 = (fmx2 << TILE_SHIFT) + TILE_UNITS/2;
				fmy2 = (fmy2 << TILE_SHIFT) + TILE_UNITS/2;
				if (psDroid->sMove.numPoints == 1)
				{
					fmx1 = (SDWORD)psDroid->x;
					fmy1 = (SDWORD)psDroid->y;
				}
				else
				{
					fmx1 = psDroid->sMove.asPath[psDroid->sMove.numPoints -2].x;
					fmy1 = psDroid->sMove.asPath[psDroid->sMove.numPoints -2].y;
					fmx1 = (fmx1 << TILE_SHIFT) + TILE_UNITS/2;
					fmy1 = (fmy1 << TILE_SHIFT) + TILE_UNITS/2;
				}

				// no formation so create a new one
				if (formationNew(&psDroid->sMove.psFormation, FT_LINE, (SDWORD)x,(SDWORD)y,
						(SDWORD)calcDirection(fmx1,fmy1, fmx2,fmy2)))
				{
					formationJoin(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid);
				}
			}
		}
//		DBPRINTF(("moveDroidTo: form %p id %d end\n",psDroid->sMove.psFormation, psDroid->id));
#endif

	}
	else if (retVal == FPR_RESCHEDULE)
	{
		debug( LOG_MOVEMENT, "reroute: id %d\n", psDroid->id);

		// maxed out routing time this frame - do it next time
		psDroid->sMove.DestinationX = x;
		psDroid->sMove.DestinationY = y;

		if ((psDroid->sMove.Status != MOVEROUTE) &&
			(psDroid->sMove.Status != MOVEROUTESHUFFLE))
		{
			debug( LOG_MOVEMENT, "Unit %d (player %d) started waiting at %d\n", psDroid->id, psDroid->player, gameTime);

			psDroid->sMove.Status = MOVEROUTE;

			// note when the unit first tried to route
			psDroid->sMove.bumpTime = gameTime;

//			psDroid->sMove.bumpTime = gameTime + REROUTE_BASETIME + REROUTE_RNDTIME - (rand()%REROUTE_RNDTIME);
		}
	}
	else if (retVal == FPR_WAIT)
	{
		// reset the next route droid
		if (psDroid == psNextRouteDroid)
		{
			debug( LOG_MOVEMENT, "Waiting droid %d (player %d) got route\n", psDroid->id, psDroid->player);
			psNextRouteDroid = NULL;
		}

		// the route will be calculated over a number of frames
		psDroid->sMove.Status = MOVEWAITROUTE;
		psDroid->sMove.DestinationX = x;
		psDroid->sMove.DestinationY = y;
	}
	else // if (retVal == FPR_FAILED)
	{
		psDroid->sMove.Status = MOVEINACTIVE;
		actionDroid(psDroid, DACTION_SULK);
//		DBPRINTF(("mdt: FALSE\n");
		return(FALSE);
	}

	return TRUE;
}


// Shame about this but the find path code uses too much stack space
// so we can't safely run it in the dcache.
//
static BOOL moveDroidToBase(DROID	*psDroid, UDWORD x, UDWORD y, BOOL bFormation)
{

	return _moveDroidToBase(psDroid,x,y,bFormation);
}


// move a droid to a location, joining a formation
BOOL moveDroidTo(DROID *psDroid, UDWORD x,UDWORD y)
{
	return moveDroidToBase(psDroid,x,y, TRUE);
}

// move a droid to a location, not joining a formation
BOOL moveDroidToNoFormation(DROID *psDroid, UDWORD x,UDWORD y)
{
	return moveDroidToBase(psDroid,x,y, FALSE);
}


// move a droid directly to a location (used by vtols only)
void moveDroidToDirect(DROID *psDroid, UDWORD x, UDWORD y)
{
	ASSERT( psDroid != NULL && vtolDroid(psDroid),
		"moveUnitToDirect: only valid for a vtol unit" );

	fpathSetDirectRoute((BASE_OBJECT *)psDroid, (SDWORD)x, (SDWORD)y);
	psDroid->sMove.Status = MOVENAVIGATE;
	psDroid->sMove.Position=0;

	// leave any old formation
	if (psDroid->sMove.psFormation)
	{
		formationLeave(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid);
		psDroid->sMove.psFormation = NULL;
	}
}


// Get a droid to turn towards a locaton
void moveTurnDroid(DROID *psDroid, UDWORD x, UDWORD y)
{
	SDWORD moveDir = (SDWORD)calcDirection(psDroid->x, psDroid->y, x, y);

	if ( (SDWORD)(psDroid->direction) != moveDir )
	{
		psDroid->sMove.targetX = (SDWORD)x;
		psDroid->sMove.targetY = (SDWORD)y;
		psDroid->sMove.Status = MOVETURNTOTARGET;
	}
}

// get the difference in direction
static SDWORD moveDirDiff(SDWORD start, SDWORD end)
{
	SDWORD retval, diff;

	diff = end - start;

	if (diff > 0)
	{
		if (diff < 180)
		{
			retval = diff;
		}
		else
		{
			retval = diff - 360;
		}
	}
	else
	{
		if (diff > -180)
		{
			retval = diff;
		}
		else
		{
			retval = 360 + diff;
		}
	}

	return retval;
}

// Tell a droid to move out the way for a shuffle
static void moveShuffleDroid(DROID *psDroid, UDWORD shuffleStart, SDWORD sx, SDWORD sy)
{
	float	shuffleDir, droidDir;
	DROID	*psCurr;
	SDWORD	xdiff,ydiff, mx,my, shuffleMag, diff;
	BOOL	frontClear = TRUE, leftClear = TRUE, rightClear = TRUE;
	SDWORD	lvx,lvy, rvx,rvy, svx,svy;
	SDWORD	shuffleMove;
	SDWORD	tarX,tarY;

	shuffleDir = vectorToAngle(MKF(sx),MKF(sy));
	shuffleMag = (SDWORD)sqrtf(sx*sx + sy*sy);

	if (shuffleMag == 0)
	{
		return;
	}

	shuffleMove = SHUFFLE_MOVE;
/*	if (vtolDroid(psDroid))
	{
		shuffleMove /= 4;
	}*/

	// calculate the possible movement vectors
	lvx = -sy * shuffleMove / shuffleMag;
	lvy = sx * shuffleMove / shuffleMag;

	rvx = sy * shuffleMove / shuffleMag;
	rvy = -sx * shuffleMove / shuffleMag;

	svx = lvy; // sx * SHUFFLE_MOVE / shuffleMag;
	svy = rvx; // sy * SHUFFLE_MOVE / shuffleMag;

	// check for blocking tiles
	if ( fpathBlockingTile( ((SDWORD)psDroid->x + lvx) >> TILE_SHIFT,
						    ((SDWORD)psDroid->y + lvy) >> TILE_SHIFT ) )
	{
		leftClear = FALSE;
	}
	else if ( fpathBlockingTile( ((SDWORD)psDroid->x + rvx) >> TILE_SHIFT,
								 ((SDWORD)psDroid->y + rvy) >> TILE_SHIFT ) )
	{
		rightClear = FALSE;
	}
	else if ( fpathBlockingTile( ((SDWORD)psDroid->x + svx) >> TILE_SHIFT,
								 ((SDWORD)psDroid->y + svy) >> TILE_SHIFT ) )
	{
		frontClear = FALSE;
	}

	// find any droids that could block the shuffle
	for(psCurr=apsDroidLists[psDroid->player]; psCurr; psCurr=psCurr->psNext)
	{
		if (psCurr != psDroid)
		{
			xdiff = (SDWORD)psCurr->x - (SDWORD)psDroid->x;
			ydiff = (SDWORD)psCurr->y - (SDWORD)psDroid->y;
			if (xdiff*xdiff + ydiff*ydiff < SHUFFLE_DIST*SHUFFLE_DIST)
			{
				droidDir = vectorToAngle(MKF(xdiff),MKF(ydiff));
				diff = (SDWORD)moveDirDiff(MKI(shuffleDir), MKI(droidDir));
				if ((diff > -135) && (diff < -45))
				{
					leftClear = FALSE;
				}
				else if ((diff > 45) && (diff < 135))
				{
					rightClear = FALSE;
				}
			}
		}
	}

	// calculate a target
	if (leftClear)
	{
		mx = lvx;
		my = lvy;
	}
	else if (rightClear)
	{
		mx = rvx;
		my = rvy;
	}
	else if (frontClear)
	{
		mx = svx;
		my = svy;
	}
	else
	{
		// nowhere to shuffle to, quit
		return;
	}

	// check the location for vtols
	tarX = (SDWORD)psDroid->x + mx;
	tarY = (SDWORD)psDroid->y + my;
	if (vtolDroid(psDroid))
	{
		actionVTOLLandingPos(psDroid, (UDWORD *)&tarX,(UDWORD *)&tarY);
	}

	// set up the move state
	if (psDroid->sMove.Status == MOVEROUTE)
	{
		psDroid->sMove.Status = MOVEROUTESHUFFLE;
	}
	else
	{
		psDroid->sMove.Status = MOVESHUFFLE;
	}
//	psDroid->sMove.shuffleX = (SWORD)sx;
//	psDroid->sMove.shuffleY = (SWORD)sy;
	psDroid->sMove.shuffleStart = shuffleStart;
	psDroid->sMove.srcX = (SDWORD)psDroid->x;
	psDroid->sMove.srcY = (SDWORD)psDroid->y;
//	psDroid->sMove.targetX = (SDWORD)psDroid->x + mx;
//	psDroid->sMove.targetY = (SDWORD)psDroid->y + my;
	psDroid->sMove.targetX = tarX;
	psDroid->sMove.targetY = tarY;
	// setting the Destination could overwrite a MOVEROUTE's destination
	// it is not actually needed for a shuffle anyway
//	psDroid->sMove.DestinationX = psDroid->sMove.targetX;
//	psDroid->sMove.DestinationY = psDroid->sMove.targetY;
//	psDroid->sMove.bumpTime = 0;
	psDroid->sMove.numPoints = 0;
	psDroid->sMove.Position = 0;
	psDroid->sMove.fx = MAKEFRACT(psDroid->x);
	psDroid->sMove.fy = MAKEFRACT(psDroid->y);

	psDroid->sMove.fz = MAKEFRACT(psDroid->z);

	moveCalcBoundary(psDroid);

	if (psDroid->sMove.psFormation != NULL)
	{
		formationLeave(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid);
		psDroid->sMove.psFormation = NULL;
	}
}


/* Stop a droid */
void moveStopDroid(DROID *psDroid)
{
	PROPULSION_STATS	*psPropStats;

	ASSERT( psDroid != NULL,
		"moveStopUnit: Invalid unit pointer" );

	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	ASSERT( psPropStats != NULL,
			"moveUpdateUnit: invalid propulsion stats pointer" );

	if ( psPropStats->propulsionType == LIFT )
	{
		psDroid->sMove.Status = MOVEHOVER;
	}
	else
	{
		psDroid->sMove.Status = MOVEINACTIVE;
	}
}

/*Stops a droid dead in its tracks - doesn't allow for any little skidding bits*/
void moveReallyStopDroid(DROID *psDroid)
{
    ASSERT( psDroid != NULL,
        "moveReallyStopUnit: invalid unit pointer" );

    psDroid->sMove.Status = MOVEINACTIVE;
    psDroid->sMove.speed = MKF(0);
}


#define PITCH_LIMIT 150

/* Get pitch and roll from direction and tile data */
void updateDroidOrientation(DROID *psDroid)
{
	SDWORD hx0, hx1, hy0, hy1, w;
	SDWORD newPitch, dPitch, pitchLimit;
	double dx, dy;
	double direction, pitch, roll;
	ASSERT( psDroid->x < (mapWidth << TILE_SHIFT),
		"mapHeight: x coordinate bigger than map width" );
	ASSERT( psDroid->y < (mapHeight<< TILE_SHIFT),
		"mapHeight: y coordinate bigger than map height" );


	//if(psDroid->droidType == DROID_PERSON || psDroid->droidType == DROID_CYBORG ||
    if(psDroid->droidType == DROID_PERSON || cyborgDroid(psDroid) ||
        psDroid->droidType == DROID_TRANSPORTER)
	{
		/* These guys always stand upright */
		return;
	}

	w = 20;
	hx0 = map_Height(psDroid->x + w, psDroid->y);
	hx1 = map_Height(psDroid->x - w, psDroid->y);
	hy0 = map_Height(psDroid->x, psDroid->y + w);
	hy1 = map_Height(psDroid->x, psDroid->y - w);

	//update height in case were in the bottom of a trough
	if (((hx0 +hx1)/2) > (SDWORD)psDroid->z)
	{
		psDroid->z = (UWORD)((hx0 +hx1)/2);
	}
	if (((hy0 +hy1)/2) > (SDWORD)psDroid->z)
	{
		psDroid->z = (UWORD)((hy0 +hy1)/2);
	}

	dx = (double)(hx0 - hx1) / ((double)w * 2.0);
	dy = (double)(hy0 - hy1) / ((double)w * 2.0);
	//dx is atan of angle of elevation along x axis
	//dx is atan of angle of elevation along y axis
	//body
	direction = (M_PI * psDroid->direction) / 180.0;
	pitch = sin(direction) * dx + cos(direction) * dy;
	pitch = atan(pitch);
	newPitch = (SDWORD)((pitch * 180) / M_PI);
	//limit the rate the front comes down to simulate momentum
	pitchLimit = PITCH_LIMIT * frameTime/GAME_TICKS_PER_SEC;
	dPitch = newPitch - psDroid->pitch;
	if (dPitch < 0)
	{
		dPitch +=360;
	}
	if ((dPitch > 180) && (dPitch < 360 - pitchLimit))
	{
		psDroid->pitch = (SWORD)(psDroid->pitch - pitchLimit);
	}
	else
	{
		psDroid->pitch = (SWORD)newPitch;
	}
	roll = cos(direction) * dx - sin(direction) * dy;
	roll = atan(roll);
	psDroid->roll = (UWORD)((roll * 180) / M_PI);
	//turret
/*	direction = (PI * psDroid->turretDirection) / 180.0;
	pitch = sin(direction) * dx + cos(direction) * dy;
	pitch = atan(pitch);
	psDroid->turretPitch = (UDWORD)((pitch * 180) / M_PI);
	roll = cos(direction) * dx - sin(direction) * dy;
	roll = atan(roll);
	psDroid->turretRoll = (UDWORD)((roll * 180) / M_PI);
*/
	return;
}


/* Turn a vector into an angle - returns a float (!) */
static float vectorToAngle(float vx, float vy)
{
	float	angle;	// Angle in degrees (0->360)

	angle = (float)(TRIG_DEGREES * atan2(-vy,vx) / M_PI / 2);
	angle += TRIG_DEGREES/4;
	if (angle < 0)
	{
		angle += TRIG_DEGREES;
	}

	if(angle>=360.0f)
	{
		angle -= 360.0f;
	}



	return angle;
}


/* Calculate the change in direction given a target angle and turn rate */
static void moveCalcTurn(float *pCurr, float target, UDWORD rate)
{
	float diff, change, retval = *pCurr;

	ASSERT( target < 360.0 && target >= 0.0,
			 "moveCalcTurn: target out of range %f", target );
	ASSERT( retval < 360.0 && retval >= 0.0,
			 "moveCalcTurn: cur ang out of range %f", retval );

	// calculate the difference in the angles
	diff = target - retval;

	// calculate the change in direction

	change = (baseTurn * rate); // constant rate so we can use a normal multiplication

	if ((diff >= 0 && diff < change) ||
	    (diff < 0 && diff > -change))
	{
		// got to the target direction
		retval = target;
	}
	else if (diff > 0)
	{
		// Target dir is greater than current
		if (diff < TRIG_DEGREES / 2)
		{
			// Simple case - just increase towards target
			retval += change;
		}
		else
		{
			// decrease to target, but could go over 0 boundary */
			retval -= change;
		}
	}
	else
	{
		if (diff > -(TRIG_DEGREES/2))
		{
			// Simple case - just decrease towards target
			retval -= change;
		}
		else
		{
			// increase to target, but could go over 0 boundary
			retval += change;
		}
	}

	while (retval < 0.0f)
	{
		retval += 360.0f;
	}
	retval = fmodf(retval, 360);

	ASSERT(retval < 360 && retval >= 0, "moveCalcTurn: bad angle %f from (%f, %f, %u)\n",
	       retval, *pCurr, target, rate);

	*pCurr = retval;
}


/* Get the next target point from the route */
static BOOL moveNextTarget(DROID *psDroid)
{
	UDWORD	srcX,srcY, tarX, tarY;

	// See if there is anything left in the move list
	if (psDroid->sMove.Position == psDroid->sMove.numPoints)
	{
		return FALSE;
	}

	tarX = (psDroid->sMove.asPath[psDroid->sMove.Position].x << TILE_SHIFT) + TILE_UNITS/2;
	tarY = (psDroid->sMove.asPath[psDroid->sMove.Position].y << TILE_SHIFT) + TILE_UNITS/2;
	if (psDroid->sMove.Position == 0)
	{
		psDroid->sMove.srcX = (SDWORD)psDroid->x;
		psDroid->sMove.srcY = (SDWORD)psDroid->y;
	}
	else
	{
		srcX = (psDroid->sMove.asPath[psDroid->sMove.Position -1].x << TILE_SHIFT) + TILE_UNITS/2;
		srcY = (psDroid->sMove.asPath[psDroid->sMove.Position -1].y << TILE_SHIFT) + TILE_UNITS/2;
		psDroid->sMove.srcX = srcX;
		psDroid->sMove.srcY = srcY ;
	}
	psDroid->sMove.targetX = tarX;
	psDroid->sMove.targetY = tarY;
	psDroid->sMove.Position++;


	return TRUE;
}

/* Look at the next target point from the route */
static void movePeekNextTarget(DROID *psDroid, SDWORD *pX, SDWORD *pY)
{
	SDWORD	xdiff, ydiff;

	// See if there is anything left in the move list
	if (psDroid->sMove.Position == psDroid->sMove.numPoints)
	{
		// No points left - fudge one to continue the same direction
		xdiff = psDroid->sMove.targetX - psDroid->sMove.srcX;
		ydiff = psDroid->sMove.targetY - psDroid->sMove.srcY;
		*pX = psDroid->sMove.targetX + xdiff;
		*pY = psDroid->sMove.targetY + ydiff;
	}
	else
	{
		*pX = (psDroid->sMove.asPath[psDroid->sMove.Position].x << TILE_SHIFT) + TILE_UNITS/2;
		*pY = (psDroid->sMove.asPath[psDroid->sMove.Position].y << TILE_SHIFT) + TILE_UNITS/2;
	}
}

// Watermelon:fix these magic number...the collision radius should be based on pie imd radius not some static int's...
static	int mvPersRad = 20, mvCybRad = 30, mvSmRad = 40, mvMedRad = 50, mvLgRad = 60;

// Get the radius of a base object for collision
static SDWORD moveObjRadius(BASE_OBJECT *psObj)
{
	SDWORD	radius;
	BODY_STATS	*psBdyStats;

	switch (psObj->type)
	{
	case OBJ_DROID:
		if ( ((DROID *)psObj)->droidType == DROID_PERSON )
		{
			radius = mvPersRad;
		}
		//else if ( ((DROID *)psObj)->droidType == DROID_CYBORG )
        else if (cyborgDroid((DROID *)psObj))
		{
			radius = mvCybRad;
		}
		else
		{
			radius = psObj->sDisplay.imd->radius;
			psBdyStats = asBodyStats + ((DROID *)psObj)->asBits[COMP_BODY].nStat;
			switch (psBdyStats->size)
			{
			default:
			case SIZE_LIGHT:
				radius = mvSmRad;
				break;
			case SIZE_MEDIUM:
				radius = mvMedRad;
				break;
			case SIZE_HEAVY:
				radius = mvLgRad;
				break;
			case SIZE_SUPER_HEAVY:
				radius = 130;
				break;
			}
		}
		break;
	case OBJ_STRUCTURE:
//		radius = psObj->sDisplay.imd->visRadius;
		radius = psObj->sDisplay.imd->radius/2;
		break;
	case OBJ_FEATURE:
//		radius = psObj->sDisplay.imd->visRadius;
		radius = psObj->sDisplay.imd->radius/2;
		break;
	default:
		ASSERT( FALSE,"moveObjRadius: unknown object type" );
		radius = 0;
		break;
	}

	return radius;
}


// see if a Droid has run over a person
static void moveCheckSquished(DROID *psDroid, float mx,float my)
{
	SDWORD		i, droidR, rad, radSq;
	SDWORD		objR;
	SDWORD		xdiff,ydiff, distSq;
	NAYBOR_INFO	*psInfo;

	droidR = moveObjRadius((BASE_OBJECT *)psDroid);

	for(i=0; i<(SDWORD)numNaybors; i++)
	{
		psInfo = asDroidNaybors + i;
		if (psInfo->psObj->type != OBJ_DROID ||
			((DROID *)psInfo->psObj)->droidType != DROID_PERSON)
		{
			// ignore everything but people
			continue;
		}

		ASSERT( psInfo->psObj->type == OBJ_DROID &&
				((DROID *)psInfo->psObj)->droidType == DROID_PERSON,
			"squished - eerk" );

		objR = moveObjRadius(psInfo->psObj);
		rad = droidR + objR;
		radSq = rad*rad;

		xdiff = (SDWORD)psDroid->x + MAKEINT(mx) - (SDWORD)psInfo->psObj->x;
		ydiff = (SDWORD)psDroid->y + MAKEINT(my) - (SDWORD)psInfo->psObj->y;
		distSq = xdiff*xdiff + ydiff*ydiff;

		if (((2*radSq)/3) > distSq)
		{
			if ( (psDroid->player != psInfo->psObj->player) &&
				 !aiCheckAlliances(psDroid->player, psInfo->psObj->player) )
			{
				// run over a bloke - kill him
				destroyDroid((DROID*)psInfo->psObj);
				scoreUpdateVar(WD_BARBARIANS_MOWED_DOWN);
				continue;
			}
		}
		else if (psInfo->distSqr > OBJ_MAXRADIUS*OBJ_MAXRADIUS)
		{
			// object is too far away to be hit
			break;
		}
	}
}


// See if the droid has been stopped long enough to give up on the move
static BOOL moveBlocked(DROID *psDroid)
{
	SDWORD	xdiff,ydiff, diffSq;
	UDWORD	blockTime;

	if ((psDroid->sMove.bumpTime == 0) || (psDroid->sMove.bumpTime > gameTime) ||
		(psDroid->sMove.Status == MOVEROUTE) || (psDroid->sMove.Status == MOVEROUTESHUFFLE))
	{
		// no bump - can't be blocked
		return FALSE;
	}

	// See if the block can be cancelled
	if (dirDiff(psDroid->direction, psDroid->sMove.bumpDir) > BLOCK_DIR)
	{
		// Move on, clear the bump
		psDroid->sMove.bumpTime = 0;
		psDroid->sMove.lastBump = 0;
		return FALSE;
	}
	xdiff = (SDWORD)psDroid->x - (SDWORD)psDroid->sMove.bumpX;
	ydiff = (SDWORD)psDroid->y - (SDWORD)psDroid->sMove.bumpY;
	diffSq = xdiff*xdiff + ydiff*ydiff;
	if (diffSq > BLOCK_DIST*BLOCK_DIST)
	{
		// Move on, clear the bump
		psDroid->sMove.bumpTime = 0;
		psDroid->sMove.lastBump = 0;
		return FALSE;
	}

	if (psDroid->sMove.Status == MOVESHUFFLE)
	{
		blockTime = SHUFFLE_BLOCK_TIME;
	}
	else
	{
		blockTime = BLOCK_TIME;
	}

	if (gameTime - psDroid->sMove.bumpTime > blockTime)
	{
		// Stopped long enough - blocked
		psDroid->sMove.bumpTime = 0;
		psDroid->sMove.lastBump = 0;

		// if the unit cannot see the next way point - reroute it's got stuck
		if ( ( bMultiPlayer || (psDroid->player == selectedPlayer) ) &&
			(psDroid->sMove.Position != psDroid->sMove.numPoints) &&
			!fpathTileLOS((SDWORD)psDroid->x >> TILE_SHIFT, (SDWORD)psDroid->y >> TILE_SHIFT,
						  psDroid->sMove.DestinationX >> TILE_SHIFT, psDroid->sMove.DestinationY >> TILE_SHIFT))
		{
			moveDroidTo(psDroid, psDroid->sMove.DestinationX, psDroid->sMove.DestinationY);
			return FALSE;
		}

		return TRUE;
	}

	return FALSE;
}


// Calculate the actual movement to slide around
static void moveCalcSlideVector(DROID *psDroid,SDWORD objX, SDWORD objY, float *pMx, float *pMy)
{
	SDWORD		obstX, obstY;
	SDWORD		absX, absY;
	SDWORD		dirX, dirY, dirMag;
	float		mx, my, unitX,unitY;
	float		dotRes;

	mx = *pMx;
	my = *pMy;

	// Calculate the vector to the obstruction
	obstX = (SDWORD)psDroid->x - objX;
	obstY = (SDWORD)psDroid->y - objY;

	// if the target dir is the same, don't need to slide
	if (obstX*mx + obstY*my >= 0)
	{
		return;
	}

	// Choose the tangent vector to this on the same side as the target
//	tarX = psDriod->sMove.targetX - (SDWORD)psDroid->x;
//	tarY = psDriod->sMove.targetY - (SDWORD)psDroid->y;
//	dotRes = FRACTmul(MAKEFRACT(obstY),*pMx);
//	dotRes -= FRACTmul(MAKEFRACT(obstX),*pMy);
//	dotRes = obstY * mx - obstX * my;
	dotRes = FRACTmul(MAKEFRACT(obstY),mx) - FRACTmul(MAKEFRACT(obstX),my);
	if (dotRes >= 0)
	{
		dirX = obstY;
		dirY = -obstX;
	}
	else
	{
		dirX = -obstY;
		dirY = obstX;
		dotRes = FRACTmul(MAKEFRACT(dirX),*pMx) + FRACTmul(MAKEFRACT(dirY),*pMy);
	}
	absX = labs(dirX); absY = labs(dirY);
	dirMag = absX > absY ? absX + absY/2 : absY + absX/2;

	// Calculate the component of the movement in the direction of the tangent vector
	unitX = FRACTdiv(MAKEFRACT(dirX), MAKEFRACT(dirMag));
	unitY = FRACTdiv(MAKEFRACT(dirY), MAKEFRACT(dirMag));
	dotRes = FRACTdiv(dotRes, MAKEFRACT(dirMag));
	*pMx = FRACTmul(unitX, dotRes);
	*pMy = FRACTmul(unitY, dotRes);
}


// see if a droid has run into a blocking tile
static void moveCalcBlockingSlide(DROID *psDroid, float *pmx, float *pmy, SDWORD tarDir, SDWORD *pSlideDir)
{
	float	mx = *pmx,my = *pmy, nx,ny;
	SDWORD	tx,ty, ntx,nty;		// current tile x,y and new tile x,y
	SDWORD	blkCX,blkCY;
	SDWORD	horizX,horizY, vertX,vertY;
	SDWORD	intx,inty;
	SDWORD	jumpx,jumpy, bJumped=FALSE;
#ifdef DEBUG
	BOOL	slide =FALSE;
#define NOTE_SLIDE	slide=TRUE
	SDWORD	state = 0;
#define NOTE_STATE(x) state = x
#else
#define NOTE_STATE(x)
#define NOTE_SLIDE
#define NOTE_STATE(x)
#endif
//	float	mag, rad, temp;
	float	radx,rady;
	BOOL	blocked;
	SDWORD	slideDir;

	blocked = FALSE;
	radx = MKF(0);
	rady = MKF(0);

	// calculate the new coords and see if they are on a different tile
	tx = MAKEINT(psDroid->sMove.fx) >> TILE_SHIFT;
	ty = MAKEINT(psDroid->sMove.fy) >> TILE_SHIFT;
	nx = psDroid->sMove.fx + mx;
	ny = psDroid->sMove.fy + my;
	ntx = MAKEINT(nx) >> TILE_SHIFT;
	nty = MAKEINT(ny) >> TILE_SHIFT;

	// is the new tile blocking?
	if (fpathBlockingTile(ntx,nty))
	{
		blocked = TRUE;
	}

	// now test ahead of the droid
/*	if (!blocked)
	{
		rad = MKF(moveObjRadius((BASE_OBJECT *)psDroid));
		mag = sqrtf(mx*mx + my*my);

		if (mag==0)
		{
			*pmx = MKF(0);
			*pmy = MKF(0);
			return;
		}

		radx = (rad * mx) / mag;
		rady = (rad * my) / mag;

		nx = psDroid->sMove.fx + radx;
		ny = psDroid->sMove.fy + rady;
		tx = MAKEINT(nx) >> TILE_SHIFT;
		ty = MAKEINT(ny) >> TILE_SHIFT;
		nx += mx;
		ny += my;
		ntx = MAKEINT(nx) >> TILE_SHIFT;
		nty = MAKEINT(ny) >> TILE_SHIFT;

		// is the new tile blocking?
		if (fpathBlockingTile(ntx,nty))
		{
			blocked = TRUE;
		}
	}

	// now test one side of the droid
	if (!blocked)
	{
		nx = psDroid->sMove.fx - rady;
		ny = psDroid->sMove.fy + radx;
		tx = MAKEINT(nx) >> TILE_SHIFT;
		ty = MAKEINT(ny) >> TILE_SHIFT;
		nx += mx;
		ny += my;
		ntx = MAKEINT(nx) >> TILE_SHIFT;
		nty = MAKEINT(ny) >> TILE_SHIFT;

		// is the new tile blocking?
		if (fpathBlockingTile(ntx,nty))
		{
			blocked = TRUE;
			temp = radx;
			radx = -rady;
			rady = radx;
		}
	}

	// now test the other side of the droid
	if (!blocked)
	{
		nx = psDroid->sMove.fx + rady;
		ny = psDroid->sMove.fy - radx;
		tx = MAKEINT(nx) >> TILE_SHIFT;
		ty = MAKEINT(ny) >> TILE_SHIFT;
		nx += mx;
		ny += my;
		ntx = MAKEINT(nx) >> TILE_SHIFT;
		nty = MAKEINT(ny) >> TILE_SHIFT;

		// is the new tile blocking?
		if (fpathBlockingTile(ntx,nty))
		{
			blocked = TRUE;
			temp = radx;
			radx = rady;
			rady = -radx;
		}
	}*/

	blkCX = (ntx << TILE_SHIFT) + TILE_UNITS/2;
	blkCY = (nty << TILE_SHIFT) + TILE_UNITS/2;

	// is the new tile blocking?
	if (!blocked)
	{
		// not blocking, don't change the move vector
		return;
	}

	// if the droid is shuffling - just stop
	if (psDroid->sMove.Status == MOVESHUFFLE)
	{
		psDroid->sMove.Status = MOVEINACTIVE;
	}

	// note the bump time and position if necessary
	if (!vtolDroid(psDroid) &&
		psDroid->sMove.bumpTime == 0)
	{
		psDroid->sMove.bumpTime = gameTime;
		psDroid->sMove.lastBump = 0;
		psDroid->sMove.pauseTime = 0;
		psDroid->sMove.bumpX = psDroid->x;
		psDroid->sMove.bumpY = psDroid->y;
		psDroid->sMove.bumpDir = (SWORD)psDroid->direction;
	}

	if (tx != ntx && ty != nty)
	{
		// moved diagonally

		// figure out where the other two possible blocking tiles are
		horizX = mx < 0 ? ntx + 1 : ntx - 1;
		horizY = nty;

		vertX = ntx;
		vertY = my < 0 ? nty + 1 : nty - 1;

		if (fpathBlockingTile(horizX,horizY) && fpathBlockingTile(vertX,vertY))
		{
			// in a corner - choose an arbitrary slide
			if (ONEINTWO)
			{
				*pmx = MAKEFRACT(0);
				*pmy = -*pmy;
				NOTE_STATE(1);
			}
			else
			{
				*pmx = -*pmx;
				*pmy = MAKEFRACT(0);
				NOTE_STATE(2);
			}
		}
		else if (fpathBlockingTile(horizX,horizY))
		{
			*pmy = MAKEFRACT(0);
			NOTE_STATE(3);
		}
		else if (fpathBlockingTile(vertX,vertY))
		{
			*pmx = MAKEFRACT(0);
			NOTE_STATE(4);
		}
		else
		{
			moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
			NOTE_SLIDE;
			NOTE_STATE(5);
		}

//		*pmx = MAKEFRACT(0);
//		*pmy = MAKEFRACT(0);
	}
	else if (tx != ntx)
	{
		// moved horizontally - see which half of the tile were in
		if ((psDroid->y & TILE_MASK) > TILE_UNITS/2)
		{
			// top half
			if (fpathBlockingTile(ntx,nty+1))
			{
				*pmx = MAKEFRACT(0);
				NOTE_STATE(6);
			}
			else
			{
				moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
				NOTE_SLIDE;
				NOTE_STATE(7);
			}
		}
		else
		{
			// bottom half
			if (fpathBlockingTile(ntx,nty-1))
			{
				*pmx = MAKEFRACT(0);
				NOTE_STATE(8);
			}
			else
			{
				moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
				NOTE_SLIDE;
				NOTE_STATE(9);
			}
		}
	}
	else if (ty != nty)
	{
		// moved vertically
		if ((psDroid->x & TILE_MASK) > TILE_UNITS/2)
		{
			// top half
			if (fpathBlockingTile(ntx+1,nty))
			{
				*pmy = MAKEFRACT(0);
				NOTE_STATE(10);
			}
			else
			{
				moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
				NOTE_SLIDE;
				NOTE_STATE(11);
			}
		}
		else
		{
			// bottom half
			if (fpathBlockingTile(ntx-1,nty))
			{
				*pmy = MAKEFRACT(0);
				NOTE_STATE(12);
			}
			else
			{
				moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
				NOTE_SLIDE;
				NOTE_STATE(13);
			}
		}
	}
	else // if (tx == ntx && ty == nty)
	{
		// on a blocking tile - see if we need to jump off

		intx = MAKEINT(psDroid->sMove.fx) & TILE_MASK;
		inty = MAKEINT(psDroid->sMove.fy) & TILE_MASK;
		jumpx = (SDWORD)psDroid->x;
		jumpy = (SDWORD)psDroid->y;
		bJumped = FALSE;

/*		jumpx = MAKEINT(nx - mx);
		jumpy = MAKEINT(ny - my);
		intx = jumpx & TILE_MASK;
		inty = jumpy & TILE_MASK;
		bJumped = FALSE;*/

		if (intx < TILE_UNITS/2)
		{
			if (inty < TILE_UNITS/2)
			{
				// top left
				if ((mx < 0) && fpathBlockingTile(tx-1, ty))
				{
					bJumped = TRUE;
					jumpy = (jumpy & ~TILE_MASK) -1;
				}
				if ((my < 0) && fpathBlockingTile(tx, ty-1))
				{
					bJumped = TRUE;
					jumpx = (jumpx & ~TILE_MASK) -1;
				}
				NOTE_STATE(14);
			}
			else
			{
				// bottom left
				if ((mx < 0) && fpathBlockingTile(tx-1, ty))
				{
					bJumped = TRUE;
					jumpy = (jumpy & ~TILE_MASK) + TILE_UNITS;
				}
				if ((my >= 0) && fpathBlockingTile(tx, ty+1))
				{
					bJumped = TRUE;
					jumpx = (jumpx & ~TILE_MASK) -1;
				}
				NOTE_STATE(15);
			}
		}
		else
		{
			if (inty < TILE_UNITS/2)
			{
				// top right
				if ((mx >= 0) && fpathBlockingTile(tx+1, ty))
				{
					bJumped = TRUE;
					jumpy = (jumpy & ~TILE_MASK) -1;
				}
				if ((my < 0) && fpathBlockingTile(tx, ty-1))
				{
					bJumped = TRUE;
					jumpx = (jumpx & ~TILE_MASK) + TILE_UNITS;
				}
				NOTE_STATE(16);
			}
			else
			{
				// bottom right
				if ((mx >= 0) && fpathBlockingTile(tx+1, ty))
				{
					bJumped = TRUE;
					jumpy = (jumpy & ~TILE_MASK) + TILE_UNITS;
				}
				if ((my >= 0) && fpathBlockingTile(tx, ty+1))
				{
					bJumped = TRUE;
					jumpx = (jumpx & ~TILE_MASK) + TILE_UNITS;
				}
				NOTE_STATE(17);
			}
		}

		if (bJumped)
		{
			psDroid->x = (SWORD)(jumpx - MKI(radx));
			psDroid->y = (SWORD)(jumpy - MKI(rady));
			psDroid->sMove.fx = MAKEFRACT(jumpx);
			psDroid->sMove.fy = MAKEFRACT(jumpy);
			*pmx = MAKEFRACT(0);
			*pmy = MAKEFRACT(0);
		}
		else
		{
			moveCalcSlideVector(psDroid, blkCX,blkCY, pmx,pmy);
			NOTE_SLIDE;
		}
	}


	slideDir = MKI(vectorToAngle(*pmx,*pmy));
	if (ntx != tx)
	{
		// hit a horizontal block
		if ((tarDir < 90 || tarDir > 270) &&
			(slideDir >= 90 && slideDir <= 270))
		{
			slideDir = tarDir;
		}
		else if ((tarDir >= 90 && tarDir <= 270) &&
				 (slideDir < 90 || slideDir > 270))
		{
			slideDir = tarDir;
		}
	}
	if (nty != ty)
	{
		// hit a vertical block
		if ((tarDir < 180) &&
			(slideDir >= 180))
		{
			slideDir = tarDir;
		}
		else if ((tarDir >= 180) &&
				 (slideDir < 180))
		{
			slideDir = tarDir;
		}
	}
	*pSlideDir = slideDir;

#ifdef DEBUG
	nx = psDroid->sMove.fx + *pmx;
	ny = psDroid->sMove.fy + *pmy;

//	ASSERT( slide || (!fpathBlockingTile(MAKEINT(nx)>>TILE_SHIFT, MAKEINT(ny)>>TILE_SHIFT)),
//		"moveCalcBlockingSlide: slid onto a blocking tile" );
#endif

}


// see if a droid has run into another droid
// Only consider stationery droids
static void moveCalcDroidSlide(DROID *psDroid, float *pmx, float *pmy)
{
	SDWORD		i, droidR, rad, radSq;
	SDWORD		objR;
	SDWORD		xdiff,ydiff, distSq;
	NAYBOR_INFO	*psInfo;
	BASE_OBJECT	*psObst;
	BOOL		bLegs;

	bLegs = FALSE;
	if (psDroid->droidType == DROID_PERSON ||
		//psDroid->droidType == DROID_CYBORG)
        cyborgDroid(psDroid))
	{
		bLegs = TRUE;
	}

	droidR = moveObjRadius((BASE_OBJECT *)psDroid);
	psObst = NULL;
	for(i=0; i<(SDWORD)numNaybors; i++)
	{
		psInfo = asDroidNaybors + i;
		if (psInfo->psObj->type == OBJ_DROID)
		{
			if ( ((DROID *)psInfo->psObj)->droidType == DROID_TRANSPORTER )
			{
				// ignore transporters
				continue;
			}

			if (bLegs &&
				((DROID *)psInfo->psObj)->droidType != DROID_PERSON &&
				//((DROID *)psInfo->psObj)->droidType != DROID_CYBORG)
                !cyborgDroid((DROID *)psInfo->psObj))
			{
				// cyborgs/people only avoid other cyborgs/people
				continue;
			}
			if (!bLegs &&
				(((DROID *)psInfo->psObj)->droidType == DROID_PERSON))
			{
				// everything else doesn't avoid people
				continue;
			}
		}
		else
		{
			// ignore anything that isn't a droid
			continue;
		}

		objR = moveObjRadius(psInfo->psObj);
		rad = droidR + objR;
		radSq = rad*rad;

		xdiff = MAKEINT(psDroid->sMove.fx + *pmx) - (SDWORD)psInfo->psObj->x;
		ydiff = MAKEINT(psDroid->sMove.fy + *pmy) - (SDWORD)psInfo->psObj->y;
		distSq = xdiff*xdiff + ydiff*ydiff;
		if (Fmul(MKF(xdiff),(*pmx)) + Fmul(MKF(ydiff),(*pmy)) >= 0)
		{
			// object behind
			continue;
		}

		if (radSq > distSq)
		{
			if (psObst != NULL || !aiCheckAlliances(psInfo->psObj->player, psDroid->player))
			{
				// hit more than one droid - stop
				*pmx = (float)0;
				*pmy = (float)0;
				psObst = NULL;
				break;
			}
			else
			{
//				if (((DROID *)psInfo->psObj)->sMove.Status == MOVEINACTIVE)
				psObst = psInfo->psObj;

				// note the bump time and position if necessary
				if (psDroid->sMove.bumpTime == 0)
				{
					psDroid->sMove.bumpTime = gameTime;
					psDroid->sMove.lastBump = 0;
					psDroid->sMove.pauseTime = 0;
					psDroid->sMove.bumpX = psDroid->x;
					psDroid->sMove.bumpY = psDroid->y;
					psDroid->sMove.bumpDir = (SWORD)psDroid->direction;
				}
				else
				{
					psDroid->sMove.lastBump = (UWORD)(gameTime - psDroid->sMove.bumpTime);
				}

				// tell inactive droids to get out the way
				if ((psObst->type == OBJ_DROID) &&
					aiCheckAlliances(psObst->player, psDroid->player) &&
					((((DROID *)psObst)->sMove.Status == MOVEINACTIVE) ||
					 (((DROID *)psObst)->sMove.Status == MOVEROUTE)) )
				{
					if (psDroid->sMove.Status == MOVESHUFFLE)
					{
						moveShuffleDroid( (DROID *)psObst, psDroid->sMove.shuffleStart,
							psDroid->sMove.targetX - (SDWORD)psDroid->x,
							psDroid->sMove.targetY - (SDWORD)psDroid->y);
					}
					else
					{
						moveShuffleDroid( (DROID *)psObst, gameTime,
							psDroid->sMove.targetX - (SDWORD)psDroid->x,
							psDroid->sMove.targetY - (SDWORD)psDroid->y);
					}
				}
			}
		}
		else if (psInfo->distSqr > OBJ_MAXRADIUS*OBJ_MAXRADIUS)
		{
			// object is too far away to be hit
			break;
		}
	}

	if (psObst != NULL)
	{
		// Try to slide round it
		moveCalcSlideVector(psDroid, (SDWORD)psObst->x,(SDWORD)psObst->y, pmx,pmy);
	}
}

/* arrow colours */
#define	YELLOWARROW		117
#define	GREENARROW		253
#define	WHITEARROW		255
#define REDARROW		179

// get an obstacle avoidance vector
static void moveGetObstVector4(DROID *psDroid, float *pX, float *pY)
{
	SDWORD				i,xdiff,ydiff, absx,absy, dist;
	BASE_OBJECT			*psObj;
	SDWORD				numObst, distTot;
	float				dirX,dirY;
	float				omag, ox,oy, ratio;
	float				avoidX,avoidY;
	SDWORD				mapX,mapY, tx,ty, td;
	PROPULSION_STATS	*psPropStats;

	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	ASSERT( psPropStats != NULL,
			"moveUpdateUnit: invalid propulsion stats pointer" );

	numObst = 0;
	dirX = MKF(0);
	dirY = MKF(0);
	distTot = 0;

	droidGetNaybors(psDroid);

	// scan the neighbours for obstacles
	for(i=0; i<(SDWORD)numNaybors; i++)
	{
		psObj = asDroidNaybors[i].psObj;
		if (psObj->type != OBJ_DROID ||
			asDroidNaybors[i].distSqr >= AVOID_DIST*AVOID_DIST)
		{
			// object too far away to worry about
			continue;
		}

		// vtol droids only avoid each other and don't affect ground droids
		if ( (vtolDroid(psDroid) && (psObj->type != OBJ_DROID || !vtolDroid((DROID *)psObj))) ||
			 (!vtolDroid(psDroid) && psObj->type == OBJ_DROID && vtolDroid((DROID *)psObj)) )
		{
			continue;
		}

		if (((DROID *)psObj)->droidType == DROID_TRANSPORTER ||
			(((DROID *)psObj)->droidType == DROID_PERSON &&
			 psObj->player != psDroid->player))
		{
			// don't avoid people on the other side - run over them
			continue;
		}
		xdiff = (SDWORD)psObj->x - (SDWORD)psDroid->x;
		ydiff = (SDWORD)psObj->y - (SDWORD)psDroid->y;
		if (Fmul(MKF(xdiff),(*pX)) + Fmul(MKF(ydiff),(*pY)) < 0)
		{
			// object behind
			continue;
		}

		absx = labs(xdiff);
		absy = labs(ydiff);
		dist = absx > absy ? absx + absy/2 : absx/2 + absy;

		if (dist != 0)
		{
			dirX += Fdiv(MKF(xdiff),MKF(dist*dist));
			dirY += Fdiv(MKF(ydiff),MKF(dist*dist));
			distTot += dist*dist;
			numObst += 1;
		}
		else
		{
			dirX += MKF(xdiff);
			dirY += MKF(ydiff);
			numObst += 1;
		}
	}

	// now scan for blocking tiles
	mapX = (SDWORD)(psDroid->x >> TILE_SHIFT);
	mapY = (SDWORD)(psDroid->y >> TILE_SHIFT);
	for(ydiff=-2; ydiff<=2; ydiff++)
	{
		for(xdiff=-2; xdiff<=2; xdiff++)
		{
			if (Fmul(MKF(xdiff),(*pX)) + Fmul(MKF(ydiff),(*pY)) <= 0)
			{
				// object behind
				continue;
			}
			if (fpathBlockingTile(mapX+xdiff, mapY+ydiff))
			{
				tx = xdiff << TILE_SHIFT;
				ty = ydiff << TILE_SHIFT;
				td = tx*tx + ty*ty;
				if (td < AVOID_DIST*AVOID_DIST)
				{
					absx = labs(tx);
					absy = labs(ty);
					dist = absx > absy ? absx + absy/2 : absx/2 + absy;

					if (dist != 0)
					{
						dirX += Fdiv(MKF(tx),MKF(dist*dist));
						dirY += Fdiv(MKF(ty),MKF(dist*dist));
						distTot += dist*dist;
						numObst += 1;
					}
				}
			}
		}
	}

	if (numObst > 0)
	{
#ifdef ARROWS
		static BOOL bTest = TRUE;
#endif

		distTot /= numObst;

		// Create the avoid vector
		if (dirX == MKF(0) && dirY == MKF(0))
		{
			avoidX = MKF(0);
			avoidY = MKF(0);
			distTot = AVOID_DIST*AVOID_DIST;
		}
		else
		{
			omag = sqrtf(dirX*dirX + dirY*dirY);
			ox = dirX / omag;
			oy = dirY / omag;
			if (FRACTmul((*pX), oy) + FRACTmul((*pY),-ox) < 0)
			{
// 				debug( LOG_NEVER, "First perp\n");
				avoidX = -oy;
				avoidY = ox;
			}
			else
			{
// 				debug( LOG_NEVER, "Second perp\n");
				avoidX = oy;
				avoidY = -ox;
			}
		}

		// combine the avoid vector and the target vector
		ratio = Fdiv(MKF(distTot), MKF(AVOID_DIST*AVOID_DIST));
		if (ratio > MKF(1))
		{
			ratio = MKF(1);
		}

		*pX = Fmul((*pX), ratio) + Fmul(avoidX, (1 - ratio));
		*pY = Fmul((*pY), ratio) + Fmul(avoidY, (1 - ratio));

#ifdef ARROWS
		if ( bTest && psDroid->selected)
		{
			SDWORD	iHeadX, iHeadY, iHeadZ;

			/* target direction - yellow */
			iHeadX = psDroid->sMove.targetX;
			iHeadY = psDroid->sMove.targetY;
			iHeadZ = map_Height( iHeadX, iHeadY );
			arrowAdd( psDroid->x, psDroid->y, psDroid->z,
						iHeadX, iHeadY, iHeadZ, YELLOWARROW );

			/* average obstacle vector - green */
			iHeadX = MAKEINT(FRACTmul(ox, 200)) + psDroid->x;
			iHeadY = MAKEINT(FRACTmul(oy, 200)) + psDroid->y;
			arrowAdd( psDroid->x, psDroid->y, psDroid->z,
						iHeadX, iHeadY, iHeadZ, GREENARROW );

			/* normal - green */
			iHeadX = MAKEINT(FRACTmul(avoidX, 100)) + psDroid->x;
			iHeadY = MAKEINT(FRACTmul(avoidY, 100)) + psDroid->y;
			arrowAdd( psDroid->x, psDroid->y, psDroid->z,
						iHeadX, iHeadY, iHeadZ, GREENARROW );

			/* resultant - white */
			iHeadX = MAKEINT(FRACTmul((*pX), 200)) + psDroid->x;
			iHeadY = MAKEINT(FRACTmul((*pY), 200)) + psDroid->y;
			arrowAdd( psDroid->x, psDroid->y, psDroid->z,
						iHeadX, iHeadY, iHeadZ, WHITEARROW );
		}
#endif

	}
}

/* Get a direction for a droid to avoid obstacles etc. */
// This routine smells ...
static void moveGetDirection(DROID *psDroid, float *pX, float *pY)
{
	SDWORD	dx,dy, tx,ty;
	SDWORD	mag;
	float	root;
	BOOL	bNoVector;

	SDWORD	ndx,ndy, ntx,nty, nmag;
	float	nroot;


	tx = psDroid->sMove.targetX;
	ty = psDroid->sMove.targetY;

	// Calc the basic vector
	dx = tx - (SDWORD)psDroid->x;
	dy = ty - (SDWORD)psDroid->y;
	// If the droid is getting close to the way point start to phase in the next target
	mag = dx*dx + dy*dy;

	bNoVector = TRUE;

	// fade in the next target point if we arn't at the end of the waypoints
	if ((psDroid->sMove.Position != psDroid->sMove.numPoints) &&
		(mag < WAYPOINT_DSQ))
	{
		// find the next target
		movePeekNextTarget(psDroid, &ntx, &nty);
		ndx = ntx - (SDWORD)psDroid->x;
		ndy = nty - (SDWORD)psDroid->y;
		nmag = ndx*ndx + ndy*ndy;

		if (mag != 0 && nmag != 0)
		{
			// Get the size of the vectors
			root = sqrtf(MAKEFRACT(mag));
			nroot = sqrtf(MAKEFRACT(nmag));

			// Split the proportion of the vectors based on how close to the point they are
			ndx = (ndx * (WAYPOINT_DSQ - mag)) / WAYPOINT_DSQ;
			ndy = (ndy * (WAYPOINT_DSQ - mag)) / WAYPOINT_DSQ;

			dx = (dx * mag) / WAYPOINT_DSQ;
			dy = (dy * mag) / WAYPOINT_DSQ;

			// Calculate the normalised result
			*pX = FRACTdiv(MKF(dx), root) + FRACTdiv(MKF(ndx), nroot);
			*pY = FRACTdiv(MKF(dy), root) + FRACTdiv(MKF(ndy), nroot);
			bNoVector = FALSE;
		}
	}

	if (bNoVector)

	{

		root = sqrtf(MAKEFRACT(mag));
		*pX = FRACTdiv(MKF(dx), root);
		*pY = FRACTdiv(MKF(dy), root);



	}


	if ( psDroid->droidType != DROID_TRANSPORTER )
	{
		moveGetObstVector4(psDroid, pX,pY);
	}


}


// Calculate the boundary vector
void moveCalcBoundary(DROID *psDroid)
{
	SDWORD	absX, absY;
	SDWORD	prevX,prevY, prevMag;
	SDWORD	nTarX,nTarY, nextX,nextY,nextMag;
	SDWORD	sumX,sumY;

	// No points left - simple case for the bound vector
	if (psDroid->sMove.Position == psDroid->sMove.numPoints)
	{
		psDroid->sMove.boundX = (SWORD)(psDroid->sMove.srcX - psDroid->sMove.targetX);
		psDroid->sMove.boundY = (SWORD)(psDroid->sMove.srcY - psDroid->sMove.targetY);
		return;
	}

	// Calculate the vector back along the current path
	prevX = psDroid->sMove.srcX - psDroid->sMove.targetX;
	prevY = psDroid->sMove.srcY - psDroid->sMove.targetY;
	absX = labs(prevX);
	absY = labs(prevY);
	prevMag = absX > absY ? absX + absY/2 : absY + absX/2;
//	prevMag = sqrt(prevX*prevX + prevY*prevY);

	// Calculate the vector to the next target
	movePeekNextTarget(psDroid, &nTarX, &nTarY);
	nextX = nTarX - psDroid->sMove.targetX;
	nextY = nTarY - psDroid->sMove.targetY;
	absX = labs(nextX);
	absY = labs(nextY);
	nextMag = absX > absY ? absX + absY/2 : absY + absX/2;
//	nextMag = sqrt(nextX*nextX + nextY*nextY);

	if (prevMag != 0 && nextMag == 0)
	{
		// don't bother mixing the boundaries - cos there isn't a next vector anyway
		psDroid->sMove.boundX = (SWORD)(psDroid->sMove.srcX - psDroid->sMove.targetX);
		psDroid->sMove.boundY = (SWORD)(psDroid->sMove.srcY - psDroid->sMove.targetY);
		return;
	}
	else if (prevMag == 0 || nextMag == 0)
	{
		psDroid->sMove.boundX = 0;
		psDroid->sMove.boundY = 0;
		return;
	}

	// Calculate the vector between the two
	sumX = (prevX * BOUND_ACC)/prevMag + (nextX * BOUND_ACC)/nextMag;
	sumY = (prevY * BOUND_ACC)/prevMag + (nextY * BOUND_ACC)/nextMag;

	// Rotate by 90 degrees one way and see if it is the same side as the src vector
	// if not rotate 90 the other.
	if (prevX*sumY - prevY*sumX < 0)
	{
		psDroid->sMove.boundX = (SWORD)-sumY;
		psDroid->sMove.boundY = (SWORD)sumX;
	}
	else
	{
		psDroid->sMove.boundX = (SWORD)sumY;
		psDroid->sMove.boundY = (SWORD)-sumX;
	}

	debug( LOG_NEVER, "new boundary: droid %d boundary (%d,%d)\n",
			psDroid->id, psDroid->sMove.boundX, psDroid->sMove.boundY);
}


// Check if a droid has got to a way point
static BOOL moveReachedWayPoint(DROID *psDroid)
{
	SDWORD	droidX,droidY, iRange;

	// Calculate the vector to the droid
	droidX = (SDWORD)psDroid->x - psDroid->sMove.targetX;
	droidY = (SDWORD)psDroid->y - psDroid->sMove.targetY;

	// see if this is a formation end point
	if (psDroid->droidType == DROID_TRANSPORTER ||
		(psDroid->sMove.psFormation &&
		 formationMember(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid)) ||
		 (vtolDroid(psDroid) && (psDroid->sMove.numPoints == psDroid->sMove.Position)) )
//							 && (psDroid->action != DACTION_VTOLATTACK)) )
	{
		if ( psDroid->droidType == DROID_TRANSPORTER )
		{

			iRange = TILE_UNITS/4;

		}
		else
		{
			iRange = TILE_UNITS/4;
		}

		if (droidX*droidX + droidY*droidY < iRange*iRange)
		{
			return TRUE;
		}
	}
	else
	{
		// if the dot product is -ve the droid has got past the way point
		// but only move onto the next way point if we can see the previous one
		// (this helps units that have got nudged off course).
		if ((psDroid->sMove.boundX * droidX + psDroid->sMove.boundY * droidY <= 0) &&
			fpathTileLOS((SDWORD)psDroid->x >> TILE_SHIFT, (SDWORD)psDroid->y >> TILE_SHIFT, psDroid->sMove.targetX >> TILE_SHIFT, psDroid->sMove.targetY >> TILE_SHIFT))
		{
//		DBPRINTF(("Waypoint %d\n", psDroid->sMove.Position));
			debug( LOG_MOVEMENT, "Next waypoint: droid %d bound (%d,%d) target (%d,%d)\n",
					psDroid->id, psDroid->sMove.boundX,psDroid->sMove.boundY,
					droidX,droidY);

			return TRUE;
		}
	}

	return FALSE;
}

void moveToggleFormationSpeedLimiting( void )
{
	g_bFormationSpeedLimitingOn = !g_bFormationSpeedLimitingOn;
}

void moveSetFormationSpeedLimiting( BOOL bVal )
{
	g_bFormationSpeedLimitingOn = bVal;
}

BOOL moveFormationSpeedLimitingOn( void )
{
	return g_bFormationSpeedLimitingOn;
}

#define MAX_SPEED_PITCH  60

// Calculate the new speed for a droid
SDWORD moveCalcDroidSpeed(DROID *psDroid)
{
	UDWORD			mapX,mapY, damLevel;//, tarSpeed;
	SDWORD			speed, pitch;
	WEAPON_STATS	*psWStats;

	mapX = psDroid->x >> TILE_SHIFT;
	mapY = psDroid->y >> TILE_SHIFT;
	speed = (SDWORD) calcDroidSpeed(psDroid->baseSpeed, TERRAIN_TYPE(mapTile(mapX,mapY)),
							  psDroid->asBits[COMP_PROPULSION].nStat);

/*	if ( vtolDroid(psDroid) &&
		 ((asBodyStats + psDroid->asBits[COMP_BODY].nStat)->size == SIZE_HEAVY) )
	{
		speed /= 2;
	}*/

	pitch = psDroid->pitch;
	if (pitch > MAX_SPEED_PITCH)
	{
		pitch = MAX_SPEED_PITCH;
	}
	else if (pitch < - MAX_SPEED_PITCH)
	{
		pitch = -MAX_SPEED_PITCH;
	}
	// now offset the speed for the slope of the droid
	speed = (MAX_SPEED_PITCH - pitch) * speed / MAX_SPEED_PITCH;

	// slow down damaged droids
	damLevel = PERCENT(psDroid->body, psDroid->originalBody);
	if (damLevel < HEAVY_DAMAGE_LEVEL)
	{
		speed = 2 * speed / 3;
	}

	// stop droids that have just fired a no fire while moving weapon
	//if (psDroid->numWeaps > 0 && psDroid->asWeaps[0].lastFired + FOM_MOVEPAUSE > gameTime)
	//Watermelon:I dont think droid with no weapon should do this check...
	if (psDroid->numWeaps > 0)
	{
		if (psDroid->asWeaps[0].nStat > 0 && psDroid->asWeaps[0].lastFired + FOM_MOVEPAUSE > gameTime)
		{
			psWStats = asWeaponStats + psDroid->asWeaps[0].nStat;
			if (psWStats->fireOnMove == FOM_NO)
			{
				speed = 0;
			}
		}
	}

	/* adjust speed for formation */
	if(!vtolDroid(psDroid) &&
		moveFormationSpeedLimitingOn() && psDroid->sMove.psFormation)
	{
		SDWORD FrmSpeed = (SDWORD)psDroid->sMove.psFormation->iSpeed;

		if ( speed > FrmSpeed )
		{
			speed = FrmSpeed;
		}
	}

	// slow down shuffling VTOLs
	if (vtolDroid(psDroid) &&
		(psDroid->sMove.Status == MOVESHUFFLE) &&
		(speed > MIN_END_SPEED))
	{
		speed = MIN_END_SPEED;
	}

//	/* adjust speed for formation */
//	if ( moveFormationSpeedLimitingOn() &&
//		 psDroid->sMove.psFormation &&
//		 speed > (SDWORD)psDroid->sMove.psFormation->iSpeed )
//	{
//		speed = psDroid->sMove.psFormation->iSpeed;
//	}

//#if(1)
//	if(psDroid->selected) {
//		printf("%d : %d : %d\n",driveGetSpeed(),psDroid->baseSpeed,speed);
//	}
//#endif

	return speed;
}

static BOOL moveDroidStopped( DROID *psDroid, SDWORD speed )
{
	if ((psDroid->sMove.Status == MOVEINACTIVE || psDroid->sMove.Status == MOVEROUTE) &&
		speed == 0 && psDroid->sMove.speed == MKF(0))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static void moveUpdateDroidDirection( DROID *psDroid, SDWORD *pSpeed, SDWORD direction,
		SDWORD iSpinAngle, SDWORD iSpinSpeed, SDWORD iTurnSpeed, float *pDroidDir,
		float *pfSpeed ) // direction is target-direction
{
	float		adiff;
	float		temp;

	*pfSpeed = MKF(*pSpeed);
	*pDroidDir = psDroid->direction;

	// don't move if in MOVEPAUSE state
	if (psDroid->sMove.Status == MOVEPAUSE)
	{
		return;
	}

	temp = *pDroidDir;
	adiff = fabsf(direction - *pDroidDir);
	if (adiff > TRIG_DEGREES/2)
	{
		adiff = TRIG_DEGREES - adiff;
	}
	if (adiff > iSpinAngle)
	{
		// large change in direction, spin on the spot
// 		debug( LOG_NEVER, "Spin ");
		moveCalcTurn(&temp, MKF(direction), iSpinSpeed);
		*pSpeed = 0;
	}
	else
	{
		// small change in direction, turn while moving
// 		debug( LOG_NEVER, "Curve ");
		moveCalcTurn(&temp, MKF(direction), iTurnSpeed);
	}

	*pDroidDir = temp;
}


// Calculate current speed perpendicular to droids direction
static float moveCalcPerpSpeed( DROID *psDroid, float iDroidDir, SDWORD iSkidDecel )
{
	float		adiff;
	float		perpSpeed;

	adiff = fabsf(iDroidDir - psDroid->sMove.moveDir);
	perpSpeed = psDroid->sMove.speed * trigSin(adiff);

	// decelerate the perpendicular speed
	perpSpeed -= (iSkidDecel * baseSpeed);
	if (perpSpeed < MKF(0))
	{
		perpSpeed = MKF(0);
	}

	return perpSpeed;
}


static void moveCombineNormalAndPerpSpeeds( DROID *psDroid, float fNormalSpeed,
		float fPerpSpeed, float iDroidDir )
{
	float		finalDir, adiff;
	float		finalSpeed;

	/* set current direction */
	psDroid->direction = iDroidDir;

	/* set normal speed and direction if perpendicular speed is zero */
	if (fPerpSpeed == MKF(0))
	{
		psDroid->sMove.speed = fNormalSpeed;
		psDroid->sMove.moveDir = iDroidDir;
		return;
	}

	finalSpeed = sqrtf(Fmul(fNormalSpeed,fNormalSpeed) + Fmul(fPerpSpeed,fPerpSpeed));

	// calculate the angle between the droid facing and movement direction
	finalDir = trigInvCos(fNormalSpeed / finalSpeed);

	// choose the finalDir on the same side as the old movement direction
	adiff = fabsf(iDroidDir - psDroid->sMove.moveDir);
	if (adiff < TRIG_DEGREES/2)
	{
		if (iDroidDir > psDroid->sMove.moveDir)
		{
			finalDir = iDroidDir - finalDir;
		}
		else
		{
			finalDir = iDroidDir + finalDir;
		}
	}
	else
	{
		if (iDroidDir > psDroid->sMove.moveDir)
		{
			finalDir = iDroidDir + finalDir;
			if (finalDir >= TRIG_DEGREES)
			{
				finalDir -= TRIG_DEGREES;
			}
		}
		else
		{
			finalDir = iDroidDir - finalDir;
			if (finalDir < 0)
			{
				finalDir += TRIG_DEGREES;
			}
		}
	}

	psDroid->sMove.moveDir = finalDir;
	psDroid->sMove.speed = finalSpeed;
}


// Calculate the current speed in the droids normal direction
static float moveCalcNormalSpeed( DROID *psDroid, float fSpeed, float iDroidDir,
		SDWORD iAccel, SDWORD iDecel )
{
	float		adiff;
	float		normalSpeed;

	adiff = fabsf(iDroidDir - psDroid->sMove.moveDir);
	normalSpeed = psDroid->sMove.speed * trigCos(adiff);

	if (normalSpeed < fSpeed)
	{
		// accelerate
		normalSpeed += (iAccel * baseSpeed);
		if (normalSpeed > fSpeed)
		{
			normalSpeed = fSpeed;
		}
	}
	else
	{
		// decelerate
		normalSpeed -= (iDecel * baseSpeed);
		if (normalSpeed < fSpeed)
		{
			normalSpeed = fSpeed;
		}
	}

	return normalSpeed;
}


static void moveGetDroidPosDiffs( DROID *psDroid, float *pDX, float *pDY )
{
	float	move;


	move = Fmul(psDroid->sMove.speed, baseSpeed);


	*pDX = move * trigSin(psDroid->sMove.moveDir);
	*pDY = move * trigCos(psDroid->sMove.moveDir);
}

// see if the droid is close to the final way point
static void moveCheckFinalWaypoint( DROID *psDroid, SDWORD *pSpeed )
{
	SDWORD		xdiff,ydiff, distSq;
	SDWORD		minEndSpeed = psDroid->baseSpeed/3;

	if (minEndSpeed > MIN_END_SPEED)
	{
		minEndSpeed = MIN_END_SPEED;
	}



	// don't do this for VTOLs doing attack runs
	if (vtolDroid(psDroid) && (psDroid->action == DACTION_VTOLATTACK))
	{
		return;
	}

	if (*pSpeed > minEndSpeed &&
		(psDroid->sMove.Status != MOVESHUFFLE) &&
		psDroid->sMove.Position == psDroid->sMove.numPoints)
	{
		xdiff = (SDWORD)psDroid->x - psDroid->sMove.targetX;
		ydiff = (SDWORD)psDroid->y - psDroid->sMove.targetY;
		distSq = xdiff*xdiff + ydiff*ydiff;
		if (distSq < END_SPEED_RANGE*END_SPEED_RANGE)
		{
			*pSpeed = (MAX_END_SPEED-minEndSpeed) * distSq
						/ (END_SPEED_RANGE*END_SPEED_RANGE) + minEndSpeed;
		}
	}
}

static void moveUpdateDroidPos( DROID *psDroid, float dx, float dy )
{
	SDWORD	iX = 0, iY = 0;

	ASSERT( psDroid->x < (mapWidth << TILE_SHIFT),
		"moveUpdateDroidPos: droid is already off the map in the x direction" );
	ASSERT( psDroid->y < (mapHeight<< TILE_SHIFT),
		"moveUpdateDroidPos: droid is already off the map in the y direction" );

	if (psDroid->sMove.Status == MOVEPAUSE)
	{
		// don't actually move if the move is paused
		return;
	}

	psDroid->sMove.fx += dx;
	psDroid->sMove.fy += dy;
//	psDroid->sMove.dx = dx;
//	psDroid->sMove.dy = dy;

	iX = MAKEINT(psDroid->sMove.fx);
	iY = MAKEINT(psDroid->sMove.fy);

	/* impact if about to go off map else update coordinates */
	if ( worldOnMap( iX, iY ) == FALSE )
	{
		if ( psDroid->droidType == DROID_TRANSPORTER )
		{
			/* transporter going off-world - trigger next map */

		}
		else
		{
			/* dreadful last-ditch crash-avoiding hack - sort this! - GJ */
			debug( LOG_NEVER, "**** droid about to go off map - fixed ****\n" );
			destroyDroid( psDroid );
		}
	}
	else
	{
		psDroid->x = (UWORD)iX;
		psDroid->y = (UWORD)iY;
	}

	// lovely hack to keep transporters just on the map
	// two weeks to go and the hacks just get better !!!
	if (psDroid->droidType == DROID_TRANSPORTER)
	{
		if (psDroid->x == 0)
		{
			psDroid->x = 1;
		}
		if (psDroid->y == 0)
		{
			psDroid->y = 1;
		}
	}

	ASSERT( psDroid->x < (mapWidth << TILE_SHIFT),
		"moveUpdateDroidPos: droid just moved off the map in the x direction" );
	ASSERT( psDroid->y < (mapHeight<< TILE_SHIFT),
		"moveUpdateDroidPos: droid just moved off the map in the y direction" );

}

/* Update a tracked droids position and speed given target values */
static void moveUpdateGroundModel(DROID *psDroid, SDWORD speed, SDWORD direction)
{
	float				fPerpSpeed, fNormalSpeed, dx, dy, fSpeed, bx,by;
	float				iDroidDir;
	SDWORD				slideDir;
	PROPULSION_STATS	*psPropStats;
	SDWORD				spinSpeed, turnSpeed, skidDecel;
	// constants for the different propulsion types
	static SDWORD		hvrSkid = HOVER_SKID_DECEL;
	static SDWORD		whlSkid = WHEELED_SKID_DECEL;
	static SDWORD		trkSkid = TRACKED_SKID_DECEL;
	static float		hvrTurn = FRACTCONST(3,4);		//0.75f;
	static float		whlTurn = FRACTCONST(1,1);		//1.0f;
	static float		trkTurn = FRACTCONST(1,1);		//1.0f;

	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	switch (psPropStats->propulsionType)
	{
	case HOVER:
		spinSpeed = MAKEINT(psDroid->baseSpeed*hvrTurn);
		turnSpeed = MAKEINT(psDroid->baseSpeed/3*hvrTurn);
		skidDecel = hvrSkid;//HOVER_SKID_DECEL;
		break;
	case WHEELED:
		spinSpeed = MAKEINT(psDroid->baseSpeed*hvrTurn);
		turnSpeed = MAKEINT(psDroid->baseSpeed/3*whlTurn);
		skidDecel = whlSkid;//WHEELED_SKID_DECEL;
		break;
	case TRACKED:
	default:
		spinSpeed = MAKEINT(psDroid->baseSpeed*hvrTurn);
		turnSpeed = MAKEINT(psDroid->baseSpeed/3*trkTurn);
		skidDecel = trkSkid;//TRACKED_SKID_DECEL;
		break;
	}

	// nothing to do if the droid is stopped
	if ( moveDroidStopped( psDroid, speed ) == TRUE )
	{
		return;
	}

	// update the naybors list
	droidGetNaybors(psDroid);

#ifdef DEBUG_DRIVE_SPEED
if(psDroid == driveGetDriven())	debug( LOG_NEVER, "%d ", speed );
#endif

	moveCheckFinalWaypoint( psDroid, &speed );

#ifdef DEBUG_DRIVE_SPEED
if(psDroid == driveGetDriven())	debug( LOG_NEVER, "%d ", speed );
#endif

//	moveUpdateDroidDirection( psDroid, &speed, direction, TRACKED_SPIN_ANGLE,
//				TRACKED_SPIN_SPEED, TRACKED_TURN_SPEED, &iDroidDir, &fSpeed );
	moveUpdateDroidDirection( psDroid, &speed, direction, TRACKED_SPIN_ANGLE,
				spinSpeed, turnSpeed, &iDroidDir, &fSpeed );

#ifdef DEBUG_DRIVE_SPEED
if(psDroid == driveGetDriven())	debug( LOG_NEVER, "%d ", speed );
#endif

	fNormalSpeed = moveCalcNormalSpeed( psDroid, fSpeed, iDroidDir,
										TRACKED_ACCEL, TRACKED_DECEL );
	fPerpSpeed   = moveCalcPerpSpeed( psDroid, iDroidDir, skidDecel );

	moveCombineNormalAndPerpSpeeds( psDroid, fNormalSpeed,
										fPerpSpeed, iDroidDir );

#ifdef DEBUG_DRIVE_SPEED
if(psDroid == driveGetDriven())	debug( LOG_NEVER, "%d\n", speed );
#endif

//	if (psDroid->direction != psDroid->sMove.dir)
/*	if (fPerpSpeed > 0)
	{
		DBPRINTF(("droid %d direction %d total dir %d perpspeed %f\n",
			psDroid->id, psDroid->direction, psDroid->sMove.dir, fPerpSpeed));
	}*/

	moveGetDroidPosDiffs( psDroid, &dx, &dy );

	moveCheckSquished(psDroid, dx,dy);
	moveCalcDroidSlide(psDroid, &dx,&dy);
	bx = dx;
	by = dy;
	moveCalcBlockingSlide(psDroid, &bx,&by, direction, &slideDir);
	if (bx != dx || by != dy)
	{
		moveUpdateDroidDirection( psDroid, &speed, slideDir, TRACKED_SPIN_ANGLE,
					psDroid->baseSpeed, psDroid->baseSpeed/3, &iDroidDir, &fSpeed );
		psDroid->direction = iDroidDir;
	}

	moveUpdateDroidPos( psDroid, bx, by );

	//set the droid height here so other routines can use it
	psDroid->z = map_Height(psDroid->x, psDroid->y);//jps 21july96
	updateDroidOrientation(psDroid);
}

/* Update a persons position and speed given target values */
void moveUpdatePersonModel(DROID *psDroid, SDWORD speed, SDWORD direction)
{
	float			fPerpSpeed, fNormalSpeed, dx, dy, fSpeed;
	float			iDroidDir;
	SDWORD			slideDir;
//	BASE_OBJECT		*psObst;
	BOOL			bRet;

	// nothing to do if the droid is stopped
	if ( moveDroidStopped( psDroid, speed ) == TRUE )
	{
		if ( psDroid->droidType == DROID_PERSON &&
			 psDroid->order != DORDER_RUNBURN &&
			 (psDroid->action == DACTION_ATTACK ||
			  psDroid->action == DACTION_ROTATETOATTACK) )
		{
			/* remove previous anim */
			if ( psDroid->psCurAnim != NULL &&
				 psDroid->psCurAnim->psAnim->uwID != ID_ANIM_DROIDFIRE )
			{
				bRet = animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID );
				ASSERT( bRet == TRUE, "moveUpdatePersonModel: animObj_Remove failed" );
				psDroid->psCurAnim = NULL;
			}

			/* add firing anim */
			if ( psDroid->psCurAnim == NULL )
			{
				psDroid->psCurAnim = animObj_Add( psDroid, ID_ANIM_DROIDFIRE, 0, 0 );
			}
			else
			{
				psDroid->psCurAnim->bVisible = TRUE;
			}

			return;
		}

		/* don't show move animations if inactive */
		if ( psDroid->psCurAnim != NULL )
		{
//			DBPRINTF(("droid anim stopped %p\n",psDroid);
//DBPRINTF(("vis 1 off %p\n",psDroid);
			psDroid->psCurAnim->bVisible = FALSE;
		}
		return;
	}

	// update the naybors list
	droidGetNaybors(psDroid);

	moveUpdateDroidDirection( psDroid, &speed, direction, PERSON_SPIN_ANGLE,
				PERSON_SPIN_SPEED, PERSON_TURN_SPEED, &iDroidDir, &fSpeed );

	fNormalSpeed = moveCalcNormalSpeed( psDroid, fSpeed, iDroidDir,
										PERSON_ACCEL, PERSON_DECEL );
	/* people don't skid at the moment so set zero perpendicular speed */
	fPerpSpeed = MKF(0);

	moveCombineNormalAndPerpSpeeds( psDroid, fNormalSpeed,
										fPerpSpeed, iDroidDir );

	moveGetDroidPosDiffs( psDroid, &dx, &dy );

/*	if (moveFindObstacle(psDroid, dx,dy, &psObst))
	{
		moveCalcSlideVector(psDroid, (SDWORD)psObst->x, (SDWORD)psObst->y, &dx, &dy);
	}*/

	moveCalcDroidSlide(psDroid, &dx,&dy);
	moveCalcBlockingSlide(psDroid, &dx,&dy, direction, &slideDir);

	moveUpdateDroidPos( psDroid, dx, dy );

	//set the droid height here so other routines can use it
	psDroid->z = map_Height(psDroid->x, psDroid->y);//jps 21july96

	psDroid->sMove.fz = MAKEFRACT(psDroid->z);


	/* update anim if moving and not on fire */
	if ( psDroid->droidType == DROID_PERSON && speed != 0 &&
		 psDroid->order != DORDER_RUNBURN )
	{
		/* remove previous anim */
		if ( psDroid->psCurAnim != NULL &&
			 (psDroid->psCurAnim->psAnim->uwID != ID_ANIM_DROIDRUN ||
			  psDroid->psCurAnim->psAnim->uwID != ID_ANIM_DROIDRUN)   )
		{
			bRet = animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID );
			ASSERT( bRet == TRUE, "moveUpdatePersonModel: animObj_Remove failed" );
			psDroid->psCurAnim = NULL;
		}

		/* if no anim currently attached, get one */
		if ( psDroid->psCurAnim == NULL )
		{
			// Only add the animation if the droid is on screen, saves memory and time.
			if(clipXY(psDroid->x,psDroid->y)) {
				debug( LOG_NEVER, "Added person run anim\n" );
				psDroid->psCurAnim = animObj_Add( (BASE_OBJECT *) psDroid,
													ID_ANIM_DROIDRUN, 0, 0 );
			}
		} else {
			// If the droid went off screen then remove the animation, saves memory and time.
			if(!clipXY(psDroid->x,psDroid->y)) {
				bRet = animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID );
				ASSERT( bRet == TRUE, "moveUpdatePersonModel : animObj_Remove failed" );
				psDroid->psCurAnim = NULL;
				debug( LOG_NEVER, "Removed person run anim\n" );
			}
		}
	}

	/* show anim */
	if ( psDroid->psCurAnim != NULL )
	{
		psDroid->psCurAnim->bVisible = TRUE;
	}
}

//#define	VTOL_VERTICAL_SPEED		((SDWORD)psDroid->baseSpeed / 4)
#define	VTOL_VERTICAL_SPEED		((((SDWORD)psDroid->baseSpeed / 4) > 60) ? ((SDWORD)psDroid->baseSpeed / 4) : 60)

/* primitive 'bang-bang' vtol height controller */
static void moveAdjustVtolHeight( DROID * psDroid, UDWORD iMapHeight )
{
	UDWORD	iMinHeight, iMaxHeight, iLevelHeight;

	if ( psDroid->droidType == DROID_TRANSPORTER )
	{
		iMinHeight   = 2*VTOL_HEIGHT_MIN;
		iLevelHeight = 2*VTOL_HEIGHT_LEVEL;
		iMaxHeight   = 2*VTOL_HEIGHT_MAX;
	}
	else
	{
		iMinHeight   = VTOL_HEIGHT_MIN;
		iLevelHeight = VTOL_HEIGHT_LEVEL;
		iMaxHeight   = VTOL_HEIGHT_MAX;
	}

	if ( psDroid->z >= (iMapHeight+iMaxHeight) )
	{
		psDroid->sMove.iVertSpeed = (SWORD)-VTOL_VERTICAL_SPEED;
	}
	else if ( psDroid->z < (iMapHeight+iMinHeight) )
	{
		psDroid->sMove.iVertSpeed = (SWORD)VTOL_VERTICAL_SPEED;
	}
	else if ( (psDroid->z < iLevelHeight) &&
			  (psDroid->sMove.iVertSpeed < 0 )    )
	{
		psDroid->sMove.iVertSpeed = 0;
	}
	else if ( (psDroid->z > iLevelHeight) &&
			  (psDroid->sMove.iVertSpeed > 0 )    )
	{
		psDroid->sMove.iVertSpeed = 0;
	}
}

// set a vtol to be hovering in the air
void moveMakeVtolHover( DROID *psDroid )
{
	psDroid->sMove.Status = MOVEHOVER;
	psDroid->z = (UWORD)(map_Height(psDroid->x,psDroid->y) + VTOL_HEIGHT_LEVEL);
}

static void moveUpdateVtolModel(DROID *psDroid, SDWORD speed, SDWORD direction)
{
	float	fPerpSpeed, fNormalSpeed, dx, dy, fSpeed;
	float	iDroidDir;
	SDWORD	iMapZ, iRoll, slideDir, iSpinSpeed, iTurnSpeed;
	float	fDZ, fDroidZ, fMapZ;


	// nothing to do if the droid is stopped
	if ( moveDroidStopped(  psDroid, speed ) == TRUE )
	{
		return;
	}

	// update the naybors list
	droidGetNaybors(psDroid);

	moveCheckFinalWaypoint( psDroid, &speed );

	if ( psDroid->droidType == DROID_TRANSPORTER )
	{
		moveUpdateDroidDirection( psDroid, &speed, direction, VTOL_SPIN_ANGLE,
					VTOL_SPIN_SPEED, VTOL_TURN_SPEED, &iDroidDir, &fSpeed );
	}
	else
	{
		iSpinSpeed = (psDroid->baseSpeed/2 > VTOL_SPIN_SPEED) ? psDroid->baseSpeed/2 : VTOL_SPIN_SPEED;
		iTurnSpeed = (psDroid->baseSpeed/8 > VTOL_TURN_SPEED) ? psDroid->baseSpeed/8 : VTOL_TURN_SPEED;
		moveUpdateDroidDirection( psDroid, &speed, direction, VTOL_SPIN_ANGLE,
				iSpinSpeed, iTurnSpeed, &iDroidDir, &fSpeed );
	}

	fNormalSpeed = moveCalcNormalSpeed( psDroid, fSpeed, iDroidDir,
										VTOL_ACCEL, VTOL_DECEL );
	fPerpSpeed   = moveCalcPerpSpeed( psDroid, iDroidDir, VTOL_SKID_DECEL );

	moveCombineNormalAndPerpSpeeds( psDroid, fNormalSpeed,
										fPerpSpeed, iDroidDir );

	moveGetDroidPosDiffs( psDroid, &dx, &dy );

	/* set slide blocking tile for map edge */
	if ( psDroid->droidType != DROID_TRANSPORTER )
	{
		fpathBlockingTile = fpathLiftSlideBlockingTile;
		moveCalcBlockingSlide( psDroid, &dx, &dy, direction, &slideDir );
		fpathBlockingTile = fpathGroundBlockingTile;
	}

	moveUpdateDroidPos( psDroid, dx, dy );

	/* update vtol orientation */
	iRoll = (psDroid->sMove.moveDir - psDroid->direction) / 3;
	if ( iRoll < 0 )
	{
		iRoll += 360;
	}
	psDroid->roll = (UWORD) iRoll;

	iMapZ = map_Height(psDroid->x, psDroid->y);

	/* do vertical movement */

	fDZ = (float)(psDroid->sMove.iVertSpeed * (SDWORD)frameTime) / GAME_TICKS_PER_SEC;
	fDroidZ = psDroid->sMove.fz;
	fMapZ = (float) map_Height(psDroid->x, psDroid->y);
	if ( fDroidZ+fDZ < 0 )
	{
		psDroid->sMove.fz = 0;
	}
	else if ( fDroidZ+fDZ < fMapZ )
	{
		psDroid->sMove.fz = fMapZ;
	}
	else
	{
		psDroid->sMove.fz = psDroid->sMove.fz + fDZ;
	}
	psDroid->z = (UWORD)psDroid->sMove.fz;


	moveAdjustVtolHeight( psDroid, iMapZ );
}

#ifdef DEBUG

WZ_DECL_UNUSED static void moveGetStatusStr( UBYTE status, char *szStr )
{
	switch ( status )
	{
		case MOVEINACTIVE:
			strcpy( szStr, "MOVEINACTIVE" );
			break;
		case MOVENAVIGATE:
			strcpy( szStr, "MOVENAVIGATE" );
			break;
		case MOVETURN:
			strcpy( szStr, "MOVETURN" );
			break;
		case MOVEPAUSE:
			strcpy( szStr, "MOVEPAUSE" );
			break;
		case MOVEPOINTTOPOINT:
			strcpy( szStr, "MOVEPOINTTOPOINT" );
			break;
		case MOVETURNSTOP:
			strcpy( szStr, "MOVETURNSTOP" );
			break;
		case MOVETURNTOTARGET:
			strcpy( szStr, "MOVETURNTOTARGET" );
			break;
		case MOVEROUTE:
			strcpy( szStr, "MOVEROUTE" );
			break;
		case MOVEHOVER:
			strcpy( szStr, "MOVEHOVER" );
			break;
		case MOVEDRIVE:
			strcpy( szStr, "MOVEDRIVE" );
			break;
		case MOVEDRIVEFOLLOW:
			strcpy( szStr, "MOVEDRIVEFOLLOW" );
			break;
		default:
			strcpy( szStr, "" );
			break;
	}
}
#endif

#define CYBORG_VERTICAL_SPEED	((SDWORD)psDroid->baseSpeed/2)

static void
moveCyborgLaunchAnimDone( ANIM_OBJECT *psObj )
{
	DROID	*psDroid = (DROID*)psObj->psParent;

	ASSERT( psDroid != NULL,
			"moveCyborgLaunchAnimDone: invalid cyborg pointer" );

	/* raise cyborg a little bit so flying - terrible hack - GJ */
	psDroid->z++;
	psDroid->sMove.iVertSpeed = (SWORD)CYBORG_VERTICAL_SPEED;

	psDroid->psCurAnim = NULL;
}

static void
moveCyborgTouchDownAnimDone( ANIM_OBJECT *psObj )
{
	DROID	*psDroid = (DROID*)psObj->psParent;

	ASSERT( psDroid != NULL,
			"moveCyborgTouchDownAnimDone: invalid cyborg pointer" );

	psDroid->psCurAnim = NULL;
	psDroid->z = map_Height( psDroid->x, psDroid->y );
}


static void moveUpdateJumpCyborgModel(DROID *psDroid, SDWORD speed, SDWORD direction)
{
	float	fPerpSpeed, fNormalSpeed, dx, dy, fSpeed;
	float	iDroidDir;

	// nothing to do if the droid is stopped
	if ( moveDroidStopped(  psDroid, speed ) == TRUE )
	{
		return;
	}

	// update the naybors list
	droidGetNaybors(psDroid);

	moveUpdateDroidDirection( psDroid, &speed, direction, VTOL_SPIN_ANGLE,
				psDroid->baseSpeed, psDroid->baseSpeed/3, &iDroidDir, &fSpeed );

	fNormalSpeed = moveCalcNormalSpeed( psDroid, fSpeed, iDroidDir,
										VTOL_ACCEL, VTOL_DECEL );
	fPerpSpeed   = MKF(0);
	moveCombineNormalAndPerpSpeeds( psDroid, fNormalSpeed,
										fPerpSpeed, iDroidDir );

	moveGetDroidPosDiffs( psDroid, &dx, &dy );
	moveUpdateDroidPos( psDroid, dx, dy );
}

static void
moveUpdateCyborgModel( DROID *psDroid, SDWORD moveSpeed, SDWORD moveDir, UBYTE oldStatus )
{
	PROPULSION_STATS	*psPropStats;
	BASE_OBJECT			*psObj = (BASE_OBJECT *) psDroid;
	UDWORD				iMapZ = map_Height(psDroid->x, psDroid->y);
	SDWORD				iDist, iDx, iDy, iDz, iDroidZ;
	BOOL			bRet;


	// nothing to do if the droid is stopped
	if ( moveDroidStopped( psDroid, moveSpeed ) == TRUE )
	{
		if ( psDroid->psCurAnim != NULL )
		{
			if ( animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->uwID ) == FALSE )
			{
				debug( LOG_NEVER, "moveUpdateCyborgModel: couldn't remove walk anim\n" );
			}
			psDroid->psCurAnim = NULL;
		}

		return;
	}

	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	ASSERT( psPropStats != NULL,
			"moveUpdateCyborgModel: invalid propulsion stats pointer" );

	/* do vertical movement */
	if ( psPropStats->propulsionType == JUMP )
	{
		iDz = psDroid->sMove.iVertSpeed * (SDWORD)frameTime / GAME_TICKS_PER_SEC;
		iDroidZ = (SDWORD) psDroid->z;

		if ( iDroidZ+iDz < (SDWORD) iMapZ )
		{
			psDroid->sMove.iVertSpeed = 0;
			psDroid->z = (UWORD)iMapZ;
		}
		else
		{
			psDroid->z = (UWORD)(psDroid->z + iDz);
		}

		if ( (psDroid->z >= (iMapZ+CYBORG_MAX_JUMP_HEIGHT)) &&
			 (psDroid->sMove.iVertSpeed > 0)                        )
		{
			psDroid->sMove.iVertSpeed = (SWORD)-CYBORG_VERTICAL_SPEED;
		}


		psDroid->sMove.fz = MAKEFRACT(psDroid->z);

	}

	/* calculate move distance */
	iDx = (SDWORD) psDroid->sMove.DestinationX - (SDWORD) psDroid->x;
	iDy = (SDWORD) psDroid->sMove.DestinationY - (SDWORD) psDroid->y;
	iDz = (SDWORD) psDroid->z - (SDWORD) iMapZ;
	iDist = MAKEINT( trigIntSqrt( iDx*iDx + iDy*iDy ) );

	/* set jumping cyborg walking short distances */
	if ( (psPropStats->propulsionType != JUMP) ||
		 ((psDroid->sMove.iVertSpeed == 0)      &&
		  (iDist < CYBORG_MIN_JUMP_DISTANCE))       )
	{
		if ( psDroid->psCurAnim == NULL )
		{
			// Only add the animation if the droid is on screen, saves memory and time.
			if(clipXY(psDroid->x,psDroid->y))
			{
//DBPRINTF(("Added cyborg run anim\n"));
                //What about my new cyborg droids?????!!!!!!!
				/*if ( psDroid->droidType == DROID_CYBORG )
				{
					psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_CYBORG_RUN, 0, 0 );
				}
				else if ( psDroid->droidType == DROID_CYBORG_SUPER )
				{
					psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_SUPERCYBORG_RUN, 0, 0 );
				}*/
                if ( psDroid->droidType == DROID_CYBORG_SUPER )
				{
					psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_SUPERCYBORG_RUN, 0, 0 );
				}
                else if (cyborgDroid(psDroid))
                {
                    psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_CYBORG_RUN, 0, 0 );
                }
			}
		} else {
			// If the droid went off screen then remove the animation, saves memory and time.
			if(!clipXY(psDroid->x,psDroid->y)) {
				bRet = animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->psAnim->uwID );
				ASSERT( bRet == TRUE, "moveUpdateCyborgModel : animObj_Remove failed" );
				psDroid->psCurAnim = NULL;
//DBPRINTF(("Removed cyborg run anim\n"));
			}
		}

		/* use baba person movement */
		moveUpdatePersonModel(psDroid,moveSpeed,moveDir);
	}
	else
	{
		/* jumping cyborg: remove walking animation if present */
		if ( psDroid->psCurAnim != NULL )
		{
			if ( (psDroid->psCurAnim->uwID == ID_ANIM_CYBORG_RUN ||
				  psDroid->psCurAnim->uwID == ID_ANIM_SUPERCYBORG_RUN ||
				  psDroid->psCurAnim->uwID == ID_ANIM_CYBORG_PACK_RUN) &&
				 (animObj_Remove( &psDroid->psCurAnim, psDroid->psCurAnim->uwID ) == FALSE) )
			{
				debug( LOG_NEVER, "moveUpdateCyborgModel: couldn't remove walk anim\n" );
			}
		}

		/* add jumping or landing anim */
		if ( (oldStatus == MOVEPOINTTOPOINT) &&
				  (psDroid->sMove.Status == MOVEINACTIVE) )
		{
			psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_CYBORG_PACK_LAND, 0, 1 );
			animObj_SetDoneFunc( psDroid->psCurAnim, moveCyborgTouchDownAnimDone );
		}
		else if ( psDroid->sMove.Status == MOVEPOINTTOPOINT )
		{
			if ( psDroid->z == iMapZ )
			{
				if ( psDroid->sMove.iVertSpeed == 0 )
				{
					psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_CYBORG_PACK_JUMP, 0, 1 );
					animObj_SetDoneFunc( psDroid->psCurAnim, moveCyborgLaunchAnimDone );
				}
				else
				{
					psDroid->psCurAnim = animObj_Add( psObj, ID_ANIM_CYBORG_PACK_LAND, 0, 1 );
					animObj_SetDoneFunc( psDroid->psCurAnim, moveCyborgTouchDownAnimDone );
				}
			}
			else
			{
				moveUpdateJumpCyborgModel( psDroid, moveSpeed, moveDir );
			}
		}
	}

	psDroid->pitch = 0;
	psDroid->roll  = 0;
}

static BOOL moveDescending( DROID *psDroid, UDWORD iMapHeight )
{

	if ( psDroid->z > iMapHeight )

	{
		/* descending */
		psDroid->sMove.iVertSpeed = (SWORD)-VTOL_VERTICAL_SPEED;
		psDroid->sMove.speed = MAKEFRACT(0);
//		psDroid->sMove.Speed = 0;

		/* return TRUE to show still descending */
		return TRUE;
	}
	else
	{
		/* on floor - stop */
		psDroid->sMove.iVertSpeed = 0;

		/* conform to terrain */
		updateDroidOrientation(psDroid);

		/* return FALSE to show stopped descending */
		return FALSE;
	}
}


BOOL moveCheckDroidMovingAndVisible( void *psObj )
{
	DROID	*psDroid = (DROID*)psObj;

	if ( psDroid == NULL )
	{
		return FALSE;
	}

	/* check for dead, not moving or invisible to player */
	if ( psDroid->died || moveDroidStopped( psDroid, 0 ) ||
		 (psDroid->droidType == DROID_TRANSPORTER && psDroid->order == DORDER_NONE) ||
		 !(psDroid->visible[selectedPlayer] || godMode)                                )
	{
		psDroid->iAudioID = NO_SOUND;
		return FALSE;
	}

	return TRUE;
}



static void movePlayDroidMoveAudio( DROID *psDroid )
{
	SDWORD				iAudioID = NO_SOUND;
	PROPULSION_TYPES	*psPropType;
	UBYTE				iPropType = 0;

	ASSERT( psDroid != NULL,
		"movePlayUnitMoveAudio: unit pointer invalid\n" );

	if ( (psDroid != NULL) &&
		 (psDroid->visible[selectedPlayer] || godMode) )
	{
		iPropType = asPropulsionStats[(psDroid)->asBits[COMP_PROPULSION].nStat].propulsionType;
		psPropType = &asPropulsionTypes[iPropType];

		/* play specific wheeled and transporter or stats-specified noises */
		if ( iPropType == WHEELED && psDroid->droidType != DROID_CONSTRUCT )
		{
			iAudioID = ID_SOUND_TREAD;
		}
		else if ( psDroid->droidType == DROID_TRANSPORTER )
		{
			iAudioID = ID_SOUND_BLIMP_FLIGHT;
		}
		//else if ( iPropType == LEGGED && psDroid->droidType == DROID_CYBORG )
        else if ( iPropType == LEGGED && cyborgDroid(psDroid))
		{
			iAudioID = ID_SOUND_CYBORG_MOVE;
		}
		else
		{
			iAudioID = psPropType->moveID;
		}

		if ( iAudioID != NO_SOUND )
		{
			if ( audio_PlayObjDynamicTrack( psDroid, iAudioID,
					moveCheckDroidMovingAndVisible ) )
			{
				psDroid->iAudioID = iAudioID;
			}
		}
	}
}



static BOOL moveDroidStartCallback( void *psObj )
{
	DROID *psDroid = (DROID*)psObj;

	if ( psDroid == NULL )
	{
		return FALSE;
	}

	movePlayDroidMoveAudio( psDroid );

	return TRUE;
}



static void movePlayAudio( DROID *psDroid, BOOL bStarted, BOOL bStoppedBefore, SDWORD iMoveSpeed )
{
	UBYTE				propType;
	PROPULSION_STATS	*psPropStats;
	PROPULSION_TYPES	*psPropType;
	BOOL				bStoppedNow;
	SDWORD				iAudioID = NO_SOUND;
	AUDIO_CALLBACK		pAudioCallback = NULL;

	/* get prop stats */
	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	ASSERT( psPropStats != NULL,
			"moveUpdateUnit: invalid propulsion stats pointer" );
	propType = psPropStats->propulsionType;
	psPropType = &asPropulsionTypes[propType];

	/* get current droid motion status */
	bStoppedNow = moveDroidStopped( psDroid, iMoveSpeed );

	if ( bStarted )
	{
		/* play start audio */
		if ( (propType == WHEELED && psDroid->droidType != DROID_CONSTRUCT) ||
			 (psPropType->startID == NO_SOUND)                                 )
		{
			movePlayDroidMoveAudio( psDroid );
			return;
		}
		else if ( psDroid->droidType == DROID_TRANSPORTER )
		{
			iAudioID = ID_SOUND_BLIMP_TAKE_OFF;
		}
		else
		{
			iAudioID = psPropType->startID;
		}

		pAudioCallback = moveDroidStartCallback;
	}
	else if ( !bStoppedBefore && bStoppedNow &&
				(psPropType->shutDownID != NO_SOUND) )
	{
		/* play stop audio */
		if ( psDroid->droidType == DROID_TRANSPORTER )
		{
			iAudioID = ID_SOUND_BLIMP_LAND;
		}
		else if ( propType != WHEELED || psDroid->droidType == DROID_CONSTRUCT )
		{
			iAudioID = psPropType->shutDownID;
		}
	}
	else if ( (!bStoppedBefore && !bStoppedNow) &&
			  (psDroid->iAudioID == NO_SOUND)      )
	{
		/* play move audio */
		movePlayDroidMoveAudio( psDroid );
		return;
	}

	if ( (iAudioID != NO_SOUND) &&
		 (psDroid->visible[selectedPlayer] || godMode) )
	{
		if ( audio_PlayObjDynamicTrack( psDroid, iAudioID,
				pAudioCallback ) )
		{
			psDroid->iAudioID = iAudioID;
		}
	}

#if 0
if ( oldStatus != newStatus )
{
	char	szOldStatus[100], szNewStatus[100];
	moveGetStatusStr( oldStatus, szOldStatus );
	moveGetStatusStr( newStatus, szNewStatus );
	debug( LOG_NEVER, "oldStatus = %s newStatus = %s\n", szOldStatus, szNewStatus );
}
#endif

}


// called when a droid moves to a new tile.
// use to pick up oil, etc..
static void checkLocalFeatures(DROID *psDroid)
{
	SDWORD			i;
	BASE_OBJECT		*psObj;

	// only do for players droids.
	if(psDroid->player != selectedPlayer)
	{
		return;
	}

	droidGetNaybors(psDroid);// update naybor list.

	// scan the neighbours
	for(i=0; i<(SDWORD)numNaybors; i++)
	{
#define DROIDDIST (((TILE_UNITS*3)/2) * ((TILE_UNITS*3)/2))
		psObj = asDroidNaybors[i].psObj;
		if (   psObj->type != OBJ_FEATURE
			|| ((FEATURE *)psObj)->psStats->subType != FEAT_OIL_DRUM
			|| asDroidNaybors[i].distSqr >= DROIDDIST )
		{
			// object too far away to worry about
			continue;
		}


		if(bMultiPlayer && (psObj->player == ANYPLAYER))
		{
			giftPower(ANYPLAYER,selectedPlayer,TRUE);			// give power and tell everyone.
			addOilDrum(1);
		}
		else

		{
			addPower(selectedPlayer,OILDRUM_POWER);
		}
		removeFeature((FEATURE*)psObj);							// remove artifact+ send multiplay info.

	}
}


//static UDWORD LastMoveFrame;

/* Frame update for the movement of a tracked droid */
void moveUpdateDroid(DROID *psDroid)
{
//	SDWORD		xdiff,ydiff, obstX,obstY;
//	UDWORD		mapX,mapY, tarSpeed;
//	float		newX,newY;
//	float		speed;
//	float		dangle;
//	BASE_OBJECT	*psObst;

	float				tx,ty;		 //adiff, dx,dy, mx,my;
	float				tangle;		// thats DROID angle and TARGET angle - not some bizzare pun :-)
									// doesn't matter - they're still shit names...! :-)
	SDWORD				fx, fy;
	UDWORD				oldx, oldy, iZ;
	UBYTE				oldStatus = psDroid->sMove.Status;
	SDWORD				moveSpeed;
	float				moveDir;
	PROPULSION_STATS	*psPropStats;
	Vector3i pos;
	BOOL				bStarted = FALSE, bStopped;
//	UDWORD				landX,landY;

//	ASSERT( psDroid->x != 0 && psDroid->y != 0,
//		"moveUpdateUnit: unit at (0,0)" );

	psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	ASSERT( psPropStats != NULL,
			"moveUpdateUnit: invalid propulsion stats pointer" );

//	if(driveModeActive()) {
//		driveUpdateDroid(psDroid);
//	}

    //if the droid has been attacked by an EMP weapon, it is temporarily disabled
    if (psDroid->lastHitWeapon == WSC_EMP)
    {
        if (gameTime - psDroid->timeLastHit < EMP_DISABLE_TIME)
        {
            //get out without updating
            return;
        }
    }

	/* save current motion status of droid */
	bStopped = moveDroidStopped( psDroid, 0 );

#ifdef DEBUG_GROUP4
	if (psDroid->sMove.Status != MOVEINACTIVE &&
		psDroid->sMove.Status != MOVETURN &&
		psDroid->sMove.Status != MOVEPOINTTOPOINT)
	{
		debug( LOG_MOVEMENT, "status: id %d state %d\n", psDroid->id, psDroid->sMove.Status);
	}
#endif

	fpathSetBlockingTile( psPropStats->propulsionType );
	fpathSetCurrentObject( (BASE_OBJECT *) psDroid );

	moveSpeed = 0;
	moveDir = psDroid->direction;

	/* get droid height */
	iZ = map_Height(psDroid->x, psDroid->y);

	switch (psDroid->sMove.Status)
	{
	case MOVEINACTIVE:
		if ( (psDroid->droidType == DROID_PERSON) &&
			 (psDroid->psCurAnim != NULL) &&
			 (psDroid->psCurAnim->psAnim->uwID == ID_ANIM_DROIDRUN) )
		{
			psDroid->psCurAnim->bVisible = FALSE;
		}
		break;
	case MOVEROUTE:
	case MOVEROUTESHUFFLE:
	case MOVESHUFFLE:
		// deal with both waiting for a route (MOVEROUTE) and the droid shuffle (MOVESHUFFLE)
		// here because droids waiting for a route need to shuffle out of the way (MOVEROUTESHUFFLE)
		// of those that have already got a route

		if ((psDroid->sMove.Status == MOVEROUTE) ||
			(psDroid->sMove.Status == MOVEROUTESHUFFLE))
		{
			// see if this droid started waiting for a route before the previous one
			// and note it to be the next droid to route.
			// selectedPlayer always gets precidence in single player
			if (psNextRouteDroid == NULL)
			{
				debug( LOG_MOVEMENT, "Waiting droid set to %d (player %d) started at %d now %d (none waiting)\n",
					psDroid->id, psDroid->player, psDroid->sMove.bumpTime, gameTime);
				psNextRouteDroid = psDroid;
			}

			else if (bMultiPlayer &&
					 (psNextRouteDroid->sMove.bumpTime > psDroid->sMove.bumpTime))
			{
				debug( LOG_MOVEMENT, "Waiting droid set to %d (player %d) started at %d now %d (mulitplayer)\n",
					psDroid->id, psDroid->player, psDroid->sMove.bumpTime, gameTime);
				psNextRouteDroid = psDroid;
			}

			else if ( (psDroid->player == selectedPlayer) &&
					  ( (psNextRouteDroid->player != selectedPlayer) ||
						(psNextRouteDroid->sMove.bumpTime > psDroid->sMove.bumpTime) ) )
			{
				debug( LOG_MOVEMENT, "Waiting droid set to %d (player %d) started at %d now %d (selectedPlayer)\n",
					psDroid->id, psDroid->player, psDroid->sMove.bumpTime, gameTime);
				psNextRouteDroid = psDroid;
			}
			else if ( (psDroid->player != selectedPlayer) &&
					  (psNextRouteDroid->player != selectedPlayer) &&
					  (psNextRouteDroid->sMove.bumpTime > psDroid->sMove.bumpTime) )
			{
				debug( LOG_MOVEMENT, "Waiting droid set to %d (player %d) started at %d now %d (non selectedPlayer)\n",
					psDroid->id, psDroid->player, psDroid->sMove.bumpTime, gameTime);
				psNextRouteDroid = psDroid;
			}
		}

		if ((psDroid->sMove.Status == MOVEROUTE) ||
			(psDroid->sMove.Status == MOVEROUTESHUFFLE))
//			 (gameTime >= psDroid->sMove.bumpTime) )
		{
			psDroid->sMove.fx = MAKEFRACT(psDroid->x);
			psDroid->sMove.fy = MAKEFRACT(psDroid->y);

			psDroid->sMove.fz = MAKEFRACT(psDroid->z);

//			psDroid->sMove.bumpTime = 0;

			turnOffMultiMsg(TRUE);
			moveDroidTo(psDroid, psDroid->sMove.DestinationX,psDroid->sMove.DestinationY);
			fpathSetBlockingTile( psPropStats->propulsionType );
			turnOffMultiMsg(FALSE);
		}
		else if ((psDroid->sMove.Status == MOVESHUFFLE) ||
				 (psDroid->sMove.Status == MOVEROUTESHUFFLE))
		{
			if (moveReachedWayPoint(psDroid) ||
				((psDroid->sMove.shuffleStart + MOVE_SHUFFLETIME) < gameTime))
			{
				if ( psDroid->sMove.Status == MOVEROUTESHUFFLE )
				{
					psDroid->sMove.Status = MOVEROUTE;
				}
				else if ( psPropStats->propulsionType == LIFT )
				{
					psDroid->sMove.Status = MOVEHOVER;
				}
				else
				{
					psDroid->sMove.Status = MOVEINACTIVE;
				}
			}
			else
			{
				// Calculate a target vector
				moveGetDirection(psDroid, &tx,&ty);

				// Turn the droid if necessary
				tangle = vectorToAngle(tx,ty);

				if ( psDroid->droidType == DROID_TRANSPORTER )
				{
					debug( LOG_NEVER, "a) dir %g,%g (%g)\n", tx, ty, tangle );
				}

				moveSpeed = moveCalcDroidSpeed(psDroid);
				moveDir = tangle;
			}
		}

		break;
	case MOVEWAITROUTE:
		moveDroidTo(psDroid, psDroid->sMove.DestinationX,psDroid->sMove.DestinationY);
		fpathSetBlockingTile( psPropStats->propulsionType );
		break;
	case MOVENAVIGATE:
		// Get the next control point
		if (!moveNextTarget(psDroid))
		{
			// No more waypoints - finish
			if ( psPropStats->propulsionType == LIFT )
			{
				psDroid->sMove.Status = MOVEHOVER;
			}
			else
			{
				psDroid->sMove.Status = MOVEINACTIVE;
			}
			break;
		}

		// Calculate the direction vector
//		psDroid->direction = calcDirection(psDroid->x,psDroid->y, tarX,tarY);
		psDroid->sMove.fx = MAKEFRACT(psDroid->x);
		psDroid->sMove.fy = MAKEFRACT(psDroid->y);

		psDroid->sMove.fz = MAKEFRACT(psDroid->z);


		moveCalcBoundary(psDroid);

		if (vtolDroid(psDroid))
		{
			psDroid->pitch = 0;
		}

		psDroid->sMove.Status = MOVEPOINTTOPOINT;
		psDroid->sMove.bumpTime = 0;

		/* save started status for movePlayAudio */
		if ( psDroid->sMove.speed == MKF(0) )
		{
			bStarted = TRUE;
		}

		break;
	case MOVEPOINTTOPOINT:
	case MOVEPAUSE:
		// moving between two way points

#ifdef ARROWS
		// display the route
		if (psDroid->selected)
		{
			SDWORD	pos, x,y,z, px,py,pz;

			// display the boundary vector
			x = psDroid->sMove.targetX;
			y = psDroid->sMove.targetY;
			z = map_Height( x,y );
			px = x - psDroid->sMove.boundY;
			py = y + psDroid->sMove.boundX;
			pz = map_Height( px,py );
			arrowAdd( x,y,z, px,py,pz, REDARROW );

			// display the route
			px = (SDWORD)psDroid->x;
			py = (SDWORD)psDroid->y;
			pz = map_Height( px, py );
			pos = psDroid->sMove.Position;
			pos = ((pos - 1) <= 0) ? 0 : (pos - 1);
			for(; pos < psDroid->sMove.numPoints; pos += 1)
			{
				x = (psDroid->sMove.asPath[pos].x << TILE_SHIFT) + TILE_UNITS/2;
				y = (psDroid->sMove.asPath[pos].y << TILE_SHIFT) + TILE_UNITS/2;
				z = map_Height( x,y );
				arrowAdd( px,py,pz, x,y,z, REDARROW );

				px = x; py = y; pz = z;
			}
		}
#endif

		// See if the target point has been reached
		if (moveReachedWayPoint(psDroid))
		{
			// Got there - move onto the next waypoint
			if (!moveNextTarget(psDroid))
			{
				// No more waypoints - finish
//				psDroid->sMove.Status = MOVEINACTIVE;
				if ( psPropStats->propulsionType == LIFT )
				{
					psDroid->sMove.Status = MOVEHOVER;
				}
				else
				{
					psDroid->sMove.Status = MOVETURN;
				}
				debug( LOG_MOVEMENT, "\n");
				break;
			}
			moveCalcBoundary(psDroid);
		}

#if !FORMATIONS_DISABLE
		if (psDroid->sMove.psFormation &&
			psDroid->sMove.Position == psDroid->sMove.numPoints)
		{
			if (vtolDroid(psDroid))
			{
				// vtols have to use the ground blocking tile when they are going to land
				fpathBlockingTile = fpathGroundBlockingTile;
			}

			if (formationGetPos(psDroid->sMove.psFormation, (BASE_OBJECT *)psDroid, &fx,&fy,TRUE))
			{
				psDroid->sMove.targetX = fx;
				psDroid->sMove.targetY = fy;
				moveCalcBoundary(psDroid);
			}

			/*if (vtolDroid(psDroid))
			{
				// reset to the normal blocking tile
				fpathBlockingTile = fpathLiftBlockingTile;
			}*/
		}
#endif
//		DebugP=FALSE;
//		if ( psDroid->droidType == DROID_TRANSPORTER ) DebugP=TRUE;

		// Calculate a target vector
		moveGetDirection(psDroid, &tx,&ty);


		// Turn the droid if necessary
		// calculate the difference in the angles
//		dangle = (float) psDroid->direction;
		tangle = vectorToAngle(tx,ty);




		moveSpeed = moveCalcDroidSpeed(psDroid);

		moveDir = tangle;


//if ( psDroid->droidType == DROID_TRANSPORTER )
//{
//			DBPRINTF(("dir %d,%d ($%x=%d)\n",tx,ty,tangle,moveDir));
//	}


		if ((psDroid->sMove.bumpTime != 0) &&
			(psDroid->sMove.pauseTime + psDroid->sMove.bumpTime + BLOCK_PAUSETIME < gameTime))
		{
			if (psDroid->sMove.Status == MOVEPOINTTOPOINT)
			{
				psDroid->sMove.Status = MOVEPAUSE;
			}
			else
			{
				psDroid->sMove.Status = MOVEPOINTTOPOINT;
			}
			psDroid->sMove.pauseTime = (UWORD)(gameTime - psDroid->sMove.bumpTime);
		}

		if ((psDroid->sMove.Status == MOVEPAUSE) &&
			(psDroid->sMove.bumpTime != 0) &&
			(psDroid->sMove.lastBump > psDroid->sMove.pauseTime) &&
			(psDroid->sMove.lastBump + psDroid->sMove.bumpTime + BLOCK_PAUSERELEASE < gameTime))
		{
			psDroid->sMove.Status = MOVEPOINTTOPOINT;
		}

		break;
	case MOVETURN:
		// Turn the droid to it's final facing
		if (psDroid->sMove.psFormation &&
			psDroid->sMove.psFormation->refCount > 1 &&
			(SDWORD)psDroid->direction != (SDWORD)psDroid->sMove.psFormation->dir)
		{
			moveSpeed = 0;
			moveDir = psDroid->sMove.psFormation->dir;
		}
		else
		{
			if ( psPropStats->propulsionType == LIFT )
			{
				psDroid->sMove.Status = MOVEPOINTTOPOINT;
			}
			else
			{
				psDroid->sMove.Status = MOVEINACTIVE;
			}
		}
		break;
	case MOVETURNTOTARGET:
		moveSpeed = 0;
		moveDir = calcDirection( psDroid->x, psDroid->y, psDroid->sMove.targetX, psDroid->sMove.targetY );
		if ((int)psDroid->direction == (int)moveDir)
		{
			if ( psPropStats->propulsionType == LIFT )
			{
				psDroid->sMove.Status = MOVEPOINTTOPOINT;
			}
			else
			{
				psDroid->sMove.Status = MOVEINACTIVE;
			}
		}
		break;
	case MOVEHOVER:
		/* change vtols to attack run mode if target found - but not if no ammo*/
		/*if ( psDroid->droidType != DROID_CYBORG && psDroid->psTarget != NULL &&
			!vtolEmpty(psDroid))
		{
			psDroid->sMove.Status = MOVEPOINTTOPOINT;
			break;
		}*/

/*		moveGetDirection(psDroid, &tx,&ty);
		tangle = vectorToAngle(tx,ty);
		moveSpeed = moveCalcDroidSpeed(psDroid);
		moveDir = MAKEINT(tangle);*/

		/* descend if no orders or actions or cyborg at target */
/*		if ( (psDroid->droidType == DROID_CYBORG) ||
			 ((psDroid->action == DACTION_NONE) && (psDroid->order == DORDER_NONE)) ||
			 ((psDroid->action == DACTION_NONE) && (psDroid->order == DORDER_TRANSPORTIN)) ||
			 ((psDroid->action == DACTION_NONE) && (psDroid->order == DORDER_GUARD)) ||
			 (psDroid->action == DACTION_MOVE) || (psDroid->action == DACTION_WAITFORREARM) ||
			 (psDroid->action == DACTION_WAITDURINGREARM)  || (psDroid->action == DACTION_FIRESUPPORT))*/
		{
			if ( moveDescending( psDroid, iZ ) == FALSE )
			{
				/* reset move state */
				psDroid->sMove.Status = MOVEINACTIVE;
			}
/*			else
			{
				// see if the landing position is clear
				landX = psDroid->x;
				landY = psDroid->y;
				actionVTOLLandingPos(psDroid, &landX,&landY);
				if ((SDWORD)(landX >> TILE_SHIFT) != (psDroid->x >> TILE_SHIFT) &&
					(SDWORD)(landY >> TILE_SHIFT) != (psDroid->y >> TILE_SHIFT))
				{
					moveDroidToDirect(psDroid, landX,landY);
				}
			}*/
		}
/*		else
		{
			moveAdjustVtolHeight( psDroid, iZ );
		}*/

		break;

		// Driven around by the player.
	case MOVEDRIVE:
		driveSetDroidMove(psDroid);
		moveSpeed = driveGetMoveSpeed();	//MAKEINT(psDroid->sMove.speed);
		moveDir = driveGetMoveDir();		//psDroid->sMove.dir;
//		DBPRINTF(("%d\n",frameGetFrameNumber()-LastMoveFrame);
//		LastMoveFrame = frameGetFrameNumber();
//		psDroid->sMove.speed = MAKEFRACT(driveSpeed);
//		psDroid->sMove.dir = driveDir;
//		psDroid->direction = driveDir;
		break;

	// Follow the droid being driven around by the player.
	case MOVEDRIVEFOLLOW:
//		orderDroidLoc(psDroid,DORDER_MOVE, psDrivenDroid->x,psDrivenDroid->y)
//		if (gameTime >= psDroid->sMove.bumpTime)
//		{
//			if(psDrivenDroid != NULL) {
//				psDroid->sMove.DestinationX = psDrivenDroid->x;
//				psDroid->sMove.DestinationY = psDrivenDroid->y;
//				psDroid->sMove.fx = MAKEFRACT(psDroid->x);
//				psDroid->sMove.fy = MAKEFRACT(psDroid->y);
//				psDroid->sMove.bumpTime = 0;
//				moveDroidTo(psDroid, psDroid->sMove.DestinationX,psDroid->sMove.DestinationY);
//			} else {
//				psDroid->sMove.Status = MOVEINACTIVE;
//			}
//		}
		break;
	default:
		ASSERT( FALSE, "moveUpdateUnit: unknown move state" );
		break;
	}

	// Update the movement model for the droid
	oldx = psDroid->x;
	oldy = psDroid->y;

	if ( psDroid->droidType == DROID_PERSON )
	{
		moveUpdatePersonModel(psDroid, moveSpeed, moveDir);
	}
	//else if ( psDroid->droidType == DROID_CYBORG )
    else if (cyborgDroid(psDroid))
	{
		moveUpdateCyborgModel(psDroid, moveSpeed, moveDir, oldStatus);
	}
	else if ( psPropStats->propulsionType == LIFT )
	{
		moveUpdateVtolModel(psDroid, moveSpeed, moveDir);
	}
	else
	{
		moveUpdateGroundModel(psDroid, moveSpeed, moveDir);
	}

	if ((SDWORD)oldx >> TILE_SHIFT != psDroid->x >> TILE_SHIFT ||
		(SDWORD)oldy >> TILE_SHIFT != psDroid->y >> TILE_SHIFT)
	{
		visTilesUpdate((BASE_OBJECT *)psDroid,FALSE);
		gridMoveObject((BASE_OBJECT *)psDroid, (SDWORD)oldx,(SDWORD)oldy);

		// object moved from one tile to next, check to see if droid is near stuff.(oil)
		checkLocalFeatures(psDroid);
	}

	// See if it's got blocked
	if ( (psPropStats->propulsionType != LIFT) && moveBlocked(psDroid) )
	{
		debug( LOG_MOVEMENT, "status: id %d blocked\n", psDroid->id);
		psDroid->sMove.Status = MOVETURN;
	}

//	// If were in drive mode and the droid is a follower then stop it when it gets within
//	// range of the driver.
//	if(driveIsFollower(psDroid)) {
//		if(DoFollowRangeCheck) {
////DBPRINTF(("%d\n",gameTime);
//			if(driveInDriverRange(psDroid)) {
//				psDroid->sMove.Status = MOVEINACTIVE;
////				ClearFollowRangeCheck = TRUE;
//			} else {
//				AllInRange = FALSE;
//			}
//		}
//	}

	// reset the blocking tile function and current object
	fpathBlockingTile = fpathGroundBlockingTile;
	fpathSetCurrentObject( NULL );

//	ASSERT( psDroid->x != 0 && psDroid->y != 0,
//		"moveUpdateUnit (end): unit at (0,0)" );

	/* If it's sitting in water then it's got to go with the flow! */
	if(TERRAIN_TYPE(mapTile(psDroid->x/TILE_UNITS,psDroid->y/TILE_UNITS)) == TER_WATER)
	{
		updateDroidOrientation(psDroid);
	}

	if( (psDroid->inFire && psDroid->type != DROID_PERSON) && psDroid->visible[selectedPlayer])
	{
		pos.x = psDroid->x + (18-rand()%36);
		pos.z = psDroid->y + (18-rand()%36);
//		pos.y = map_Height(pos.x,pos.z) + (psDroid->sDisplay.imd->ymax/3);
		pos.y = psDroid->z + (psDroid->sDisplay.imd->ymax/3);
		addEffect(&pos,EFFECT_EXPLOSION,EXPLOSION_TYPE_SMALL,FALSE,NULL,0);
	}


#if DROID_RUN_SOUND
	movePlayAudio( psDroid, bStarted, bStopped, moveSpeed );
#endif

}
