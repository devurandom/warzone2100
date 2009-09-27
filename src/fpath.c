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
 * @file fpath.c
 *
 * Interface to the routing functions.
 *
 */

#include <SDL.h>
#include <SDL_thread.h>

#include "lib/framework/frame.h"

#include "objects.h"
#include "map.h"
#include "raycast.h"
#include "geometry.h"
#include "hci.h"
#include "order.h"
#include "multiplay.h"
#include "astar.h"
#include "action.h"
#include "formation.h"

#include "fpath.h"

// If the path finding system is shutdown or not
static volatile bool fpathQuit = false;

/* Beware: Enabling this will cause significant slow-down. */
#undef DEBUG_MAP

/* minimum height difference for VTOL blocking tile */
#define	LIFT_BLOCK_HEIGHT_LIGHTBODY		  30
#define	LIFT_BLOCK_HEIGHT_MEDIUMBODY	 350
#define	LIFT_BLOCK_HEIGHT_HEAVYBODY		 350

typedef struct _jobDone
{
	UDWORD		droidID;	///< Unique droid ID.
	MOVE_CONTROL	sMove;		///< New movement values for the droid.
	struct _jobDone	*next;		///< Next result in the list.
	FPATH_RETVAL	retval;		///< Result value from path-finding.
	bool		done;		///< If the result is finished and ready for use.
} PATHRESULT;


#define NUM_BASIC	 8
#define NUM_DIR		24

// Convert a direction into an offset, spanning two tiles
static const Vector2i aDirOffset[NUM_DIR] =
{
	{  0,  1 }, { -1,  1 }, { -1,  0 }, { -1, -1 }, {  0, -1 }, {  1, -1 }, {  1,  0 }, {  1,  1 },
	{ -2, -2 }, { -1, -2 }, {  0, -2 }, {  1, -2 }, {  2, -2 }, { -2, -1 }, {  2, -1 }, { -2,  0 },
	{  2,  0 }, { -2,  1 }, {  2,  1 }, { -2,  2 }, { -1,  2 }, {  0,  2 }, {  1,  2 }, {  2,  2 },
};

// threading stuff
static SDL_Thread	*fpathThread = NULL;
static SDL_sem		*fpathSemaphore = NULL;
static PATHJOB		*firstJob = NULL;
static PATHRESULT	*firstResult = NULL;


static void fpathExecute(PATHJOB *psJob, PATHRESULT *psResult);


/** Find the length of the job queue. Function is thread-safe. */
static int fpathJobQueueLength(void)
{
	PATHJOB *psJob;
	int count = 0;

	SDL_SemWait(fpathSemaphore);
	psJob = firstJob;
	while (psJob)
	{
		count++;
		psJob = psJob->next;
	}
	SDL_SemPost(fpathSemaphore);
	return count;
}


/** Find the length of the result queue, excepting future results. Function is thread-safe. */
static int fpathResultQueueLength(void)
{
	PATHRESULT *psResult;
	int count = 0;

	SDL_SemWait(fpathSemaphore);
	psResult = firstResult;
	while (psResult)
	{
		if (psResult->done)
		{
			count++;
		}
		psResult = psResult->next;
	}
	SDL_SemPost(fpathSemaphore);
	return count;
}


/** This runs in a separate thread */
static int fpathThreadFunc(WZ_DECL_UNUSED void *data)
{
	SDL_SemWait(fpathSemaphore);

	while (!fpathQuit)
	{
		PATHJOB		job;
		PATHRESULT	*psResult, result;
		bool		gotWork = false;

		// Pop the first job off the queue
		if (firstJob)
		{
			PATHJOB	*next = firstJob->next;

			job = *firstJob;	// struct copy
			job.next = NULL;
			free(firstJob);
			firstJob = next;
			gotWork = true;
		}

		if (!gotWork)
		{
			SDL_SemPost(fpathSemaphore);
			SDL_Delay(100);
			SDL_SemWait(fpathSemaphore);
			continue;
		}

		// Create future result
		psResult = malloc(sizeof(*psResult));
		psResult->done = false;
		psResult->droidID = job.droidID;
		psResult->sMove.asPath = NULL;
		psResult->retval = FPR_FAILED;

		// Add to beginning of result list
		psResult->next = firstResult;
		firstResult = psResult;
		psResult = NULL;	// now hands off

		SDL_SemPost(fpathSemaphore);

		// Execute path-finding for this job using our local temporaries
		memset(&result, 0, sizeof(result));
		result.sMove.asPath = NULL;
		fpathExecute(&job, &result);

		SDL_SemWait(fpathSemaphore);

		// Find our result again, and replace it with our local temporary
		// We do it this way to avoid a race condition where a droid dies
		// while we are generating its path, and we never free the result.
		psResult = firstResult;
		while (psResult && psResult->droidID != job.droidID)
		{
			psResult = psResult->next;
		}
		if (psResult)
		{
			psResult->sMove = result.sMove; 	// struct copy
			psResult->retval = result.retval;
			psResult->done = true;
		}
	}
	SDL_SemPost(fpathSemaphore);
	return 0;
}


// initialise the findpath module
BOOL fpathInitialise(void)
{
	// The path system is up
	fpathQuit = false;

	if (!fpathThread)
	{
		fpathSemaphore = SDL_CreateSemaphore(1);
		fpathThread = SDL_CreateThread(fpathThreadFunc, NULL);
	}

	return true;
}


void fpathShutdown()
{
	// Signal the path finding thread to quit
	fpathQuit = true;

	fpathHardTableReset();
	if (fpathThread)
	{
		SDL_WaitThread(fpathThread, NULL);
		fpathThread = NULL;
		SDL_DestroySemaphore(fpathSemaphore);
		fpathSemaphore = NULL;
	}
}


/**
 *	Updates the pathfinding system.
 *	@ingroup pathfinding
 */
void fpathUpdate(void)
{
	// Nothing now
}


// Check if the map tile at a location blocks a droid
BOOL fpathBaseBlockingTile(SDWORD x, SDWORD y, PROPULSION_TYPE propulsion, int player, FPATH_MOVETYPE moveType)
{
	MAPTILE	*psTile;

	/* All tiles outside of the map and on map border are blocking. */
	if (x < 1 || y < 1 || x > mapWidth - 1 || y > mapHeight - 1)
	{
		return true;
	}

	/* Check scroll limits (used in campaign to partition the map. */
	if (propulsion != PROPULSION_TYPE_LIFT && (x < scrollMinX + 1 || y < scrollMinY + 1 || x >= scrollMaxX - 1 || y >= scrollMaxY - 1))
	{
		// coords off map - auto blocking tile
		return true;
	}

	psTile = mapTile(x, y);

	// Only tall structures are blocking VTOL now
	if (propulsion == PROPULSION_TYPE_LIFT && !TileHasTallStructure(psTile))
	{
		return false;
	}
	else if (propulsion == PROPULSION_TYPE_LIFT)
	{
		return true;
	}
	else if (propulsion == PROPULSION_TYPE_PROPELLOR && terrainType(psTile) != TER_WATER)
	{
		return true;
	}

	if (TileIsOccupied(psTile) && !TileIsNotBlocking(psTile))
	{
		// If the type of movement order is simple movement (FMT_MOVE) then we treat all genuine obstacles as
		// impassable. However, if it is an attack type order, we assume we can blast our way through enemy buildings.
		if (moveType == FMT_MOVE)
		{
			return true;
		}
		else if (moveType == FMT_ATTACK && psTile->psObject
		         && psTile->psObject->type == OBJ_STRUCTURE && aiCheckAlliances(psTile->psObject->player, player))
		{
			return true;
		}
	}

	if (psTile->tileInfoBits & BITS_FPATHBLOCK 
	    || terrainType(psTile) == TER_CLIFFFACE
	    || (terrainType(psTile) == TER_WATER && propulsion != PROPULSION_TYPE_HOVER && propulsion != PROPULSION_TYPE_PROPELLOR))
	{
		return true;
	}

	return false;
}

// Check if the map tile at a location blocks a droid
BOOL fpathBlockingTile(SDWORD x, SDWORD y, PROPULSION_TYPE propulsion)
{
	return fpathBaseBlockingTile(x, y, propulsion, MAX_PLAYERS, FMT_MOVE);
}


/** Calculate the distance to a tile from a point
 *
 *  @ingroup pathfinding
 */
static inline int fpathDistToTile(int tileX, int tileY, int pointX, int pointY)
{
	// get the difference in world coords
	int xdiff = world_coord(tileX) - pointX;
	int ydiff = world_coord(tileY) - pointY;

	if (xdiff == 0 && ydiff == 0)
	{
		return 0;
	}
	return trigIntSqrt(xdiff * xdiff + ydiff * ydiff);
}


static void fpathSetMove(MOVE_CONTROL *psMoveCntl, SDWORD targetX, SDWORD targetY)
{
	psMoveCntl->asPath = realloc(psMoveCntl->asPath, sizeof(*psMoveCntl->asPath));
	psMoveCntl->DestinationX = targetX;
	psMoveCntl->DestinationY = targetY;
	psMoveCntl->numPoints = 1;
	psMoveCntl->asPath[0].x = map_coord(targetX);
	psMoveCntl->asPath[0].y = map_coord(targetY);
}


void fpathSetDirectRoute(DROID *psDroid, SDWORD targetX, SDWORD targetY)
{
	fpathSetMove(&psDroid->sMove, targetX, targetY);
}


void fpathRemoveDroidData(int id)
{
	PATHJOB		*psJob;
	PATHJOB		*psPrevJob = NULL;
	PATHRESULT	*psResult;
	PATHRESULT	*psPrevResult = NULL;

	SDL_SemWait(fpathSemaphore);

	psJob = firstJob;
	psResult = firstResult;

	while (psJob)
	{
		if (psJob->droidID == id)
		{
			if (psPrevJob)
			{
				psPrevJob->next = psJob->next;
				free(psJob);
				psJob = psPrevJob->next;
			}
			else
			{
				firstJob = psJob->next;
				free(psJob);
				psJob = firstJob;
			}
		}
		else
		{
			psPrevJob = psJob;
			psJob = psJob->next;
		}
	}
	while (psResult)
	{
		if (psResult->droidID == id)
		{
			if (psPrevResult)
			{
				psPrevResult->next = psResult->next;
				free(psResult->sMove.asPath);
				free(psResult);
				psResult = psPrevResult->next;
			}
			else
			{
				firstResult = psResult->next;
				free(psResult->sMove.asPath);
				free(psResult);
				psResult = firstResult;
			}
		}
		else
		{
			psPrevResult = psResult;
			psResult = psResult->next;
		}
	}
	SDL_SemPost(fpathSemaphore);
}


static FPATH_RETVAL fpathRoute(MOVE_CONTROL *psMove, int id, int startX, int startY, int tX, int tY, PROPULSION_TYPE propulsionType, DROID_TYPE droidType, FPATH_MOVETYPE moveType, int owner)
{
	PATHJOB		*psJob = NULL;

	objTrace(id, "called(*,id=%d,sx=%d,sy=%d,ex=%d,ey=%d,prop=%d,type=%d,move=%d,owner=%d)", id, startX, startY, tX, tY, (int)propulsionType, (int)droidType, (int)moveType, owner);

	// don't have to do anything if already there
	if (startX == tX && startY == tY)
	{
		// return failed to stop them moving anywhere
		objTrace(id, "Tried to move nowhere");
		return FPR_FAILED;
	}

	// Check if waiting for a result
	if (psMove->Status == MOVEWAITROUTE)
	{
		PATHRESULT *psPrev = NULL, *psNext = firstResult;

		objTrace(id, "Checking if we have a path yet");
		SDL_SemWait(fpathSemaphore);

		while (psNext)
		{
			if (psNext->droidID == id && psNext->done)
			{
				FPATH_RETVAL	retval;

				ASSERT(psNext->retval != FPR_OK || psNext->sMove.asPath, "Ok result but no path in list");

				// Remove it from the result list
				if (psPrev)
				{
					psPrev->next = psNext->next;
				}
				else
				{
					firstResult = psNext->next;
				}

				// Copy over select fields - preserve others
				psMove->DestinationX = psNext->sMove.DestinationX;
				psMove->DestinationY = psNext->sMove.DestinationY;
				psMove->numPoints = psNext->sMove.numPoints;
				psMove->Position = 0;
				psMove->Status = MOVEROUTE;
				if (psMove->asPath)
				{
					free(psMove->asPath);
				}
				psMove->asPath = psNext->sMove.asPath;
				retval = psNext->retval;
				ASSERT(retval != FPR_OK || psMove->asPath, "Ok result but no path after copy");
				free(psNext);
				SDL_SemPost(fpathSemaphore);
				objTrace(id, "Got a path to (%d, %d)! Length=%d Retval=%d", (int)psMove->DestinationX,
				         (int)psMove->DestinationY, (int)psMove->numPoints, (int)retval);
				return retval;
			}
			psPrev = psNext;
			psNext = psNext->next;
		}
		SDL_SemPost(fpathSemaphore);
		objTrace(id, "No path yet. Waiting.");
		return FPR_WAIT;	// keep waiting
	}

	// We were not waiting for a result, and found no trivial path, so create new job and start waiting
	psJob = malloc(sizeof(*psJob));
	ASSERT(psJob, "Out of memory");
	if (!psJob)
	{
		return FPR_FAILED;
	}
	psJob->origX = startX;
	psJob->origY = startY;
	psJob->droidID = id;
	psJob->destX = tX;
	psJob->destY = tY;
	psJob->next = NULL;
	psJob->droidType = droidType;
	psJob->propulsion = propulsionType;
	psJob->moveType = moveType;
	psJob->owner = owner;

	// Clear any results or jobs waiting already. It is a vital assumption that there is only one
	// job or result for each droid in the system at any time.
	fpathRemoveDroidData(id);

	SDL_SemWait(fpathSemaphore);

	// Add to end of list
	if (!firstJob)
	{
		firstJob = psJob;
	}
	else
	{
		PATHJOB *psNext = firstJob;

		while (psNext->next != NULL)
		{
			psNext = psNext->next;
		}

		psNext->next = psJob;
	}

	SDL_SemPost(fpathSemaphore);

	objTrace(id, "Queued up a path-finding request to (%d, %d)", tX, tY);
	return FPR_WAIT;	// wait while polling result queue
}


// Find a route for an DROID to a location in world coordinates
FPATH_RETVAL fpathDroidRoute(DROID* psDroid, SDWORD tX, SDWORD tY)
{
	PROPULSION_STATS	*psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
	FPATH_MOVETYPE		moveType = isHumanPlayer(psDroid->player) ? FMT_MOVE : FMT_ATTACK;

	ASSERT_OR_RETURN(FPR_FAILED, psPropStats != NULL, "invalid propulsion stats pointer");
	ASSERT_OR_RETURN(FPR_FAILED, psDroid->type == OBJ_DROID, "We got passed an object that isn't a DROID!");

	if (psDroid->asWeaps[0].nStat == 0)
	{
		moveType = FMT_MOVE;	// cannot blast its way through walls, so don't even try
	}

	// check whether the end point of the route
	// is a blocking tile and find an alternative if it is
	if (psDroid->sMove.Status != MOVEWAITROUTE && fpathBlockingTile(map_coord(tX), map_coord(tY), psPropStats->propulsionType))
	{
		// find the nearest non blocking tile to the DROID
		int minDist = SDWORD_MAX;
		int nearestDir = NUM_DIR;
		int dir;

		objTrace(psDroid->id, "BLOCKED(%d,%d) - trying workaround", map_coord(tX), map_coord(tY));
		for (dir = 0; dir < NUM_DIR; dir++)
		{
			int x = map_coord(tX) + aDirOffset[dir].x;
			int y = map_coord(tY) + aDirOffset[dir].y;

			if (tileOnMap(x, y) && !fpathBlockingTile(x, y, psPropStats->propulsionType))
			{
				// pick the adjacent tile closest to our starting point
				int tileDist = fpathDistToTile(x, y, psDroid->pos.x, psDroid->pos.y);

				if (tileDist < minDist)
				{
					minDist = tileDist;
					nearestDir = dir;
				}
			}

			if (dir == NUM_BASIC - 1 && nearestDir != NUM_DIR)
			{
				break;	// found a solution without checking at greater distance
			}
		}

		if (nearestDir == NUM_DIR)
		{
			// surrounded by blocking tiles, give up
			objTrace(psDroid->id, "route to (%d, %d) failed - target blocked", map_coord(tX), map_coord(tY));
			return FPR_FAILED;
		}
		else
		{
			tX = world_coord(map_coord(tX) + aDirOffset[nearestDir].x) + TILE_SHIFT / 2;
			tY = world_coord(map_coord(tY) + aDirOffset[nearestDir].y) + TILE_SHIFT / 2;
			objTrace(psDroid->id, "Workaround found at (%d, %d)", map_coord(tX), map_coord(tY));
		}
	}
	return fpathRoute(&psDroid->sMove, psDroid->id, psDroid->pos.x, psDroid->pos.y, tX, tY, psPropStats->propulsionType, psDroid->droidType, moveType, psDroid->player);
}

// Run only from path thread
static void fpathExecute(PATHJOB *psJob, PATHRESULT *psResult)
{
	FPATH_RETVAL retval = fpathAStarRoute(&psResult->sMove, psJob);

	ASSERT(retval != ASR_OK || psResult->sMove.asPath, "Ok result but no path in result");
	switch (retval)
	{
	case ASR_NEAREST:
		objTrace(psJob->droidID, "** Nearest route **");
		psResult->retval = FPR_OK;
		break;
	case ASR_FAILED:
		objTrace(psJob->droidID, "** Failed route **");
		// Is this really a good idea? Was in original code.
		if (psJob->propulsion == PROPULSION_TYPE_LIFT && psJob->droidType != DROID_TRANSPORTER)
		{
			objTrace(psJob->droidID, "Doing fallback for non-transport VTOL");
			fpathSetMove(&psResult->sMove, psJob->destX, psJob->destY);
			psResult->retval = FPR_OK;
		}
		else
		{
			psResult->retval = FPR_FAILED;
		}
		break;
	default:
		objTrace(psJob->droidID, "Got route of length %d", psResult->sMove.numPoints);
		psResult->retval = FPR_OK;
		break;
	}
}

static FPATH_RETVAL fpathSimpleRoute(MOVE_CONTROL *psMove, int id, int startX, int startY, int tX, int tY)
{
	return fpathRoute(psMove, id, startX, startY, tX, tY, PROPULSION_TYPE_WHEELED, DROID_WEAPON, FMT_MOVE, 0);
}

void fpathTest(int x, int y, int x2, int y2)
{
	MOVE_CONTROL sMove;
	FPATH_RETVAL r;
	int i;

	// On non-debug builds prevent warnings about defining but not using fpathJobQueueLength
	(void)fpathJobQueueLength;

	/* Check initial state */
	assert(fpathThread != NULL);
	assert(fpathSemaphore != NULL);
	assert(firstJob == NULL);
	assert(firstResult == NULL);
	assert(fpathJobQueueLength() == 0);
	assert(fpathResultQueueLength() == 0);
	fpathRemoveDroidData(0);	// should not crash

	/* This should not leak memory */
	sMove.asPath = NULL;
	for (i = 0; i < 100; i++) fpathSetMove(&sMove, 1, 1);
	free(sMove.asPath);
	sMove.asPath = NULL;

	/* Test one path */
	sMove.Status = MOVEINACTIVE;
	r = fpathSimpleRoute(&sMove, 1, x, y, x2, y2);
	assert(r == FPR_WAIT);
	sMove.Status = MOVEWAITROUTE;
	assert(fpathJobQueueLength() == 1 || fpathResultQueueLength() == 1);
	fpathRemoveDroidData(2);	// should not crash, nor remove our path
	assert(fpathJobQueueLength() == 1 || fpathResultQueueLength() == 1);
	while (fpathResultQueueLength() == 0) SDL_Delay(10);
	assert(fpathJobQueueLength() == 0);
	assert(fpathResultQueueLength() == 1);
	r = fpathSimpleRoute(&sMove, 1, x, y, x2, y2);
	assert(r == FPR_OK);
	assert(sMove.numPoints > 0 && sMove.asPath);
	assert(sMove.asPath[sMove.numPoints - 1].x == map_coord(x2));
	assert(sMove.asPath[sMove.numPoints - 1].y == map_coord(y2));
	assert(fpathResultQueueLength() == 0);

	/* Let one hundred paths flower! */
	sMove.Status = MOVEINACTIVE;
	for (i = 1; i <= 100; i++)
	{
		r = fpathSimpleRoute(&sMove, i, x, y, x2, y2);
		assert(r == FPR_WAIT);
	}
	while (fpathResultQueueLength() != 100) SDL_Delay(10);
	assert(fpathJobQueueLength() == 0);
	for (i = 1; i <= 100; i++)
	{
		sMove.Status = MOVEWAITROUTE;
		r = fpathSimpleRoute(&sMove, i, x, y, x2, y2);
		assert(r == FPR_OK);
		assert(sMove.numPoints > 0 && sMove.asPath);
		assert(sMove.asPath[sMove.numPoints - 1].x == map_coord(x2));
		assert(sMove.asPath[sMove.numPoints - 1].y == map_coord(y2));
	}
	assert(fpathResultQueueLength() == 0);

	/* Kill a hundred flowers */
	sMove.Status = MOVEINACTIVE;
	for (i = 1; i <= 100; i++)
	{
		r = fpathSimpleRoute(&sMove, i, x, y, x2, y2);
		assert(r == FPR_WAIT);
	}
	for (i = 1; i <= 100; i++)
	{
		fpathRemoveDroidData(i);
	}
	assert(fpathJobQueueLength() == 0);
	assert(fpathResultQueueLength() == 0);
	assert(firstJob == NULL);
	assert(firstResult == NULL);
}

bool fpathCheck(Vector2i orig, Vector2i dest, PROPULSION_TYPE propulsion)
{
	MAPTILE *origTile = mapTile(orig.x, orig.y);
	MAPTILE *destTile = mapTile(dest.x, dest.y);

	ASSERT(propulsion != PROPULSION_TYPE_NUM, "Bad propulsion type");
	ASSERT(origTile != NULL && destTile != NULL, "Bad tile parameter");

	switch (propulsion)
	{
	case PROPULSION_TYPE_PROPELLOR:
	case PROPULSION_TYPE_WHEELED:
	case PROPULSION_TYPE_TRACKED:
	case PROPULSION_TYPE_LEGGED:
	case PROPULSION_TYPE_SKI: 	// ?!
	case PROPULSION_TYPE_HALF_TRACKED:
		return origTile->limitedContinent == destTile->limitedContinent;
	case PROPULSION_TYPE_HOVER:
		return origTile->hoverContinent == destTile->hoverContinent;
	case PROPULSION_TYPE_JUMP:
	case PROPULSION_TYPE_LIFT:
		return true;	// FIXME: This is not entirely correct for all possible maps. - Per
	case PROPULSION_TYPE_NUM:
		break;
	}
	return true;	// should never get here
}
