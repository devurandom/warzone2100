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
/** @file
 *  Interface to the routing functions
 */

#ifndef __INCLUDED_SRC_FPATH_H__
#define __INCLUDED_SRC_FPATH_H__

#include "droiddef.h"

/** Return values for routing
 *
 *  @ingroup pathfinding
 *  @{
 */

typedef enum _fpath_movetype
{
	FMT_MOVE,		///< Move around all obstacles
	FMT_ATTACK,		///< Assume that we will destroy enemy obstacles
} FPATH_MOVETYPE;

typedef struct _jobNode
{
	PROPULSION_TYPE	propulsion;
	DROID_TYPE	droidType;
	int		destX, destY;
	int		origX, origY;
	UDWORD		droidID;
	struct _jobNode	*next;
	FPATH_MOVETYPE	moveType;
	int		owner;		///< Player owner
} PATHJOB;

typedef enum _fpath_retval
{
	FPR_OK,         ///< found a route
	FPR_FAILED,     ///< failed to find a route
	FPR_WAIT,       ///< route is being calculated by the path-finding thread
} FPATH_RETVAL;

/** Initialise the path-finding module.
 */
extern BOOL fpathInitialise(void);

/** Shutdown the path-finding module.
 */
extern void fpathShutdown(void);

extern void fpathUpdate(void);

/** Find a route for a droid to a location.
 */
extern FPATH_RETVAL fpathDroidRoute(DROID* psDroid, SDWORD targetX, SDWORD targetY);

/** Function pointer to the currently in-use blocking tile check function.
 *  
 *  This function will check if the map tile at the given location blocks droids
 *  with the currently selected propulsion type.
 *
 *  @return true if the given tile is blocking for this droid
 */
BOOL fpathBlockingTile(SDWORD x, SDWORD y, PROPULSION_TYPE propulsion);
BOOL fpathBaseBlockingTile(SDWORD x, SDWORD y, PROPULSION_TYPE propulsion, int player, FPATH_MOVETYPE moveType);

/** Set a direct path to position.
 *
 *  Plan a path from @c psDroid's current position to given position without
 *  taking obstructions into consideration.
 *
 *  Used for instance by VTOLs. Function is thread-safe.
 */
extern void fpathSetDirectRoute(DROID* psDroid, SDWORD targetX, SDWORD targetY);

/** Clean up path jobs and results for a droid. Function is thread-safe. */
extern void fpathRemoveDroidData(int id);

/** Quick O(1) test of whether it is theoretically possible to go from origin to destination
 *  using the given propulsion type. */
bool fpathCheck(Vector2i orig, Vector2i dest, PROPULSION_TYPE propulsion);

/** Unit testing. */
void fpathTest(int x, int y, int x2, int y2);

/** @} */

#endif // __INCLUDED_SRC_FPATH_H__
