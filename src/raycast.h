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
/** @file
 *  Raycaster functions
 */

#ifndef __INCLUDED_SRC_RAYCAST_H__
#define __INCLUDED_SRC_RAYCAST_H__

#include "lib/ivis_common/pievector.h"

#define NUM_RAYS 360

// maximum length for a visiblity ray
#define RAY_MAXLEN INT_MAX


/*!
 * The raycast intersection callback.
 * \param pos Current position
 * \param dist Current distance from start
 * \param data Payload (store intermediate results here)
 * \return true if ore points are required, false otherwise
 */
typedef bool (*RAY_CALLBACK)(Vector3i pos, int dist, void* data);


/*!
 * Cast a ray from a position into a certain direction
 * \param pos Position to cast from
 * \param dir Direction to cast into
 * \param length Maximum length
 * \param callback Callback to call for each passed tile
 * \param data Data to pass through to the callback
 */
extern void rayCast(Vector3i pos, Vector3i dir, int length, RAY_CALLBACK callback, void * data);


static inline Vector3i rayAngleToVector3i(float angle)
{
	Vector3i dest = {
		cosf(deg2radf(angle)) * INT_MAX,
		sinf(deg2radf(angle)) * INT_MAX,
		0
	};
	return dest;
}


// Calculates the maximum height and distance found along a line from any
// point to the edge of the grid
extern void	getBestPitchToEdgeOfGrid(UDWORD x, UDWORD y, UDWORD direction, SDWORD *pitch);

extern void	getPitchToHighestPoint( UDWORD x, UDWORD y, UDWORD direction,
								   UDWORD thresholdDistance, SDWORD *pitch );

#endif // __INCLUDED_SRC_RAYCAST_H__
