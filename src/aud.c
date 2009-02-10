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
/**
 * @file
 *
 * Warzone audio wrapper functions.
 */

#include "lib/framework/frame.h"
#include "basedef.h"
#include "map.h"
#include "display3d.h"
#include "lib/framework/fixedpoint.h"
#include "lib/gamelib/gtime.h"

#include "lib/sound/aud.h"
#include "lib/sound/tracklib.h"

BOOL audio_ObjectDead(void * psObj)
{
	SIMPLE_OBJECT * const psSimpleObj = (SIMPLE_OBJECT *) psObj;

	/* check is valid simple object pointer */
	if (psSimpleObj == NULL)
	{
		debug( LOG_NEVER, "audio_ObjectDead: simple object pointer invalid" );
		return true;
	}

	/* check projectiles */
	if (psSimpleObj->type == OBJ_PROJECTILE)
	{
		PROJECTILE * const psProj = (PROJECTILE *) psSimpleObj;

		return (psProj->state == PROJ_POSTIMPACT);
	}
	else
	{
		/* check base object */
		BASE_OBJECT *psBaseObj  = (BASE_OBJECT *) psObj;

		return psBaseObj->died;
	}
}

Vector3f audio_GetPlayerPos(void)
{
	Vector3f pos;
	// Player's Y and Z interchanged
	// @NOTE Why?
	pos.x = player.p.x + world_coord(visibleTiles.x / 2);
	pos.y = player.p.z + world_coord(visibleTiles.x / 2);
	pos.z = player.p.y;

	// Invert Y to match QSOUND axes
	// @NOTE What is QSOUND? Why invert the Y axis?
	pos.y = world_coord(GetHeightOfMap()) - pos.y;

	return pos;
}

void audio_GetPlayerOrientation(Vector3f* forward, Vector3f* up)
{
	const Vector3f r = Vector3f_ToRadians(Vector3iPSX_To3fDegree(player.r));
	*forward = Vector3f_EulerToForwardVector(r);
	*up = Vector3f_EulerToUpVector(r);
}

/**
 * Get QSound axial position from world (x,y)
 */
void audio_GetStaticPos(SDWORD iWorldX, SDWORD iWorldY, SDWORD *piX, SDWORD *piY, SDWORD *piZ)
{
	*piX = iWorldX;
	*piZ = map_TileHeight(map_coord(iWorldX), map_coord(iWorldY));
	/* invert y to match QSOUND axes */
	*piY = world_coord(GetHeightOfMap()) - iWorldY;
}

void audio_GetObjectPos(void *psObj, SDWORD *piX, SDWORD *piY, SDWORD *piZ)
{
	BASE_OBJECT	*psBaseObj = (BASE_OBJECT *) psObj;

	/* check is valid pointer */
	ASSERT( psBaseObj != NULL,
			"audio_GetObjectPos: game object pointer invalid\n" );

	*piX = psBaseObj->pos.x;
	*piZ = map_TileHeight(map_coord(psBaseObj->pos.x), map_coord(psBaseObj->pos.y));

	/* invert y to match QSOUND axes */
	*piY = world_coord(GetHeightOfMap()) - psBaseObj->pos.y;
}

UDWORD sound_GetGameTime()
{
	return gameTime;
}
