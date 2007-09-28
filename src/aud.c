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
/***************************************************************************/
/*
 * Aud.c
 *
 * Warzone audio wrapper functions
 *
 * Gareth Jones 16/12/97
 */
/***************************************************************************/

#include "lib/framework/frame.h"
#include "basedef.h"
#include "map.h"
#include "display3d.h"
#include "lib/ivis_common/piedef.h"
#include "lib/gamelib/gtime.h"

#include "cluster.h"
#include "lib/sound/aud.h"

/***************************************************************************/

extern UDWORD	mapX;
extern UDWORD	mapY;
extern iView	player;
extern UDWORD	distance;

/* Map Position of top right hand corner of the screen */
extern UDWORD	viewX;
extern UDWORD	viewY;

/***************************************************************************/

BOOL
audio_ObjectDead( void * psObj )
{
	SIMPLE_OBJECT	*psSimpleObj = (SIMPLE_OBJECT *) psObj;
	BASE_OBJECT		*psBaseObj;
	PROJECTILE		*psProj;

	/* check is valid simple object pointer */
	if ( psSimpleObj == NULL )
	{
		debug( LOG_NEVER, "audio_ObjectDead: simple object pointer invalid\n" );
		return TRUE;
	}

	/* check projectiles */
	if ( psSimpleObj->type == OBJ_PROJECTILE )
	{
		psProj = (PROJECTILE *) psSimpleObj;
		if ( psProj == NULL )
		{
			debug( LOG_NEVER, "audio_ObjectDead: projectile object pointer invalid\n" );
			return TRUE;
		}
		else
		{
			if ( psProj->state == PROJ_POSTIMPACT )
			{
				return TRUE;
			}
			else
			{
				return FALSE;
			}
		}
	}
	else
	{
		/* check base object */
		psBaseObj = (BASE_OBJECT *) psObj;

		/* check is valid pointer */
		if ( psBaseObj == NULL )
		{
			debug( LOG_NEVER, "audio_ObjectDead: base object pointer invalid\n" );
			return TRUE;
		}
		else
		{
			return psBaseObj->died;
		}
	}
}

/***************************************************************************/

void audio_Get3DPlayerPos(SDWORD *piX, SDWORD *piY, SDWORD *piZ)
{
	/* player's y and z interchanged */
	*piX = player.p.x + world_coord(visibleXTiles / 2);
	*piY = player.p.z + world_coord(visibleYTiles / 2);
	*piZ = player.p.y;

	/* invert y to match QSOUND axes */
	*piY = world_coord(GetHeightOfMap()) - *piY;
}

/***************************************************************************/
/*
 * get player direction vector - angle about vertical (y) ivis axis
 */
/***************************************************************************/

void
audio_Get3DPlayerRotAboutVerticalAxis( SDWORD *piA )
{
	*piA = player.r.y / DEG_1;
}

/***************************************************************************/
/*
 * audio_GetStaticPos
 *
 * Get QSound axial position from world (x,y)
 */
/***************************************************************************/

void audio_GetStaticPos(SDWORD iWorldX, SDWORD iWorldY, SDWORD *piX, SDWORD *piY, SDWORD *piZ)
{
	*piX = iWorldX;
	*piZ = map_TileHeight(map_coord(iWorldX), map_coord(iWorldY));
	/* invert y to match QSOUND axes */
	*piY = world_coord(GetHeightOfMap()) - iWorldY;
}

/***************************************************************************/

void audio_GetObjectPos(void *psObj, SDWORD *piX, SDWORD *piY, SDWORD *piZ)
{
	BASE_OBJECT	*psBaseObj = (BASE_OBJECT *) psObj;

	/* check is valid pointer */
	ASSERT( psBaseObj != NULL,
			"audio_GetObjectPos: game object pointer invalid\n" );

	*piX = psBaseObj->x;
	*piZ = map_TileHeight(map_coord(psBaseObj->x), map_coord(psBaseObj->y));

	/* invert y to match QSOUND axes */
	*piY = world_coord(GetHeightOfMap()) - psBaseObj->y;
}

/***************************************************************************/

UDWORD
sound_GetGameTime( void )
{
	return gameTime;
}

/***************************************************************************/
