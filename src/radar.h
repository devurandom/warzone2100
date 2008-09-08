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
 *      @file radar.h
 *      Minimap code.
 *      @defgroup Minimap Minimap (radar) subsystem.
 *      @{
 */

#ifndef __INCLUDED_SRC_RADAR_H__
#define __INCLUDED_SRC_RADAR_H__

void radarColour(UDWORD tileNumber, uint8_t r, uint8_t g, uint8_t b);	///< Set radar colour for given terrain type.

#define MAX_RADARZOOM	2.50f
#define MIN_RADARZOOM	0.75f
#define RADARZOOM_STEP	0.25f

extern void resetRadarRedraw(void);
extern BOOL InitRadar(void);				///< Initialize minimap subsystem.
extern BOOL ShutdownRadar(void);			///< Shutdown minimap subsystem.
extern BOOL resizeRadar(void);				///< Recalculate minimap size. For initialization code only.
extern void drawRadar(void);				///< Draw the minimap on the screen.
extern void CalcRadarPosition(int mX, int mY, int *PosX, int *PosY);	///< Given a position within the radar, returns a world coordinate.
extern void SetRadarZoom(float ZoomLevel);		///< Set current zoom level. 1.0 is 1:1 resolution.
extern float GetRadarZoom(void);			///< Get current zoom level.
extern BOOL CoordInRadar(int x, int y);			///< Is screen coordinate inside minimap?

/** Different mini-map draw modes. */
typedef enum _radar_draw_mode
{
	RADAR_MODE_TERRAIN,				///< Draw terrain map
	RADAR_MODE_DEFAULT = RADAR_MODE_TERRAIN,	///< Default is terrain map
	RADAR_MODE_HEIGHT_MAP,				///< Draw height map
	RADAR_MODE_COMBINED,
	RADAR_MODE_NO_TERRAIN,				///< Only display objects
	NUM_RADAR_MODES
}RADAR_DRAW_MODE;

extern BOOL		bEnemyAllyRadarColor;		///< Enemy/ally minimap color
extern RADAR_DRAW_MODE	radarDrawMode;			///< Current minimap mode

extern void radarInitVars(void);			///< Recalculate minimap variables. For initialization code only.
extern PIELIGHT clanColours[MAX_PLAYERS];

/** @} */

#endif // __INCLUDED_SRC_RADAR_H__
