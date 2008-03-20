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
/* display3d.h */

#ifndef _display3d_h
#define _display3d_h

#include "display.h"
#include "display3ddef.h"	// This should be the only place including this file
#include "lib/ivis_common/pietypes.h"
#include "lib/ivis_common/piedef.h"
#include "objectdef.h"
#include "message.h"


/*!
 * Special tile types
 */
typedef enum
{
	RIVERBED_TILE = 5, //! Underwater ground
	WATER_TILE = 17, //! Water surface
	RUBBLE_TILE = 54, //! You can drive over these
	BLOCKING_RUBBLE_TILE = 67 //! You cannot drive over these
} TILE_ID;

extern bool showFPS;
extern bool showSAMPLES;

extern void	setViewAngle(SDWORD angle);
extern UDWORD getViewDistance(void);
extern void	setViewDistance(UDWORD dist);
extern BOOL	radarOnScreen;
extern bool rangeOnScreen; // Added to get sensor/gun range on screen.  -Q 5-10-05
extern void	scaleMatrix( UDWORD percent );
extern void setViewPos( UDWORD x, UDWORD y, BOOL Pan);
extern void getPlayerPos(SDWORD *px, SDWORD *py);
extern void setPlayerPos(SDWORD x, SDWORD y);
extern void disp3d_setView(iView *newView);
extern void disp3d_resetView();
extern void disp3d_getView(iView *newView);

extern void draw3DScene (void);
extern void renderDroid					( DROID *psDroid );
extern void renderStructure				( STRUCTURE *psStructure );
extern void renderFeature				( FEATURE *psFeature );
extern void renderProximityMsg			( PROXIMITY_DISPLAY	*psProxDisp);
extern void renderProjectile			( PROJECTILE *psCurr);
extern void renderAnimComponent			( const COMPONENT_OBJECT *psObj );
extern void renderDeliveryPoint			( FLAG_POSITION *psPosition );
extern void debugToggleSensorDisplay	( void );

extern void displayFeatures( void );
extern void displayStaticObjects( void );
extern void displayDynamicObjects( void );
extern void displayProximityMsgs( void );
extern void displayDelivPoints(void);
extern void calcScreenCoords(DROID *psDroid);
extern void toggleEnergyBars( void );

extern BOOL doWeDrawRadarBlips( void );
extern BOOL doWeDrawProximitys( void );
extern void setBlipDraw(BOOL val);
extern void setProximityDraw(BOOL val);
extern void renderShadow( DROID *psDroid, iIMDShape *psShadowIMD );

extern PIELIGHT getTileColour(int x, int y);
extern void setTileColour(int x, int y, PIELIGHT colour);

extern UDWORD getSuggestedPitch( void );

extern BOOL	clipXY ( SDWORD x, SDWORD y);

extern BOOL init3DView(void);
extern void initViewPosition(void);
extern iView player;
extern UDWORD distance;
extern BOOL selectAttempt;
extern BOOL draggingTile;
extern iIMDShape *g_imd;
extern BOOL	droidSelected;
extern UDWORD terrainMidX,terrainMidY;

extern SDWORD scrollSpeed;
//extern void	assignSensorTarget( DROID *psDroid );
extern void assignSensorTarget( BASE_OBJECT *psObj );
extern void assignDestTarget( void );
extern UDWORD getWaterTileNum( void);
extern void setUnderwaterTile(UDWORD num);
extern UDWORD getRubbleTileNum( void );
extern void setRubbleTile(UDWORD num);

extern SDWORD	getCentreX( void );
extern SDWORD	getCentreZ( void );

extern SDWORD mouseTileX, mouseTileY;

extern BOOL bRender3DOnly;
extern BOOL showGateways;
extern BOOL showPath;
extern Vector2i visibleTiles;

/*returns the graphic ID for a droid rank*/
extern UDWORD  getDroidRankGraphic(DROID *psDroid);

/* Visualize radius at position */
extern void showRangeAtPos(SDWORD centerX, SDWORD centerY, SDWORD radius);

#define	BASE_MUZZLE_FLASH_DURATION	(GAME_TICKS_PER_SEC/10)
#define	EFFECT_MUZZLE_ADDITIVE		128

#define BAR_FULL	0
#define BAR_BASIC	1
#define BAR_DOT		2
#define BAR_NONE	3

extern UWORD barMode;
extern UDWORD geoOffset;

enum
{
	BAR_SELECTED,
	BAR_DROIDS,
	BAR_DROIDS_AND_STRUCTURES,
	BAR_LAST
};

#endif
