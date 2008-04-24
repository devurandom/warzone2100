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
 *  Definitions for the display system structures and routines.
 */

#ifndef __INCLUDED_SRC_DISPLAY_H__
#define __INCLUDED_SRC_DISPLAY_H__

#include "basedef.h"
#include "structure.h"

/* Initialise the display system */
extern BOOL dispInitialise(void);

extern Uint16 mouseXPos, mouseYPos;

extern void shakeStart(void);
extern void shakeStop(void);

extern void ProcessRadarInput(void);

extern void processInput(void);
/*don't want to do any of these whilst in the Intelligence Screen*/
extern void processMouseClickInput(void);

extern void	scroll(void);

extern BOOL DrawnInLastFrame(SDWORD Frame);

// Clear all selections.
extern void clearSel(void);
// Clear all selections and stop driver mode.
extern void clearSelection(void);
// deal with selecting a droid
extern void dealWithDroidSelect(DROID *psDroid, BOOL bDragBox);

extern	void	setInvertMouseStatus( BOOL val );
extern BOOL	getInvertMouseStatus( void );

extern	void	setDrawShadows( BOOL val );
extern BOOL	getDrawShadows( void );

extern	BOOL	getRadarJumpStatus( void );
extern	void	setRadarJump(BOOL	val);


/* Do the 3D display */
extern void displayWorld(void);

// Illumination value for standard light level "as the artist drew it" ... not darker, not lighter

#define MAX_SCROLL_SPEED (800+scroll_speed_accel)	// make max speed dependant on accel chosen.

extern UDWORD scroll_speed_accel;			// now user modifyable.


#define DRAG_INACTIVE 0
#define DRAG_DRAGGING 1
#define DRAG_RELEASED 2
#define DRAG_PLACING  3

#define BOX_PULSE_SPEED	50

struct	_dragBox
{
UDWORD	x1;
UDWORD	y1;
UDWORD	x2;
UDWORD	y2;
UDWORD	status;
UDWORD	lastTime;
UDWORD	pulse;
};

extern struct	_dragBox dragBox3D,wallDrag;

typedef enum _pointer
{
MP_ATTACH = 99,
MP_ATTACK,
MP_BRIDGE,
MP_BUILD,
MP_EMBARK,
MP_FIX,
MP_GUARD,
MP_JAM,
MP_MOVE,
MP_PICKUP,
MP_REPAIR,
MP_SELECT,
MP_LOCKON,
MP_MENSELECT,
MP_BOMB
} MOUSE_POINTER;

typedef enum _selectionTypes
{
SC_DROID_CONSTRUCT,
SC_DROID_DIRECT,
SC_DROID_INDIRECT,
SC_DROID_CLOSE,
SC_DROID_SENSOR,
SC_DROID_ECM,
SC_DROID_BRIDGE,
SC_DROID_RECOVERY,
SC_DROID_COMMAND,
SC_DROID_BOMBER,
SC_DROID_TRANSPORTER,
SC_DROID_DEMOLISH,
SC_DROID_REPAIR,
SC_INVALID,

} SELECTION_TYPE;

typedef enum _targets
{
MT_TERRAIN,
MT_RESOURCE,
MT_BLOCKING,
MT_RIVER,
MT_TRENCH,
MT_OWNSTRDAM,
MT_OWNSTROK,
MT_OWNSTRINCOMP,
MT_REPAIR,
MT_REPAIRDAM,
MT_ENEMYSTR,
MT_TRANDROID,
MT_OWNDROID,
MT_OWNDROIDDAM,
MT_ENEMYDROID,
MT_COMMAND,
MT_ARTIFACT,
MT_DAMFEATURE,
MT_SENSOR,
MT_WRECKFEATURE,
MT_CONSTRUCT,
MT_SENSORSTRUCT,
MT_SENSORSTRUCTDAM,

MT_NOTARGET		//leave as last one
} MOUSE_TARGET;

extern BOOL		gameStats;
extern BOOL		godMode;
extern float		RadarZoomLevel;

// reset the input state
void resetInput(void);

BOOL CheckInScrollLimits(SDWORD *xPos,SDWORD *zPos);
extern BOOL CheckScrollLimits(void);
extern BOOL	rotActive;
extern int gammaValue;

BASE_OBJECT	*mouseTarget( void );

BOOL StartObjectOrbit(BASE_OBJECT *psObj);
void CancelObjectOrbit(void);

extern void FinishDeliveryPosition(UDWORD xPos,UDWORD yPos,void *UserData);
extern void CancelDeliveryRepos(void);
extern void StartDeliveryPosition( OBJECT_POSITION *psLocation );
extern BOOL DeliveryReposValid(void);

extern void StartTacticalScrollObj(BOOL driveActive,BASE_OBJECT *psObj);
extern void CancelTacticalScroll(void);
extern void MoveTacticalScroll(SDWORD xVel,SDWORD yVel);
extern BOOL	getRotActive( void );
extern SDWORD	getDesiredPitch( void );
extern void	setDesiredPitch(SDWORD pitch);

#define MAX_PLAYER_X_ANGLE	(-1)
#define MIN_PLAYER_X_ANGLE	(-60)

#define MAXDISTANCE	(3500)
#define MINDISTANCE	(500)
#define START_DISTANCE	(2000)
#define START_HEIGHT (1500)

#define CAMERA_PIVOT_HEIGHT (500)

#define INITIAL_DESIRED_PITCH (-40)
#define INITIAL_STARTING_PITCH (-40)
#define INITIAL_DESIRED_ROTATION (-45)

#define	HIDDEN_FRONTEND_WIDTH	(640)
#define	HIDDEN_FRONTEND_HEIGHT	(480)

//access function for bSensorAssigned variable
extern void setSensorAssigned(void);
extern void	setShakeStatus( BOOL val );
extern BOOL	getShakeStatus( void );

extern void	displayInitVars(void);

void AddDerrickBurningMessage(void);

// check whether the queue order keys are pressed
extern BOOL ctrlShiftDown(void);

extern UDWORD getTargetType(void);

#endif // __INCLUDED_SRC_DISPLAY_H__
