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
#ifndef _warcam_h
/* Prevent multiple inclusion */
#define _warcam_h

#include "lib/ivis_common/pietypes.h"

#define X_UPDATE 0x1
#define Y_UPDATE 0x2
#define Z_UPDATE 0x4

#define CAM_X_ONLY	X_UPDATE
#define CAM_Y_ONLY	Y_UPDATE
#define CAM_Z_ONLY	Z_UPDATE

#define CAM_X_AND_Y	(X_UPDATE + Y_UPDATE)
#define CAM_X_AND_Z	(X_UPDATE + Z_UPDATE)
#define CAM_Y_AND_Z	(Y_UPDATE + Z_UPDATE)

#define CAM_ALL	(X_UPDATE + Y_UPDATE + Z_UPDATE)


#define ACCEL_CONSTANT			FRACTCONST(64,10)	//((float)6.4)
#define VELOCITY_CONSTANT		FRACTCONST(4,1)		//((float)4.0)
#define ROT_ACCEL_CONSTANT		FRACTCONST(4,1)		//((float)5.0)
#define ROT_VELOCITY_CONSTANT	FRACTCONST(4,1)		//((float)4.0)

#define CAM_X_SHIFT	((VISIBLE_XTILES/2)*128)
#define CAM_Z_SHIFT	((VISIBLE_YTILES/2)*128)

/* The different tracking states */
enum
{
CAM_INACTIVE,
CAM_REQUEST,
CAM_TRACKING,
CAM_RESET,
CAM_TRACK_OBJECT,
CAM_TRACK_LOCATION
};

// We define and use this struct instead of iVector because iVector is 32 bit on pc
// but only 16 bit on Playstation and we definitly need 32 bits for the war camera stuff.
typedef struct {
	int32 x,y,z;
} iVector32;

/* Storage for old viewnagles etc */
typedef struct _warcam
{
UDWORD	status;
UDWORD	trackClass;
UDWORD	lastUpdate;
iView	oldView;

PIEVECTORF	acceleration;
PIEVECTORF	velocity;
PIEVECTORF	position;

PIEVECTORF	rotation;
PIEVECTORF	rotVel;
PIEVECTORF	rotAccel;

UDWORD	oldDistance;
BASE_OBJECT *target;
}WARCAM;

/* Externally referenced functions */
extern void	initWarCam			( void );
extern void	setWarCamActive		( BOOL status );
extern BOOL	getWarCamStatus		( void );
extern void camToggleStatus		( void );
extern void camSetOldView(int x,int y,int z,int rx,int ry,int dist);
extern BOOL processWarCam		( void );
extern void	camToggleInfo		( void );
extern void	requestRadarTrack	( SDWORD x, SDWORD y );
extern BOOL	getRadarTrackingStatus( void );
extern void	dispWarCamLogo		( void );
extern void	toggleRadarAllignment( void );
extern void	camInformOfRotation			( iVector *rotation );
extern BASE_OBJECT *camFindDroidTarget(void);
extern DROID *getTrackingDroid( void );
extern SDWORD	getPresAngle( void );
extern UDWORD	getNumDroidsSelected( void );
extern void	camAllignWithTarget(BASE_OBJECT *psTarget);

extern FRACT accelConstant,velocityConstant, rotAccelConstant, rotVelocityConstant;
extern	UDWORD	getTestAngle(void);
void	updateTestAngle( void );
#define BEHIND_DROID_DIRECTION(d)	  (360-((d->direction+180)%360))





#endif
