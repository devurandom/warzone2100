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
/* WarCAM - Handles tracking/following of in game objects */
/* Alex McLean, Pumpkin Studios, EIDOS Interactive, 1998 */
/*	23rd September, 1998 - This code is now so hideously complex
	and unreadable that's it's inadvisable to attempt changing
	how the camera works, since I'm not sure that I'll be able to even
	get it working the way it used to, should anything get broken.
	I really hope that no further changes are needed here...:-(
	Alex M. */
#include <stdio.h>
#include "lib/framework/frame.h"
#include "lib/framework/trig.h"

#include "lib/ivis_opengl/piematrix.h"
#include "lib/ivis_common/piedef.h" //ivis matrix code
#include "objects.h"
#include "warcam.h"
#include "display.h"
#include "display3d.h"
#include "hci.h"
#include "console.h"
#include "lib/gamelib/gtime.h"
#include "effects.h"
#include "map.h"
#include "geometry.h"
#include "oprint.h"
#include "miscimd.h"
#include "loop.h"
#include "drive.h"
#include "move.h"
#include "order.h"
#include "action.h"
#include "intdisplay.h"
#include "e3demo.h"
#include "raycast.h"
#include "display3d.h"
#include "selection.h"

#define MODFRACT(value,mod) \
	while((value) < 0)	{ (value) += (mod); } \
	while((value) > (mod)) { (value) -= (mod); }

#define MIN_TRACK_HEIGHT 16

/* Holds all the details of our camera */
static	WARCAM	trackingCamera;

/* Present rotation for the 3d camera logo */
static	SDWORD	warCamLogoRotation;

/* The fake target that we track when jumping to a new location on the radar */
static	BASE_OBJECT	radarTarget;

/* Do we trun to face when doing a radar jump? */
static	BOOL	bRadarAllign;

static SDWORD	presAvAngle = 0;;

/*	These are the DEFAULT offsets that make us track _behind_ a droid and allow
	it to be pretty far _down_ the screen, so we can see more
*/

/* Offset from droid's world coords */
/* How far we track relative to the droids location - direction matters */
#define	CAM_DEFAULT_X_OFFSET	-400
#define CAM_DEFAULT_Y_OFFSET	-400
#define	MINCAMROTX	-20


/* How much info do you want when tracking a droid - this toggles full stat info */
static	BOOL bFullInfo = FALSE;

/* Are we requesting a new track to start that is a radar (location) track? */
static	BOOL bRadarTrackingRequested = FALSE;

/* World coordinates for a radar track/jump */
static  float	 radarX,radarY;

/*	Where we were up to (pos and rot) last update - allows us to see whether
	we are sufficently near our target to disable further tracking */
static	Vector3i	oldPosition, oldRotation;

/* The fraction of a second that the last game frame took */
static	float	fraction;

static BOOL OldViewValid;

//-----------------------------------------------------------------------------------
/* Sets the camera to inactive to begin with */
void	initWarCam( void )
{
	/* We're not intitially following anything */
	trackingCamera.status = CAM_INACTIVE;

	/* Logo setup */
	warCamLogoRotation = 0;

	OldViewValid = FALSE;
}


/* Static function that switches off tracking - and might not be desirable? - Jim?*/
static void camSwitchOff( void )
{
 	/* Restore the angles */
//	player.r.x = trackingCamera.oldView.r.x;
	player.r.z = trackingCamera.oldView.r.z;

	/* And height */
	/* Is this desirable??? */
//	player.p.y = trackingCamera.oldView.p.y;

	/* Restore distance */
	setViewDistance(trackingCamera.oldDistance);
}


#define	LEADER_LEFT			1
#define	LEADER_RIGHT		2
#define	LEADER_UP			3
#define	LEADER_DOWN			4
#define LEADER_STATIC		5

static void processLeaderSelection( void )
{
	DROID *psDroid;
	DROID *psPresent;
	DROID *psNew = NULL;
	UDWORD leaderClass;
	BOOL bSuccess;
	UDWORD dif;
	UDWORD bestSoFar;

	if (demoGetStatus())
	{
		return;
	}

	if (getWarCamStatus())
	{
		/* Only do if we're tracking a droid */
		if (trackingCamera.target->type != OBJ_DROID)
		{
			return;
		}
	}
	else
	{
		return;
	}

	/* Don't do if we're driving?! */
	if (getDrivingStatus())
	{
		return;
	}

	psPresent = (DROID*)trackingCamera.target;

	if (keyPressed(KEY_LEFTARROW))
	{
		leaderClass = LEADER_LEFT;
	}

	else if (keyPressed(KEY_RIGHTARROW))
	{
		leaderClass = LEADER_RIGHT;
	}

	else if (keyPressed(KEY_UPARROW))
	{
		leaderClass = LEADER_UP;
	}

	else if (keyPressed(KEY_DOWNARROW))
	{
		leaderClass = LEADER_DOWN;
	}
	else
	{
		leaderClass = LEADER_STATIC;
	}

	bSuccess = FALSE;
	bestSoFar = UDWORD_MAX;

	switch (leaderClass)
	{
	case	LEADER_LEFT:
		for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			/* Is it even on the sscreen? */
			if (DrawnInLastFrame(psDroid->sDisplay.frameNumber) && psDroid->selected && psDroid != psPresent)
			{
				if (psDroid->sDisplay.screenX < psPresent->sDisplay.screenX)
				{
					dif = psPresent->sDisplay.screenX - psDroid->sDisplay.screenX;
					if (dif < bestSoFar)
					{
						bestSoFar = dif;
						bSuccess = TRUE;
						psNew = psDroid;
					}
				}
			}
		}
		break;
	case	LEADER_RIGHT:
		for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			/* Is it even on the sscreen? */
			if (DrawnInLastFrame(psDroid->sDisplay.frameNumber) && psDroid->selected && psDroid != psPresent)
			{
				if (psDroid->sDisplay.screenX > psPresent->sDisplay.screenX)
				{
					dif = psDroid->sDisplay.screenX - psPresent->sDisplay.screenX;
					if (dif < bestSoFar)
					{
						bestSoFar = dif;
						bSuccess = TRUE;
						psNew = psDroid;
					}
				}
			}
		}
		break;
	case	LEADER_UP:
		for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			/* Is it even on the sscreen? */
			if (DrawnInLastFrame(psDroid->sDisplay.frameNumber) && psDroid->selected && psDroid != psPresent)
			{
				if (psDroid->sDisplay.screenY < psPresent->sDisplay.screenY)
				{
					dif = psPresent->sDisplay.screenY - psDroid->sDisplay.screenY;
					if (dif < bestSoFar)
					{
						bestSoFar = dif;
						bSuccess = TRUE;
						psNew = psDroid;
					}
				}
			}
		}
		break;
	case	LEADER_DOWN:
		for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			/* Is it even on the sscreen? */
			if (DrawnInLastFrame(psDroid->sDisplay.frameNumber) && psDroid->selected && psDroid != psPresent)
			{
				if (psDroid->sDisplay.screenY > psPresent->sDisplay.screenY)
				{
					dif = psDroid->sDisplay.screenY - psPresent->sDisplay.screenY;
					if (dif < bestSoFar)
					{
						bestSoFar = dif;
						bSuccess = TRUE;
						psNew = psDroid;
					}
				}
			}
		}
		break;
	case	LEADER_STATIC:
		break;
	}
	if (bSuccess)
	{
		camAllignWithTarget((BASE_OBJECT*)psNew);
	}
}


/* Sets up the dummy target for the camera */
static void setUpRadarTarget(SDWORD x, SDWORD y)
{
	radarTarget.pos.x = x;
	radarTarget.pos.y = y;

	if ((x < 0) || (y < 0) || (x > (SDWORD)((mapWidth - 1) * TILE_UNITS))
	    || (y > (SDWORD)((mapHeight - 1) * TILE_UNITS)))
	{
		radarTarget.pos.z = 128 * ELEVATION_SCALE;
	}
	else
	{
		radarTarget.pos.z = map_Height(x,y);
	}
	radarTarget.direction = calcDirection(player.p.x, player.p.z, x, y);
	radarTarget.pitch = 0;
	radarTarget.roll = 0;
	radarTarget.type = OBJ_TARGET;
	radarTarget.died = 0;
}


/* Attempts to find the target for the camera to track */
static BASE_OBJECT *camFindTarget(void)
{
	/*	See if we can find a selected droid. If there's more than one
		droid selected for the present player, then we track the oldest
		one. */

	if (bRadarTrackingRequested)
	{
		setUpRadarTarget(radarX, radarY);
		bRadarTrackingRequested = FALSE;
		return(&radarTarget);
	}

	return camFindDroidTarget();
}


BOOL camTrackCamera(void);

/* Updates the camera position/angle along with the object movement */
BOOL	processWarCam( void )
{
BASE_OBJECT	*foundTarget;
BOOL Status = TRUE;

	/* Get out if the camera isn't active */
	if(trackingCamera.status == CAM_INACTIVE)
	{
		return(TRUE);
	}

	/* Calculate fraction of a second for last game frame */
	fraction = (MAKEFRACT(frameTime2) / MAKEFRACT(GAME_TICKS_PER_SEC));

	/* Ensure that the camera only ever flips state within this routine! */
	switch(trackingCamera.status)
	{
	case CAM_REQUEST:

			/* See if we can find the target to follow */
			foundTarget = camFindTarget();

			if(foundTarget && !foundTarget->died)
			{
				/* We've got one, so store away info */
				camAllignWithTarget(foundTarget);
				/* We're now into tracking status */
				trackingCamera.status = CAM_TRACKING;
				/* Inform via console */
				if(foundTarget->type == OBJ_DROID)
				{
					if(!getWarCamStatus())
					{
						CONPRINTF(ConsoleString,(ConsoleString,"WZ/CAM  - %s",droidGetName((DROID*)foundTarget)));
					}
				}
				else
				{
//					CONPRINTF(ConsoleString,(ConsoleString,"DROID-CAM V0.1 Enabled - Now tracking new location"));
				}
			}
			else
			{
				/* We've requested a track with no droid selected */
//				addConsoleMessage("Droid-CAM V0.1 ERROR - No targets(s) selected",DEFAULT_JUSTIFY);
				trackingCamera.status = CAM_INACTIVE;
			}
		break;

	case CAM_TRACKING:
			/* Track the droid unless routine comes back false */
			if(!camTrackCamera())
			{
				/*
					Camera track came back false, either because droid died or is
					no longer selected, so reset to old values
				*/
				foundTarget = camFindTarget();
				if(foundTarget && !foundTarget->died)
				{
					trackingCamera.status = CAM_REQUEST;
				}
				else
				{
					trackingCamera.status = CAM_RESET;
				}
			}

		processLeaderSelection();

		break;
	case CAM_RESET:
			/* Reset camera to pre-droid tracking status */
			if( (trackingCamera.target==NULL)
			  ||(trackingCamera.target->type!=OBJ_TARGET))
			{
				camSwitchOff();
			}
			/* Switch to inactive mode */
			trackingCamera.status = CAM_INACTIVE;
//			addConsoleMessage("Droid-CAM V0.1 Disabled",DEFAULT_JUSTIFY);
			Status = FALSE;
		break;
	default:
		debug( LOG_ERROR, "Weirdy status for tracking Camera" );
		abort();
		break;
	}

	return Status;
}

//-----------------------------------------------------------------------------------

/* Flips states for camera active */
void	setWarCamActive(BOOL status)
{
	debug( LOG_NEVER, "setWarCamActive(%d)\n", status );

	/* We're trying to switch it on */
	if(status == TRUE)
	{
		/* If it's not inactive then it's already in use - so return */
		/* We're tracking a droid */
		if(trackingCamera.status!=CAM_INACTIVE)
		{
			if(bRadarTrackingRequested)
			{
				trackingCamera.status = CAM_REQUEST;
			}
			else
			{
				return;
			}
		}
		else
		{
			/* Otherwise request the camera to track */
			trackingCamera.status = CAM_REQUEST;
		}
	}
	else
		/* We trying to switch off */
	{
		/* Is it already off? */
		if(trackingCamera.status == CAM_INACTIVE)
		{
			return;
		}
		else
		{
			/* Attempt to set to normal */
			trackingCamera.status = CAM_RESET;
		}
	}
}

//-----------------------------------------------------------------------------------

BASE_OBJECT	*camFindDroidTarget(void)
{
	DROID	*psDroid;

	for(psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
	{

		if(psDroid->selected)

		{
			/* Return the first one found */
			return( (BASE_OBJECT*)psDroid);
		}
	}

	/* We didn't find one */
	return(NULL);
}


//-----------------------------------------------------------------------------------

/* Stores away old viewangle info and sets up new distance and angles */
void	camAllignWithTarget(BASE_OBJECT *psTarget)
{
	/* Store away the target */
	trackingCamera.target = psTarget;

	/* Save away all the view angles */
	trackingCamera.oldView.r.x = trackingCamera.rotation.x = MAKEFRACT(player.r.x);
	trackingCamera.oldView.r.y = trackingCamera.rotation.y = MAKEFRACT(player.r.y);
	trackingCamera.oldView.r.z = trackingCamera.rotation.z = MAKEFRACT(player.r.z);

	/* Store away the old positions and set the start position too */
	trackingCamera.oldView.p.x = trackingCamera.position.x = MAKEFRACT(player.p.x);
	trackingCamera.oldView.p.y = trackingCamera.position.y = MAKEFRACT(player.p.y);
	trackingCamera.oldView.p.z = trackingCamera.position.z = MAKEFRACT(player.p.z);

   //	trackingCamera.rotation.x = player.r.x = DEG(-90);
	/* No initial velocity for moving */
	trackingCamera.velocity.x = trackingCamera.velocity.y = trackingCamera.velocity.z = MAKEFRACT(0);
	/* Nor for rotation */
	trackingCamera.rotVel.x = trackingCamera.rotVel.y = trackingCamera.rotVel.z = MAKEFRACT(0);
	/* No initial acceleration for moving */
	trackingCamera.acceleration.x = trackingCamera.acceleration.y = trackingCamera.acceleration.z =MAKEFRACT(0);
	/* Nor for rotation */
	trackingCamera.rotAccel.x = trackingCamera.rotAccel.y = trackingCamera.rotAccel.z = MAKEFRACT(0);

	/* Sote the old distance */
	trackingCamera.oldDistance = getViewDistance();	//distance;

	/* Store away when we started */
	trackingCamera.lastUpdate = gameTime2;


	OldViewValid = TRUE;
}


//-----------------------------------------------------------------------------------
static SDWORD getAverageTrackAngle( BOOL bCheckOnScreen )
{
	DROID *psDroid;
	float xShift, yShift;
	float xTotal = 0.0, yTotal = 0.0;
	float averageAngleFloat = 0;
	SDWORD droidCount = 0, averageAngle = 0;
	SDWORD retVal;

	/* Got thru' all droids */
	for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
	{
		/* Is he worth selecting? */
		if (psDroid->selected)
		{
			if (bCheckOnScreen ? droidOnScreen(psDroid, pie_GetVideoBufferWidth() / 6) : TRUE)
			{
					droidCount++;
					averageAngle += psDroid->direction;
					xShift = trigSin(psDroid->direction);
					yShift = trigCos(psDroid->direction);
					xTotal += xShift;
					yTotal += yShift;
			}
		}
	}
	if (droidCount)
	{
		retVal = (averageAngle / droidCount);
		averageAngleFloat = (float)RAD_TO_DEG(atan2(xTotal, yTotal));
	}
	else
	{
		retVal = 0;
	}
	// FIXME: Should we return 0 when retVal is 0?
	presAvAngle = MAKEINT(averageAngleFloat);//retVal;
	return presAvAngle;
}


//-----------------------------------------------------------------------------------
static SDWORD getGroupAverageTrackAngle(UDWORD groupNumber, BOOL bCheckOnScreen)
{
	DROID *psDroid;
	float xShift, yShift;
	float xTotal = 0.0, yTotal = 0.0;
	float averageAngleFloat = 0;
	SDWORD droidCount = 0, averageAngle = 0;
	SDWORD retVal;

	/* Got thru' all droids */
	for (psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
	{
		/* Is he worth considering? */
		if (psDroid->group == groupNumber)
		{
			if (bCheckOnScreen ? droidOnScreen(psDroid, pie_GetVideoBufferWidth() / 6) : TRUE)
			{
					droidCount++;
					averageAngle += psDroid->direction;
					xShift = trigSin(psDroid->direction);
					yShift = trigCos(psDroid->direction);
					xTotal += xShift;
					yTotal += yShift;
			}
		}
	}
	if (droidCount)
	{
		retVal = (averageAngle / droidCount);
		averageAngleFloat = RAD_TO_DEG(atan2(xTotal, yTotal));
	}
	else
	{
		retVal = 0;
	}
	// FIXME: Return 0 when retVal is 0?
	presAvAngle = MAKEINT(averageAngleFloat);//retVal;
	return presAvAngle;
}


//-----------------------------------------------------------------------------------
static void getTrackingConcerns(SDWORD *x, SDWORD *y, SDWORD *z)
{
	SDWORD xTotals = 0, yTotals = 0, zTotals = 0;
	DROID *psDroid;
	UDWORD count;

	for (count = 0, psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			if (psDroid->selected)
			{
				if (droidOnScreen(psDroid, pie_GetVideoBufferWidth() / 4))
				{
					count++;
					xTotals += psDroid->pos.x;
					yTotals += psDroid->pos.z;	// note the flip
					zTotals += psDroid->pos.y;
				}
			}
		}

	if (count)	// necessary!!!!!!!
	{
		*x = xTotals / count;
		*y = yTotals / count;
		*z = zTotals / count;
	}
}


//-----------------------------------------------------------------------------------
static void getGroupTrackingConcerns(SDWORD *x, SDWORD *y, SDWORD *z, UDWORD groupNumber, BOOL bOnScreen)
{
	SDWORD xTotals = 0, yTotals = 0, zTotals = 0;
	DROID *psDroid;
	UDWORD count;

	for (count = 0, psDroid = apsDroidLists[selectedPlayer]; psDroid; psDroid = psDroid->psNext)
		{
			if (psDroid->group == groupNumber)
			{
				if (bOnScreen ? droidOnScreen(psDroid, pie_GetVideoBufferWidth() / 4) : TRUE)
				{
				 		count++;
						xTotals += psDroid->pos.x;
						yTotals += psDroid->pos.z;	// note the flip
						zTotals += psDroid->pos.y;
				}
			}
		}

	if (count)	// necessary!!!!!!!
	{
		*x = xTotals / count;
		*y = yTotals / count;
		*z = zTotals / count;
	}
}


//-----------------------------------------------------------------------------------
							/* How this all works */
/*
Each frame we calculate the new acceleration, velocity and positions for the location
and rotation of the camera. The velocity is obviously based on the acceleration and this
in turn is based on the separation between the two objects. This separation is distance
in the case of location and degrees of arc in the case of rotation.

  Each frame:-

  ACCELERATION	-	A
  VELOCITY		-	V
  POSITION		-	P
  Location of camera	(x1,y1)
  Location of droid		(x2,y2)
  Separation(distance) = D. This is the distance between (x1,y1) and (x2,y2)

  A = c1D - c2V		Where c1 and c2 are two constants to be found (by experiment)
  V = V + A(frameTime/GAME_TICKS_PER_SEC)
  P = P + V(frameTime/GAME_TICKS_PER_SEC)

  Things are the same for the rotation except that D is then the difference in angles
  between the way the camera and droid being tracked are facing. AND.... the two
  constants c1 and c2 will be different as we're dealing with entirely different scales
  and units. Separation in terms of distance could be in the thousands whereas degrees
  cannot exceed 180.

  This all works because acceleration is based on how far apart they are minus some factor
  times the camera's present velocity. This minus factor is what slows it down when the
  separation gets very small. Without this, it would continually oscillate about it's target
  point. The four constants (two each for rotation and position) need to be found
  by trial and error since the metrics of time,space and rotation are entirely warzone
  specific.

  And that's all folks.
*/

//-----------------------------------------------------------------------------------

static void updateCameraAcceleration(UBYTE update)
{
float	separation;
SDWORD	realPos;
SDWORD	xConcern,yConcern,zConcern;
SDWORD	xBehind,yBehind;
BOOL	bFlying;
DROID	*psDroid;
UDWORD	multiAngle;
PROPULSION_STATS	*psPropStats;
//SDWORD	pitch;
SDWORD	angle;

	angle = abs(((player.r.x/182)%90));
	angle = 90-angle;

	bFlying = FALSE;
	if(trackingCamera.target->type == OBJ_DROID)
	{
		psDroid = (DROID*)trackingCamera.target;
		psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
		if(psPropStats->propulsionType == LIFT)
		{
			bFlying = TRUE;
		}
	}
	/*	This is where we check what it is we're tracking.
		Were we to track a building or location - this is
		where it'd be set up */

	/*	If we're tracking a droid, then we nned to track slightly in front
		of it in order that the droid appears down the screen a bit. This means
		that we need to find an offset point from it relative to it's present
		direction
	*/
	if(trackingCamera.target->type == OBJ_DROID)
	{
		/* Present direction is important */
		if(getNumDroidsSelected()>2)
		{
			if(trackingCamera.target->selected)
			{
				multiAngle = getAverageTrackAngle(TRUE);
			}
			else
			{
				multiAngle = getGroupAverageTrackAngle( trackingCamera.target->group, TRUE );
			}
			xBehind = ( ( CAM_DEFAULT_Y_OFFSET * SIN( DEG(multiAngle) ) ) >> FP12_SHIFT );
			yBehind = ( ( CAM_DEFAULT_X_OFFSET * COS( DEG(multiAngle) ) ) >> FP12_SHIFT );
		}
		else
		{
		 	xBehind = ( ( CAM_DEFAULT_Y_OFFSET * SIN( DEG( (int)trackingCamera.target->direction ) ) ) >> FP12_SHIFT );
			yBehind = ( ( CAM_DEFAULT_X_OFFSET * COS( DEG( (int)trackingCamera.target->direction ) ) ) >> FP12_SHIFT );
		}
	}
	else
	{
		/* Irrelevant for normal radar tracking */
		xBehind = 0;
		yBehind = 0;
	}

	/*	Get these new coordinates */
	if(getNumDroidsSelected()>2 && trackingCamera.target->type == OBJ_DROID)
	{
	 	xConcern = trackingCamera.target->pos.x;		  // nb - still NEED to be set
		yConcern = trackingCamera.target->pos.z;
		zConcern = trackingCamera.target->pos.y;
		if(trackingCamera.target->selected)
		{
			getTrackingConcerns(&xConcern,&yConcern,&zConcern);
		}
		else
		{
			getGroupTrackingConcerns(&xConcern,&yConcern,&zConcern,trackingCamera.target->group,TRUE);
		}
//		getBestPitchToEdgeOfGrid(xConcern,zConcern,360-((getAverageTrackAngle(TRUE)+180)%360),&pitch);
		yConcern+=angle*5;

	}
	else
	{
		xConcern = trackingCamera.target->pos.x;
		yConcern = trackingCamera.target->pos.z;
		zConcern = trackingCamera.target->pos.y;
	}

	if(trackingCamera.target->type == OBJ_DROID && getNumDroidsSelected()<=2)
	{
//		getBestPitchToEdgeOfGrid(trackingCamera.target->pos.x,trackingCamera.target->pos.z,
//			360-((trackingCamera.target->direction+180)%360),&pitch);
		yConcern+=angle*5;

	}


	if(update & X_UPDATE)
	{
		/* Need to update acceleration along x axis */
		realPos = xConcern - (CAM_X_SHIFT) - xBehind;
		separation = (float)(realPos - trackingCamera.position.x);
		if(!bFlying)
		{
		 	trackingCamera.acceleration.x =
				(ACCEL_CONSTANT*separation - VELOCITY_CONSTANT*(float)trackingCamera.velocity.x);
		}
		else
		{
			trackingCamera.acceleration.x =
				((ACCEL_CONSTANT*separation*4) - (VELOCITY_CONSTANT*2*(float)trackingCamera.velocity.x));

		}
	}

	if(update & Y_UPDATE)
	{
//		flushConsoleMessages();
//		CONPRINTF(ConsoleString,(ConsoleString,"Attempted height : %d",yConcern));

		/* Need to update acceleration along y axis */
		realPos = (yConcern);
		separation = (float)(realPos - trackingCamera.position.y);
		if(bFlying) separation = separation/2;
//		CONPRINTF(ConsoleString,(ConsoleString,"Separation : %f",separation));
//		CONPRINTF(ConsoleString,(ConsoleString,"Distance : %d",distance));
		if(!bFlying)
		{
		 	trackingCamera.acceleration.y =
				((ACCEL_CONSTANT)*separation - (VELOCITY_CONSTANT)*trackingCamera.velocity.y);
		}
		else
		{
			trackingCamera.acceleration.y =
				(((ACCEL_CONSTANT)*separation*4) - ((VELOCITY_CONSTANT)*2*trackingCamera.velocity.y));
		}
	}

	if(update & Z_UPDATE)
	{
		/* Need to update acceleration along z axis */
		realPos = zConcern - (CAM_Z_SHIFT) - yBehind;
		separation = (float)(realPos - trackingCamera.position.z);
		if(!bFlying)
		{
			trackingCamera.acceleration.z =
				(ACCEL_CONSTANT*separation - VELOCITY_CONSTANT*trackingCamera.velocity.z);
		}
		else
		{
			trackingCamera.acceleration.z =
				((ACCEL_CONSTANT*separation*4) - (VELOCITY_CONSTANT*2*trackingCamera.velocity.z));

		}
	}
}

//-----------------------------------------------------------------------------------

static void updateCameraVelocity( UBYTE update )
{
float	fraction;

	/*	Get the time fraction of a second - the next two lines are present in 4
		of the next six functions. All 4 of these functions are called every frame, so
		it may be an idea to calculate these higher up and store them in a static but
		I've left them in for clarity for now */

	fraction = (MAKEFRACT(frameTime2) / (float)GAME_TICKS_PER_SEC);

	if(update & X_UPDATE)
	{
		trackingCamera.velocity.x += (trackingCamera.acceleration.x * fraction);
	}

	if(update & Y_UPDATE)
	{
		trackingCamera.velocity.y += (trackingCamera.acceleration.y * fraction);
	}

	if(update & Z_UPDATE)
	{
		trackingCamera.velocity.z += (trackingCamera.acceleration.z * fraction);
	}
}

//-----------------------------------------------------------------------------------

static void	updateCameraPosition(UBYTE update)
{
BOOL	bFlying;
float	fraction;
DROID	*psDroid;
PROPULSION_STATS	*psPropStats;

	bFlying = FALSE;
	if(trackingCamera.target->type == OBJ_DROID)
	{
		psDroid = (DROID*)trackingCamera.target;
		psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
		if(psPropStats->propulsionType == LIFT)
		{
			bFlying = TRUE;
		}
	}
	/* See above */
	fraction = (MAKEFRACT(frameTime2) / (float)GAME_TICKS_PER_SEC);

	if(update & X_UPDATE)
	{
		/* Need to update position along x axis */
		trackingCamera.position.x += (trackingCamera.velocity.x * fraction);
	}

	if(update & Y_UPDATE)
	{
			/* Need to update position along y axis */
			trackingCamera.position.y +=(trackingCamera.velocity.y * fraction);
	}

	if(update & Z_UPDATE)
	{
		/* Need to update position along z axis */
		trackingCamera.position.z += (trackingCamera.velocity.z * fraction);
	}
}

//-----------------------------------------------------------------------------------
/* Calculate the acceleration that the camera spins around at */
static void updateCameraRotationAcceleration( UBYTE update )
{
	SDWORD	worldAngle;
	float	separation;
	SDWORD	xConcern, yConcern, zConcern;
	BOOL	bTooLow;
	PROPULSION_STATS *psPropStats;
	SDWORD	pitch;
	BOOL	bGotFlying = FALSE;
	SDWORD	xPos = 0, yPos = 0, zPos = 0;

	bTooLow = FALSE;
	if(trackingCamera.target->type == OBJ_DROID)
	{
		DROID *psDroid = (DROID*)trackingCamera.target;
		psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
		if(psPropStats->propulsionType == LIFT)
		{
			UDWORD	droidHeight, difHeight, droidMapHeight;

			bGotFlying = TRUE;
			droidHeight = psDroid->pos.z;
			droidMapHeight = map_Height(psDroid->pos.x, psDroid->pos.y);
			difHeight = abs(droidHeight - droidMapHeight);
			if(difHeight < MIN_TRACK_HEIGHT)
			{
				bTooLow = TRUE;
			}
		}
	}

	if(update & Y_UPDATE)
	{
		/* Presently only y rotation being calculated - but same idea for other axes */
		/* Check what we're tracking */
		if(getNumDroidsSelected()>2 && trackingCamera.target->type == OBJ_DROID)
		{
			if(trackingCamera.target->selected)
			{
				yConcern = DEG( getAverageTrackAngle(FALSE) ); //DEG(trackingCamera.target->direction);
			}
			else
			{
				yConcern = DEG( getGroupAverageTrackAngle(trackingCamera.target->group, FALSE) ); //DEG(trackingCamera.target->direction);
			}
		}
		else
		{
			yConcern = DEG( (int)trackingCamera.target->direction );
		}
		yConcern += DEG(180);

  		while(trackingCamera.rotation.y < 0)
		{
			trackingCamera.rotation.y += DEG(360);
		}

		/* Which way are we facing? */
		worldAngle =  trackingCamera.rotation.y;
		separation = (float) ((yConcern - worldAngle));
		if(separation < DEG(-180))
		{
			separation += DEG(360);
		}
		else if(separation > DEG(180))
		{
			separation -= DEG(360);
		}

		/* Make new acceleration */
		trackingCamera.rotAccel.y = ROT_ACCEL_CONSTANT * separation - ROT_VELOCITY_CONSTANT * trackingCamera.rotVel.y;
	}

	if(update & X_UPDATE)
	{
		if(trackingCamera.target->type == OBJ_DROID && !bGotFlying)
		{
			getTrackingConcerns(&xPos,&yPos,&zPos);
			if(trackingCamera.target->selected)
			{
				getBestPitchToEdgeOfGrid(xPos,zPos,360-((getAverageTrackAngle(TRUE)+180)%360),&pitch);
			}
			else
			{
				getBestPitchToEdgeOfGrid(xPos,zPos,360-((getGroupAverageTrackAngle(trackingCamera.target->group,TRUE)+180)%360),&pitch);
			}
			if(pitch<14) pitch = 14;
			xConcern = DEG(-pitch);
		}
		else
		{
			xConcern = DEG(trackingCamera.target->pitch);
			xConcern += DEG(-16);
		}

		//xConcern = DEG(trackingCamera.target->pitch);
	   //	if(xConcern>DEG(MINCAMROTX))
	   //	{
	   //		xConcern = DEG(MINCAMROTX);
	   //	}
		while(trackingCamera.rotation.x<0)
			{
				trackingCamera.rotation.x+=DEG(360);
			}
		worldAngle =  trackingCamera.rotation.x;
		separation = (float) ((xConcern - worldAngle));

		MODFRACT(separation,DEG(360));

		if(separation<DEG(-180))
		{
			separation+=DEG(360);
		}
		else if(separation>DEG(180))
		{
			separation-=DEG(360);
		}

		/* Make new acceleration */
		trackingCamera.rotAccel.x =
			/* Make this really slow */
			((ROT_ACCEL_CONSTANT)*separation - ROT_VELOCITY_CONSTANT*(float)trackingCamera.rotVel.x);
	}

	/* This looks a bit arse - looks like a flight sim */
	if(update & Z_UPDATE)
	{
		if(bTooLow)
		{
			zConcern = 0;
		}
		else
		{
			zConcern = DEG(trackingCamera.target->roll);
		}
		while(trackingCamera.rotation.z<0)
			{
				trackingCamera.rotation.z+=DEG(360);
			}
		worldAngle =  trackingCamera.rotation.z;
		separation = (float) ((zConcern - worldAngle));
		if(separation<DEG(-180))
		{
			separation+=DEG(360);
		}
		else if(separation>DEG(180))
		{
			separation-=DEG(360);
		}

		/* Make new acceleration */
		trackingCamera.rotAccel.z =
			/* Make this really slow */
			((ROT_ACCEL_CONSTANT/1)*separation - ROT_VELOCITY_CONSTANT*(float)trackingCamera.rotVel.z);
	}

}

//-----------------------------------------------------------------------------------
/*	Calculate the velocity that the camera spins around at - just add previously
	calculated acceleration */
static void updateCameraRotationVelocity( UBYTE update )
{
float	fraction;

	fraction = (MAKEFRACT(frameTime2) / (float)GAME_TICKS_PER_SEC);

	if(update & Y_UPDATE)
	{
		trackingCamera.rotVel.y += ((float)trackingCamera.rotAccel.y * fraction);
	}
	if(update & X_UPDATE)
	{
		trackingCamera.rotVel.x += ((float)trackingCamera.rotAccel.x * fraction);
	}
	if(update & Z_UPDATE)
	{
		trackingCamera.rotVel.z += ((float)trackingCamera.rotAccel.z * fraction);
	}

}

//-----------------------------------------------------------------------------------
/* Move the camera around by adding the velocity */
static void updateCameraRotationPosition( UBYTE update )
{
float	fraction;

	fraction = (MAKEFRACT(frameTime2) / (float)GAME_TICKS_PER_SEC);

 	if(update & Y_UPDATE)
	{
		trackingCamera.rotation.y += (trackingCamera.rotVel.y * fraction);
	}
	if(update & X_UPDATE)
	{
		trackingCamera.rotation.x += (trackingCamera.rotVel.x * fraction);
	}
	if(update & Z_UPDATE)
	{
		trackingCamera.rotation.z += (trackingCamera.rotVel.z * fraction);
	}
}

static BOOL nearEnough(void)
{
BOOL	retVal = FALSE;
SDWORD	xPos;
SDWORD	yPos;

	xPos = player.p.x + (mapWidth * TILE_UNITS) / 2;
	yPos = player.p.z + (mapHeight * TILE_UNITS) / 2;

	if( (abs(xPos-trackingCamera.target->pos.x) <= 256) &&
		(abs(yPos-trackingCamera.target->pos.y) <= 256) )
		{
			retVal = TRUE;
		}
	return(retVal);
}


/* Returns how far away we are from our goal in a radar track */
static UDWORD getPositionMagnitude( void )
{
	Vector3i dif;
	UDWORD val;

	dif.x = abs(player.p.x - oldPosition.x);
	dif.y = abs(player.p.y - oldPosition.y);
	dif.z = abs(player.p.z - oldPosition.z);
	val = (dif.x * dif.x) + (dif.y * dif.y) + (dif.z * dif.z);
	return val;
}


static UDWORD getRotationMagnitude( void )
{
	Vector3i dif;
	UDWORD val;

	dif.x = abs(player.r.x - oldRotation.x);
	dif.y = abs(player.r.y - oldRotation.y);
	dif.z = abs(player.r.z - oldRotation.z);
	val = (dif.x * dif.x) + (dif.y * dif.y) + (dif.z * dif.z);
	return val;
}


/* Returns how far away we are from our goal in rotation */
/* Updates the viewpoint according to the object being tracked */
BOOL	camTrackCamera( void )
{
PROPULSION_STATS	*psPropStats;
DROID	*psDroid;
BOOL	bFlying;

	bFlying = FALSE;

	/* Most importantly - see if the target we're tracking is dead! */
	if(trackingCamera.target->died)
	{
		setFindNewTarget();
		return(FALSE);
	}

	/*	Cancel tracking if it's no longer selected.
		This may not be desirable? 	*/
   	if(trackingCamera.target->type == OBJ_DROID)
	{

//		if(!trackingCamera.target->selected)
//		{
//			return(FALSE);
//		}
	}

	/* Update the acceleration,velocity and position of the camera for movement */
	updateCameraAcceleration(CAM_ALL);
	updateCameraVelocity(CAM_ALL);
	updateCameraPosition(CAM_ALL);

	/* Update the acceleration,velocity and rotation of the camera for rotation */
	/*	You can track roll as well (z axis) but it makes you ill and looks
		like a flight sim, so for now just pitch and orientation */


	if(trackingCamera.target->type == OBJ_DROID)
	{
		psDroid = (DROID*)trackingCamera.target;
		psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
		if (psPropStats->propulsionType == LIFT)
		{
				bFlying = TRUE;
		}
	}
/*
	bIsBuilding = FALSE;
	if(trackingCamera.target->type == OBJ_DROID)
	{
		psDroid= (DROID*)trackingCamera.target;
		if(DroidIsBuilding(psDroid))
		{
			bIsBuilding = TRUE;
		}
	}
*/


	if(bRadarAllign || trackingCamera.target->type == OBJ_DROID)
	{
		if(bFlying)
		{
			updateCameraRotationAcceleration(CAM_ALL);
		}
		else
		{
			updateCameraRotationAcceleration(CAM_X_AND_Y);
		}
	}
	if(bFlying)
	{
	 	updateCameraRotationVelocity(CAM_ALL);
		updateCameraRotationPosition(CAM_ALL);
	}
	/*
	else if(bIsBuilding)
	{
		updateCameraRotationVelocity(CAM_X_ONLY);
	}
	*/
	else
	{
		updateCameraRotationVelocity(CAM_X_AND_Y);
		updateCameraRotationPosition(CAM_X_AND_Y);
	}

	/* Record the old positions for comparison */
	oldPosition.x = player.p.x;
	oldPosition.y = player.p.y;
	oldPosition.z = player.p.z;

	/* Update the position that's now stored in trackingCamera.position */
	player.p.x = trackingCamera.position.x;
	player.p.y = trackingCamera.position.y;
	player.p.z = trackingCamera.position.z;

	/* Record the old positions for comparison */
	oldRotation.x = player.r.x;
	oldRotation.y = player.r.y;
	oldRotation.z = player.r.z;

	/* Update the rotations that're now stored in trackingCamera.rotation */
	player.r.x = trackingCamera.rotation.x;
	/*if(!bIsBuilding)*/
	player.r.y = trackingCamera.rotation.y;
	player.r.z = trackingCamera.rotation.z;

	/* There's a minimum for this - especially when John's VTOL code lets them land vertically on cliffs */
	if(player.r.x>DEG(360+MAX_PLAYER_X_ANGLE))
  	{
   		player.r.x = DEG(360+MAX_PLAYER_X_ANGLE);
   	}

	/*
	if(bIsBuilding)
	{
		player.r.y+=DEG(1);
	}
	*/
	/* Clip the position to the edge of the map */
	CheckScrollLimits();

	/* Store away our last update as acceleration and velocity are all fn()/dt */
	trackingCamera.lastUpdate = gameTime2;
	if(bFullInfo)
	{
		flushConsoleMessages();
		if(trackingCamera.target->type == OBJ_DROID)
		{
			printDroidInfo((DROID*)trackingCamera.target);
		}
	}

	/* Switch off if we're jumping to a new location and we've got there */
	if(getRadarTrackingStatus())
	{
		/*	This will ensure we come to a rest and terminate the tracking
			routine once we're close enough
		*/
		if(getRotationMagnitude()<10000)
		{
			if(nearEnough() && getPositionMagnitude() < 60)
			{
				camToggleStatus();
			}
		}
	}
	return(TRUE);
}
//-----------------------------------------------------------------------------------
DROID *getTrackingDroid( void )
{
	if(!getWarCamStatus()) return(NULL);
	if(trackingCamera.status != CAM_TRACKING) return(NULL);
	if(trackingCamera.target->type != OBJ_DROID) return(NULL);
	return((DROID*)trackingCamera.target);
}

//-----------------------------------------------------------------------------------
SDWORD	getPresAngle( void )
{
	return(presAvAngle);
}
//-----------------------------------------------------------------------------------


//-----------------------------------------------------------------------------------
UDWORD	getNumDroidsSelected( void )
{
	return(selNumSelected(selectedPlayer));
}

//-----------------------------------------------------------------------------------

/* Returns whether or not the tracking camera is active */
BOOL	getWarCamStatus( void )
{
	/* Is it switched off? */
	if(trackingCamera.status == CAM_INACTIVE)
	{
		return(FALSE);
	}
	else
	{
		/* Tracking is ON */
		return(TRUE);
	}
}

//-----------------------------------------------------------------------------------

/* Flips the status of tracking to the opposite of what it presently is */
void	camToggleStatus( void )
{
 	/* If it's off */
	if(trackingCamera.status == CAM_INACTIVE)
	{
		/* Switch it on */
		setWarCamActive(TRUE);
	}
	else
	{
		/* Otherwise, switch it off */
		setWarCamActive(FALSE);
//		if(getDrivingStatus())
//		{
//			StopDriverMode();
//		}
	}
}


/*	Flips on/off whether we print out full info about the droid being tracked.
	If ON then this info is permanent on screen and realtime updating */
void	camToggleInfo(void)
{
	bFullInfo = !bFullInfo;
}

/* Informs the tracking camera that we want to start tracking to a new radar target */
void	requestRadarTrack(SDWORD x, SDWORD y)
{
	radarX = (SWORD)x;
 	radarY = (SWORD)y;
 	bRadarTrackingRequested = TRUE;
	trackingCamera.status = CAM_REQUEST;
	processWarCam();
// 	setWarCamActive(TRUE);
}

/* Returns whether we're presently tracking to a new _location_ */
BOOL	getRadarTrackingStatus( void )
{
BOOL	retVal;

	if(trackingCamera.status == CAM_INACTIVE)
	{
		retVal = FALSE;
	}
	else
	{
		//if(/*trackingCamera.target && */trackingCamera.target->type == OBJ_TARGET)
        //if you know why the above check was commented out please tell me AB 19/11/98
        if(trackingCamera.target && trackingCamera.target->type == OBJ_TARGET)
		{
			retVal = TRUE;
		}
		else
		{
			retVal = FALSE;
		}
	}
	return(retVal);
}

void	toggleRadarAllignment( void )
{
	bRadarAllign = !bRadarAllign;
}

void	camInformOfRotation( Vector3i *rotation )
{
	trackingCamera.rotation.x = rotation->x;
	trackingCamera.rotation.y = rotation->y;
	trackingCamera.rotation.z = rotation->z;
}
