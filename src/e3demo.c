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
/*
	Temporary Warzone file for the E3 Demo.
	Bounces the camera around and attempts to track
	interesting stuff.
	Alex McLean, Pumpkin Studios, EIDOS Interactive.
	Will be called from a DEBUG key-mapping.
*/
// -------------------------------------------------------------------------

#include "lib/framework/frame.h"

// -------------------------------------------------------------------------

#include "lib/gamelib/gtime.h"
#include "lib/framework/fixedpoint.h"
#include "objects.h"
#include "map.h"
#include "hci.h"
#include "warcam.h"
#include "order.h"
#include "display3d.h"
#include "map.h"
#include "geometry.h"
#include "action.h"
#include "console.h"
// -------------------------------------------------------------------------
#define DC_ISACTIVE	99
#define DC_INACTIVE	101
#define	DEFAULT_DEMO_INTERVAL  (8 * GAME_TICKS_PER_SEC)
#define DROID_MOVE_INTERVAL	(GAME_TICKS_PER_SEC/4)

// -------------------------------------------------------------------------
BOOL	demoGetStatus		( void );
void	initDemoCamera		( void );
void	processDemoCam		( void );
void	toggleDemoStatus	( void );
static void findSomethingInteresting(void);
void	setFindNewTarget	( void );

// -------------------------------------------------------------------------
/* When did the camera last change it's point of interest? */
static	UDWORD	lastCameraMove;
static	UDWORD	lastDroidMove;
static	UDWORD	lastDroidSending;
static	UDWORD	presentStatus;
static	UDWORD	demoCamInterval;
static	UDWORD	droidMoveInterval;
static	DROID	*psLastDroid = NULL;
static	UDWORD	lastHeight;
static	UDWORD	lastSelectedPlayer;
static	DROID	*getDroidForDemo( UDWORD player );
// -------------------------------------------------------------------------
/* Sets up the system */
void	initDemoCamera( void )
{
	presentStatus = DC_INACTIVE; // or DC_ISACTIVE
	lastCameraMove = 0;
	lastDroidMove = 0;
	droidMoveInterval = DROID_MOVE_INTERVAL;
	demoCamInterval = DEFAULT_DEMO_INTERVAL;
	psLastDroid = NULL;
	lastHeight = 0;
}

// -------------------------------------------------------------------------
/* Updates the camera's point of interest if it's time to */
void	processDemoCam( void )
{
UDWORD	firstPlayer,otherPlayer;
DROID	*psDroid;
BOOL	bSkipOrder = false;
UDWORD	i,numWith;

	/* Is the demo camera actually active? */
	if(presentStatus == DC_INACTIVE)
	{
		/* Nope, so get out */
		return;
	}

	/* Is it time for a new target? */
	if( gameTime > (lastCameraMove + demoCamInterval) )
	{
		lastCameraMove = gameTime;
		/* The bones */
		findSomethingInteresting();
	  //	player.r.x = DEG(-90);

	}
	/* Otherwise, just send a droid off to war */
	else if(gameTime > (lastDroidSending + GAME_TICKS_PER_SEC) )
	{
			/* Check all the droid lists, looking for empty ones */
			for(i = 0,numWith = 0; i<MAX_PLAYERS; i++)
			{
				/* Keep a count of how many are empty */
				if(apsDroidLists[i])
				{
					/* We got one */
					numWith++;
				}
			}
			/* If they were all empty, then record this fact and only seek locations */
			/* We need two sides for this to work! */
			if(numWith<2)
			{
				bSkipOrder = true;
			}

			if(!bSkipOrder)
			{
				lastDroidSending = gameTime;
				/* Choose a player at random */
				firstPlayer = rand()%MAX_PLAYERS;

				/* Have they got any droids? */
				while(apsDroidLists[firstPlayer]==NULL)
				{
					/* Nope, so choose another one until we get one with droids */
					firstPlayer = rand()%MAX_PLAYERS;
				}

				/* Choose a player at random */
				otherPlayer = rand()%MAX_PLAYERS;

				/* Have they got any structures? Make sure it's not their own we're checking! */
				while(apsStructLists[otherPlayer]==NULL || otherPlayer==firstPlayer)
				{
					/* Nope, so choose another one until we get one with droids */
					otherPlayer = rand()%MAX_PLAYERS;
				}
				psDroid = getDroidForDemo(firstPlayer);


				/* Only do this if we've got a droid and an enemy building to attack! */
				if(psDroid && apsStructLists[otherPlayer])
				{
					if( (orderState(psDroid,DORDER_NONE) == true) ||
						((orderState(psDroid,DORDER_GUARD) == true) && (psDroid->action == DACTION_NONE)))
					{
						/* Make the droid attack the building - it'll indirectly route there too */
						orderDroidLoc(psDroid,DORDER_SCOUT,
						apsStructLists[otherPlayer]->pos.x, apsStructLists[otherPlayer]->pos.y);
					}
				}
			}
	}
}

// -------------------------------------------------------------------------
void	setFindNewTarget( void )
{
	lastCameraMove = 0;
}
// -------------------------------------------------------------------------
/* Flips on/off */
void	toggleDemoStatus( void )
{
	if(presentStatus == DC_ISACTIVE)
	{
		presentStatus = DC_INACTIVE;
		selectedPlayer = lastSelectedPlayer;
	}
	else if(presentStatus == DC_INACTIVE)
	{
		presentStatus = DC_ISACTIVE;
		lastSelectedPlayer = selectedPlayer;
	}
}

// -------------------------------------------------------------------------
/* Returns status */
BOOL	demoGetStatus( void )
{
	if(presentStatus == DC_ISACTIVE)
	{
		return(true);
	}
	else
	{
		return(false);
	}
}

// -------------------------------------------------------------------------
/*	Attempts to find a new location for the tracking camera to go to, or
	a new object (target) for it to track.
*/

void findSomethingInteresting()
{
	enum
	{
		SEEK_DROID,
		SEEK_TARGET,

		SEEK_LAST,

		SEEK_OVERRIDE,
	} type;

UDWORD	player,otherPlayer;
BOOL	gotNewTarget;
DROID	*psDroid;
UDWORD	numWith;
BOOL	bSeekOnlyLocations;
UDWORD	i;
BOOL	bHaveHuman = false;
PROPULSION_STATS	*psPropStats;

//---


//----

	/* There may be droids, so don't rule it out */
	bSeekOnlyLocations = false;
	/* Check all the droid lists, looking for empty ones */
	for(i = 0,numWith = 0; i<MAX_PLAYERS; i++)
	{
		/* Keep a count of how many are empty */
		if(apsDroidLists[i])
		{
			/* We got one */
			numWith++;
			if(i<MAX_PLAYERS-2)
			{
				bHaveHuman = true;
			}
		}
	}
	/* If they were all empty, then record this fact and only seek locations */
	/* We need two sides for this to work! */
	if(numWith<2 || !bHaveHuman)
	{
		bSeekOnlyLocations = true;
	}

	/* We haven't yet got a droid if we're looking for one...*/
	gotNewTarget = false;

	/* Keep going until we get one */
//	while(!gotNewTarget)
//	{
		/* Are we only to seek locations? */
		if(bSeekOnlyLocations)
		{
			/* Then force the switch outcome - hacky I know, but same as if else in code */
			type = SEEK_OVERRIDE;
		}
		else
		{
			/* We're off hunting droids */
			type = rand() % SEEK_LAST;
		}
		/* Check which */
		switch (type)
		{
			/* Go after a droid, or a droid location */
		case SEEK_DROID:
		case SEEK_TARGET:
			/* Choose a player at random */
			player = rand()%MAX_PLAYERS;

			/* Have they got any droids? */
			while(apsDroidLists[player]==NULL)
			{
				/* Nope, so choose another one until we get one with droids */
				player = rand()%MAX_PLAYERS;
			}

			/* Choose a player at random */
			otherPlayer = rand()%MAX_PLAYERS;

			/* Have they got any structures? Make sure it's not their own we're checking! */
			while(apsStructLists[otherPlayer]==NULL || otherPlayer==player)
			{
				/* Nope, so choose another one until we get one with droids */
				otherPlayer = rand()%MAX_PLAYERS;
			}

			/* If there was a droid last time, deselect it */
			if(psLastDroid && !psLastDroid->died)
			{
				psLastDroid->selected = false;
			}

			/* Jump to droid and track */
			psDroid = getDroidForDemo(player);
			/* Only do if we've got a droid and an enemy building to attack */
			if(psDroid && apsStructLists[otherPlayer])
			{
				psDroid->selected = true;
			  	selectedPlayer = player;
				psLastDroid = psDroid;
			  //	if(orderState(psDroid,DORDER_ATTACK) == false)
			  //	{
		 	 		orderDroidLoc(psDroid,DORDER_MOVE,
					apsStructLists[otherPlayer]->pos.x, apsStructLists[otherPlayer]->pos.y);
			  //	}

				if(!getWarCamStatus())
				{
					/* Start the tracking */
					camToggleStatus();
				}
				else
				{
					camToggleStatus();
					processWarCam();
					camToggleStatus();
				}
				psPropStats = asPropulsionStats + psDroid->asBits[COMP_PROPULSION].nStat;
				if ( psPropStats->propulsionType == PROPULSION_TYPE_LIFT )
				{
					/* Track vtols for longer */
					demoCamInterval = 3*DEFAULT_DEMO_INTERVAL;
				}
				else
				{
					demoCamInterval = DEFAULT_DEMO_INTERVAL;
				}
			}
			break;
			/* Go to a new location cos there's no droids left in the world....ahhhhhhh*/
		case SEEK_OVERRIDE:
			requestRadarTrack((16 + rand()%(mapWidth-31))*TILE_UNITS, (16 + rand()%(mapHeight-31)) * TILE_UNITS );
			gotNewTarget = true;
			break;
		default:
			break;
		}
//	}
}

// -------------------------------------------------------------------------
DROID	*getDroidForDemo( UDWORD player )
{
UDWORD	i;
DROID	*psDroid;
UDWORD	droidIndex;

	for(psDroid = apsDroidLists[player],i=0; psDroid; psDroid = psDroid->psNext,i++)
	{
		/* Count how many droids they have */
	}

	if(i)
	{
		droidIndex = rand()%i;
		for(psDroid = apsDroidLists[player],i=0; psDroid && i<droidIndex; i++,psDroid = psDroid->psNext)
		{
			/* Find the right one */
		}
		return(psDroid);
	}
	else
	{
		return(NULL);
	}
}
