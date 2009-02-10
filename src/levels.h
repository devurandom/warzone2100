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
 *  Control the data loading for game levels
 */

#ifndef __INCLUDED_SRC_LEVELS_H__
#define __INCLUDED_SRC_LEVELS_H__

#include "init.h"
#include "game.h"

// maximum number of WRF/WDG files

#define LEVEL_MAXFILES	9


// types of level datasets


typedef enum
{
	LDS_COMPLETE,		// all data required for a stand alone level
	LDS_CAMPAIGN,		// the data set for a campaign (no map data)
	LDS_CAMSTART,		// mapdata for the start of a campaign
	LDS_CAMCHANGE,		// data for changing between levels
	LDS_EXPAND,			// extra data for expanding a campaign map
	LDS_BETWEEN,		// pause between missions
	LDS_MKEEP,			// off map mission (extra map data)
	LDS_MCLEAR,			// off map mission (extra map data)
	LDS_EXPAND_LIMBO,   // expand campaign map using droids held in apsLimboDroids
	LDS_MKEEP_LIMBO,    // off map saving any droids (selectedPlayer) at end into apsLimboDroids
	LDS_NONE,			//flags when not got a mission to go back to or when
						//already on one - ****LEAVE AS LAST ONE****
	LDS_MULTI_TYPE_START,           ///< Start number for custom type numbers (as used by a `type` instruction)
} LEVEL_TYPE;

// the WRF/WDG files needed for a particular level
// the WRF/WDG files needed for a particular level

typedef struct _level_dataset
{
	SWORD	type;					// type of map
	SWORD	players;				// number of players for the map
	SWORD	game;					// index of WRF/WDG that loads the scenario file
	char	*pName;					// title for the level
	searchPathMode	dataDir;					// title for the level
	char	*apDataFiles[LEVEL_MAXFILES];		// the WRF/WDG files for the level
							// in load order
	struct _level_dataset *psBaseData;		// LEVEL_DATASET that must be loaded for this level to load
	struct _level_dataset *psChange;		// LEVEL_DATASET used when changing to this level from another

	struct _level_dataset *psNext;
} LEVEL_DATASET;


// the current level descriptions
extern LEVEL_DATASET	*psLevels;

// parse a level description data file
extern BOOL levParse(const char* buffer, size_t size, searchPathMode datadir);

// shutdown the level system
extern void levShutDown(void);

extern BOOL levInitialise(void);

// load up the data for a level
extern BOOL levLoadData(const char* name, char *pSaveName, GAME_TYPE saveType);

// find the level dataset
extern LEVEL_DATASET* levFindDataSet(const char* name);

// free the currently loaded dataset
extern BOOL levReleaseAll(void);

// free the data for the current mission
extern BOOL levReleaseMissionData(void);

//get the type of level currently being loaded of GTYPE type
extern SDWORD getLevelLoadType(void);

extern char *getLevelName( void );

extern void levTest(void);

#endif // __INCLUDED_SRC_LEVELS_H__
