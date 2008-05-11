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
/*
 * Levels.c
 *
 * Control the data loading for game levels
 *
 */

#include <ctype.h>
#include <string.h>

// levLoadData printf's
#define DEBUG_GROUP0
#include "lib/framework/frame.h"
#include "lib/framework/frameresource.h"
#include "lib/framework/listmacs.h"
#include "init.h"
#include "objects.h"
#include "hci.h"
#include "levels.h"
#include "mission.h"
#include "levelint.h"
#include "game.h"
#include "lighting.h"
#include "lib/ivis_common/piestate.h"
#include "data.h"
#include "lib/ivis_common/ivi.h"
#include "lib/script/script.h"
#include "scripttabs.h"
#include "research.h"

// block ID number start for the current level data (as opposed to a dataset)
#define CURRENT_DATAID		LEVEL_MAXFILES

static	char	currentLevelName[32];

// the current level descriptions
LEVEL_DATASET	*psLevels;

// the currently loaded data set
static LEVEL_DATASET	*psBaseData;
static LEVEL_DATASET	*psCurrLevel;

// dummy level data for single WRF loads
static LEVEL_DATASET	sSingleWRF = { 0, 0, 0, 0, 0, { 0 }, 0, 0, 0 };

static SDWORD levelLoadType;

// initialise the level system
BOOL levInitialise(void)
{
	psLevels = NULL;
	psBaseData = NULL;
	psCurrLevel = NULL;

	return true;
}

SDWORD getLevelLoadType(void)
{
	return levelLoadType;
}

// shutdown the level system
void levShutDown(void)
{
	while (psLevels != NULL)
	{
		unsigned int i;
		LEVEL_DATASET * const toDelete = psLevels;

		psLevels = psLevels->psNext;

		for (i = 0; i < ARRAY_SIZE(toDelete->apDataFiles[i]); ++i)
		{
			if (toDelete->apDataFiles[i] != NULL)
			{
				free(toDelete->apDataFiles[i]);
			}
		}

		free(toDelete->pName);
		free(toDelete);
	}
}

/** Find a level dataset with the given name.
 *  @param name the name of the dataset to search for.
 *  @return a dataset with associated with the given @c name, or NULL if none
 *          could be found.
 */
LEVEL_DATASET* levFindDataSet(const char* name)
{
	LEVEL_DATASET* psNewLevel;

	for (psNewLevel = psLevels; psNewLevel; psNewLevel = psNewLevel->psNext)
	{
		if (psNewLevel->pName != NULL
		 && strcmp(psNewLevel->pName, name) == 0)
		{
			return psNewLevel;
		}
	}

	return NULL;
}

// free the data for the current mission
BOOL levReleaseMissionData(void)
{
	SDWORD i;

	// release old data if any was loaded
	if (psCurrLevel != NULL)
	{
		if (!stageThreeShutDown())
		{
			return false;
		}

		// free up the old data
		for(i=LEVEL_MAXFILES-1; i >= 0; i--)
		{
			if (i == psCurrLevel->game)
			{
				if (psCurrLevel->psBaseData == NULL)
				{
					if (!stageTwoShutDown())
					{
						return false;
					}
				}
			}
			else// if (psCurrLevel->apDataFiles[i])
			{

				resReleaseBlockData(i + CURRENT_DATAID);
			}
		}
	}
	return true;
}


// free the currently loaded dataset
BOOL levReleaseAll(void)
{
	SDWORD i;

	// release old data if any was loaded
	if (psCurrLevel != NULL)
	{
		if (!levReleaseMissionData())
		{
			return false;
		}

		// release the game data
		if (psCurrLevel->psBaseData != NULL)
		{
			if (!stageTwoShutDown())
			{
				return false;
			}
		}


		if (psCurrLevel->psBaseData)
		{
			for(i=LEVEL_MAXFILES-1; i >= 0; i--)
			{
				if (psCurrLevel->psBaseData->apDataFiles[i])
				{
					resReleaseBlockData(i);
				}
			}
		}

		if (!stageOneShutDown())
		{
			return false;
		}
	}

	psCurrLevel=NULL;

	return true;
}

// load up a single wrf file
static BOOL levLoadSingleWRF(const char* name)
{
	// free the old data
	levReleaseAll();

	// create the dummy level data
	if (sSingleWRF.pName)
	{
		free(sSingleWRF.pName);
	}

	memset(&sSingleWRF, 0, sizeof(LEVEL_DATASET));
	sSingleWRF.pName = strdup(name);

	// load up the WRF
	if (!stageOneInitialise())
	{
		return false;
	}

	// load the data
	debug(LOG_WZ, "Loading %s ...", name);
	if (!resLoad(name, 0))
	{
		return false;
	}

	if (!stageThreeInitialise())
	{
		return false;
	}

	psCurrLevel = &sSingleWRF;

	return true;
}


char *getLevelName( void )
{
	return(currentLevelName);
}


// load up the data for a level
BOOL levLoadData(const char* name, char *pSaveName, SDWORD saveType)
{
	LEVEL_DATASET	*psNewLevel, *psBaseData, *psChangeLevel;
	SDWORD			i;
	BOOL            bCamChangeSaveGame;

	debug(LOG_WZ, "Loading level %s (%s, type %d)", name, pSaveName, (int)saveType);
	if (saveType == GTYPE_SAVE_START || saveType == GTYPE_SAVE_MIDMISSION)
	{
		if (!levReleaseAll())
		{
			debug(LOG_ERROR, "Failed to unload old data");
			return false;
		}
	}

	levelLoadType = saveType;

	// find the level dataset
	psNewLevel = levFindDataSet(name);
	if (psNewLevel == NULL)
	{
		debug(LOG_WZ, "Dataset %s not found - trying to load as WRF", name);
		return levLoadSingleWRF(name);
	}
	debug(LOG_WZ, "** Data set found is %s type %d", psNewLevel->pName, (int)psNewLevel->type);

	/* Keep a copy of the present level name */
	strlcpy(currentLevelName, name, sizeof(currentLevelName));

	bCamChangeSaveGame = false;
	if (pSaveName && saveType == GTYPE_SAVE_START)
	{
		if (psNewLevel->psChange != NULL)
		{
			bCamChangeSaveGame = true;
			debug(LOG_WZ, "** CAMCHANGE FOUND");
		}
	}

	// select the change dataset if there is one
	psChangeLevel = NULL;
	if (((psNewLevel->psChange != NULL) && (psCurrLevel != NULL)) || bCamChangeSaveGame)
	{
		//store the level name
		debug(LOG_WZ, "Found CAMCHANGE dataset");
		psChangeLevel = psNewLevel;
		psNewLevel = psNewLevel->psChange;
	}

	// ensure the correct dataset is loaded
	if (psNewLevel->type == LDS_CAMPAIGN)
	{
		debug(LOG_ERROR, "Cannot load a campaign dataset (%s)", psNewLevel->pName);
		return false;
	}
	else
	{
		if (psCurrLevel != NULL)
		{
			if ((psCurrLevel->psBaseData != psNewLevel->psBaseData) ||
				(psCurrLevel->type < LDS_NONE && psNewLevel->type  >= LDS_NONE) ||
				(psCurrLevel->type >= LDS_NONE && psNewLevel->type  < LDS_NONE))
			{
				// there is a dataset loaded but it isn't the correct one
				debug(LOG_WZ, "Incorrect base dataset loaded - levReleaseAll()");
				levReleaseAll();	// this sets psCurrLevel to NULL
			}
			else
			{
				debug(LOG_WZ, "Correct base dataset already loaded.");
			}
		}

		// setup the correct dataset to load if necessary
		if (psCurrLevel == NULL)
		{
			if (psNewLevel->psBaseData != NULL)
			{
				debug(LOG_WZ, "Setting base dataset to load: %s", psNewLevel->psBaseData->pName);
			}
			psBaseData = psNewLevel->psBaseData;
		}
		else
		{
			debug(LOG_WZ, "No base dataset to load");
			psBaseData = NULL;
		}
	}

	rebuildSearchPath(psNewLevel->dataDir, false);

	// reset the old mission data if necessary
	if (psCurrLevel != NULL)
	{
		debug(LOG_WZ, "Reseting old mission data");
		if (!levReleaseMissionData())
		{
			return false;
		}
	}

	// need to free the current map and droids etc for a save game
	if ((psBaseData == NULL) &&
		(pSaveName != NULL))
	{
		if (!saveGameReset())
		{
			return false;
		}
	}

	// initialise if necessary
	if (psNewLevel->type == LDS_COMPLETE || //psNewLevel->type >= LDS_MULTI_TYPE_START ||
		psBaseData != NULL)
	{
		debug(LOG_WZ, "Calling stageOneInitialise!");
		if (!stageOneInitialise())
		{
			return false;
		}
	}

	// load up a base dataset if necessary
	if (psBaseData != NULL)
	{
		debug(LOG_WZ, "Loading base dataset %s", psBaseData->pName);
		for(i=0; i<LEVEL_MAXFILES; i++)
		{
			if (psBaseData->apDataFiles[i])
			{
				// load the data
				debug(LOG_WZ, "Loading %s ...", psBaseData->apDataFiles[i]);
				if (!resLoad(psBaseData->apDataFiles[i], i))
				{
					return false;
				}
			}
		}
	}
	if (psNewLevel->type == LDS_CAMCHANGE)
	{
		if (!campaignReset())
		{
			return false;
		}
	}
	if (psNewLevel->game == -1)  //no .gam file to load - BETWEEN missions (for Editor games only)
	{
		ASSERT( psNewLevel->type == LDS_BETWEEN,
			"levLoadData: only BETWEEN missions do not need a .gam file" );
		debug(LOG_WZ, "No .gam file for level: BETWEEN mission");
		if (pSaveName != NULL)
		{
			if (psBaseData != NULL)
			{
				if (!stageTwoInitialise())
				{
					return false;
				}
			}

			//set the mission type before the saveGame data is loaded
			if (saveType == GTYPE_SAVE_MIDMISSION)
			{
				debug(LOG_WZ, "Init mission stuff");
				if (!startMissionSave(psNewLevel->type))
				{
					return false;
				}

				debug(LOG_NEVER, "dataSetSaveFlag");
				dataSetSaveFlag();
			}

			debug(LOG_NEVER, "Loading savegame: %s", pSaveName);
			if (!loadGame(pSaveName, false, true,true))
			{
				return false;
			}
		}

		if ((pSaveName == NULL) ||
			(saveType == GTYPE_SAVE_START))
		{
			debug(LOG_NEVER, "Start mission - no .gam");
			if (!startMission(psNewLevel->type, NULL))
			{
				return false;
			}
		}
	}

	//we need to load up the save game data here for a camchange
	if (bCamChangeSaveGame)
	{
		if (pSaveName != NULL)
		{
			if (psBaseData != NULL)
			{
				if (!stageTwoInitialise())
				{
					return false;
				}
			}

			debug(LOG_NEVER, "loading savegame: %s", pSaveName);
			if (!loadGame(pSaveName, false, true,true))
			{
				return false;
			}

			if (!campaignReset())
			{
				return false;
			}
		}
	}


	// load the new data
	debug(LOG_NEVER, "Loading mission dataset: %s", psNewLevel->pName);
	for(i=0; i < LEVEL_MAXFILES; i++)
	{
		if (psNewLevel->game == i)
		{
			// do some more initialising if necessary
			if (psNewLevel->type == LDS_COMPLETE || psNewLevel->type >= LDS_MULTI_TYPE_START || (psBaseData != NULL && !bCamChangeSaveGame))
			{
				if (!stageTwoInitialise())
				{
					return false;
				}
			}

			// load a savegame if there is one - but not if already done so
			if (pSaveName != NULL && !bCamChangeSaveGame)
			{
				//set the mission type before the saveGame data is loaded
				if (saveType == GTYPE_SAVE_MIDMISSION)
				{
					debug(LOG_WZ, "Init mission stuff");
					if (!startMissionSave(psNewLevel->type))
					{
						return false;
					}

					debug(LOG_NEVER, "dataSetSaveFlag");
					dataSetSaveFlag();
				}

				debug(LOG_NEVER, "Loading save game %s", pSaveName);
				if (!loadGame(pSaveName, false, true,true))
				{
					return false;
				}
			}

			if ((pSaveName == NULL) ||
				(saveType == GTYPE_SAVE_START))
			{
				// load the game
				debug(LOG_WZ, "Loading scenario file %s", psNewLevel->apDataFiles[i]);
				switch (psNewLevel->type)
				{
				case LDS_COMPLETE:
				case LDS_CAMSTART:
					debug(LOG_WZ, "LDS_COMPLETE / LDS_CAMSTART");
					if (!startMission(LDS_CAMSTART, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				case LDS_BETWEEN:
					debug(LOG_WZ, "LDS_BETWEEN");
					if (!startMission(LDS_BETWEEN, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;

				case LDS_MKEEP:
					debug(LOG_WZ, "LDS_MKEEP");
					if (!startMission(LDS_MKEEP, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				case LDS_CAMCHANGE:
					debug(LOG_WZ, "LDS_CAMCHANGE");
					if (!startMission(LDS_CAMCHANGE, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;

				case LDS_EXPAND:
					debug(LOG_WZ, "LDS_EXPAND");
					if (!startMission(LDS_EXPAND, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				case LDS_EXPAND_LIMBO:
					debug(LOG_WZ, "LDS_LIMBO");
					if (!startMission(LDS_EXPAND_LIMBO, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;

				case LDS_MCLEAR:
					debug(LOG_WZ, "LDS_MCLEAR");
					if (!startMission(LDS_MCLEAR, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				case LDS_MKEEP_LIMBO:
					debug(LOG_WZ, "LDS_MKEEP_LIMBO");
					if (!startMission(LDS_MKEEP_LIMBO, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				default:
					ASSERT( psNewLevel->type >= LDS_MULTI_TYPE_START,
						"levLoadData: Unexpected mission type" );
					debug(LOG_WZ, "default (MULTIPLAYER)");
					if (!startMission(LDS_CAMSTART, psNewLevel->apDataFiles[i]))
					{
						return false;
					}
					break;
				}
			}
		}
		else if (psNewLevel->apDataFiles[i])
		{
			// load the data
			debug(LOG_WZ, "Loading %s", psNewLevel->apDataFiles[i]);
			if (!resLoad(psNewLevel->apDataFiles[i], i + CURRENT_DATAID))
			{
				return false;
			}
		}
	}

	dataClearSaveFlag();

	if (pSaveName != NULL)
	{
		//load MidMission Extras
		if (!loadMissionExtras(pSaveName, psNewLevel->type))
		{
			return false;
		}
	}

	if (pSaveName != NULL && saveType == GTYPE_SAVE_MIDMISSION)
	{
		//load script stuff
		// load the event system state here for a save game
		debug(LOG_NEVER, "Loading script system state");
		if (!loadScriptState(pSaveName))
		{
			return false;
		}
	}

	if (!stageThreeInitialise())
	{
		return false;
	}

	//this enables us to to start cam2/cam3 without going via a save game and get the extra droids
	//in from the script-controlled Transporters
	if (!pSaveName && psNewLevel->type == LDS_CAMSTART)
	{
		eventFireCallbackTrigger((TRIGGER_TYPE)CALL_NO_REINFORCEMENTS_LEFT);
	}

	//restore the level name for comparisons on next mission load up
	if (psChangeLevel == NULL)
	{
		psCurrLevel = psNewLevel;
	}
	else
	{
		psCurrLevel = psChangeLevel;
	}


	return true;
}

static void levTestLoad(const char* level)
{
	static char savegameName[80];
	bool retval;

	retval = levLoadData(level, NULL, 0);
	ASSERT(retval, "levLoadData failed selftest");
	ASSERT(checkResearchStats(), "checkResearchStats failed selftest");
	ASSERT(checkStructureStats(), "checkStructureStats failed selftest");
	fprintf(stdout, "\t\tLoaded: %s\n", level);
	strcpy(savegameName, "selftest/");
	PHYSFS_mkdir(savegameName);
	strcat(savegameName, level);
	strcat(savegameName, ".gam");
	retval = saveGame(savegameName, GTYPE_SAVE_START);
	ASSERT(retval, "saveGame failed selftest");
	strcpy(savegameName, "selftest/");	// we need to recreate string, because saveGame clobbered it
	strcat(savegameName, level);
	strcat(savegameName, ".gam");
	levReleaseAll();
	fprintf(stdout, "\t\tSaved: %s\n", savegameName);
}

void levTest(void)
{
	fprintf(stdout, "\tLevels self-test...\n");
	levTestLoad("CAM_1A");
	levTestLoad("CAM_2A");
	levTestLoad("CAM_3A");
	levTestLoad("FASTPLAY");
	levTestLoad("TUTORIAL3");
	levTestLoad("BeggarsKanyon-T1");
	fprintf(stdout, "\tLevels self-test: PASSED\n");
}
