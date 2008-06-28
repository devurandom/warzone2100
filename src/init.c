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
 * @file init.c
 *
 * Game initialisation routines.
 *
 */
#include "lib/framework/frame.h"

#include <string.h>

#include "lib/framework/frameresource.h"
#include "lib/framework/input.h"
#include "lib/framework/file.h"
#include "lib/framework/physfs_ext.h"
#include "lib/framework/strres.h"
#include "lib/ivis_common/piemode.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/tex.h"
#include "lib/ivis_common/ivi.h"
#include "lib/netplay/netplay.h"
#include "lib/script/script.h"
#include "lib/sound/audio_id.h"
#include "lib/sound/cdaudio.h"
#include "lib/sound/mixer.h"

#include "init.h"

#include "advvis.h"
#include "atmos.h"
#include "cluster.h"
#include "cmddroid.h"
#include "component.h"
#include "configuration.h"
#include "console.h"
#include "data.h"
#include "display.h"
#include "display3d.h"
#include "drive.h"
#include "edit3d.h"
#include "effects.h"
#include "environ.h"
#include "formation.h"
#include "fpath.h"
#include "frend.h"
#include "frontend.h"
#include "game.h"
#include "gateway.h"
#include "hci.h"
#include "intdisplay.h"
#include "keymap.h"
#include "levels.h"
#include "lighting.h"
#include "loop.h"
#include "mapgrid.h"
#include "mechanics.h"
#include "miscimd.h"
#include "mission.h"
#include "modding.h"
#include "multigifts.h"
#include "multiplay.h"
#include "projectile.h"
#include "radar.h"
#include "lib/framework/cursors.h"
#include "scriptextern.h"
#include "scripttabs.h"
#include "scriptvals.h"
#include "text.h"
#include "texture.h"
#include "transporter.h"
#include "warzoneconfig.h"
#include "main.h"
#include "wrappers.h"

static void	initMiscVars(void);

static const char UserMusicPath[] = "music";

extern char * global_mods[];
extern char * campaign_mods[];
extern char * multiplay_mods[];

// FIXME Totally inappropriate place for this.
char fileLoadBuffer[FILE_LOAD_BUFFER_SIZE];

IMAGEFILE *FrontImages;

BOOL DirectControl = false;

static wzSearchPath * searchPathRegistry = NULL;


// Each module in the game should have a call from here to initialise
// any globals and statics to there default values each time the game
// or frontend restarts.
//
static BOOL InitialiseGlobals(void)
{
	frontendInitVars();	// Initialise frontend globals and statics.
	statsInitVars();
	structureInitVars();
	if (!messageInitVars())
	{
		return false;
	}
	if (!researchInitVars())
	{
		return false;
	}
	featureInitVars();
	radarInitVars();
	Edit3DInitVars();

	driveInitVars(true);

	return true;
}


static BOOL loadLevFile(const char* filename, searchPathMode datadir)
{
	char *pBuffer;
	UDWORD size;

	debug( LOG_WZ, "Loading lev file: %s\n", filename );

	if (   !PHYSFS_exists(filename)
	    || !loadFile(filename, &pBuffer, &size)) {
		debug(LOG_ERROR, "loadLevFile: File not found: %s\n", filename);
		return false; // only in NDEBUG case
	}
	if (!levParse(pBuffer, size, datadir)) {
		debug(LOG_ERROR, "loadLevFile: Parse error in %s\n", filename);
		return false;
	}
	free(pBuffer);

	return true;
}


void cleanSearchPath( void )
{
	wzSearchPath * curSearchPath = searchPathRegistry, * tmpSearchPath = NULL;

	// Start at the lowest priority
	while( curSearchPath->lowerPriority )
		curSearchPath = curSearchPath->lowerPriority;

	while( curSearchPath )
	{
		tmpSearchPath = curSearchPath->higherPriority;
		free( curSearchPath );
		curSearchPath = tmpSearchPath;
	}
}


/*!
 * Register searchPath above the path with next lower priority
 * For information about what can be a search path, refer to PhysFS documentation
 */
void registerSearchPath( const char path[], unsigned int priority )
{
	wzSearchPath * curSearchPath = searchPathRegistry, * tmpSearchPath = NULL;

	tmpSearchPath = (wzSearchPath*)malloc(sizeof(*tmpSearchPath));
	sstrcpy(tmpSearchPath->path, path);
	if (path[strlen(path)-1] != *PHYSFS_getDirSeparator())
		sstrcat(tmpSearchPath->path, PHYSFS_getDirSeparator());
	tmpSearchPath->priority = priority;

	debug( LOG_WZ, "registerSearchPath: Registering %s at priority %i", path, priority );
	if ( !curSearchPath )
	{
		searchPathRegistry = tmpSearchPath;
		searchPathRegistry->lowerPriority = NULL;
		searchPathRegistry->higherPriority = NULL;
		return;
	}

	while( curSearchPath->higherPriority && priority > curSearchPath->priority )
		curSearchPath = curSearchPath->higherPriority;
	while( curSearchPath->lowerPriority && priority < curSearchPath->priority )
		curSearchPath = curSearchPath->lowerPriority;

	if ( priority < curSearchPath->priority )
	{
	        tmpSearchPath->lowerPriority = curSearchPath->lowerPriority;
        	tmpSearchPath->higherPriority = curSearchPath;
	}
	else
	{
		tmpSearchPath->lowerPriority = curSearchPath;
		tmpSearchPath->higherPriority = curSearchPath->higherPriority;
	}

	if( tmpSearchPath->lowerPriority )
		tmpSearchPath->lowerPriority->higherPriority = tmpSearchPath;
	if( tmpSearchPath->higherPriority )
		tmpSearchPath->higherPriority->lowerPriority = tmpSearchPath;
}


/*!
 * \brief Rebuilds the PHYSFS searchPath with mode specific subdirs
 *
 * Priority:
 * maps > mods > base > base.wz
 */
BOOL rebuildSearchPath( searchPathMode mode, BOOL force )
{
	static searchPathMode current_mode = mod_clean;
	wzSearchPath * curSearchPath = searchPathRegistry;
	char tmpstr[PATH_MAX] = "\0";

	if ( mode != current_mode || force )
	{
		current_mode = mode;

		rebuildSearchPath( mod_clean, false );

		// Start at the lowest priority
		while( curSearchPath->lowerPriority )
			curSearchPath = curSearchPath->lowerPriority;

		switch ( mode )
		{
			case mod_clean:
				debug( LOG_WZ, "rebuildSearchPath: Cleaning up" );

				while( curSearchPath )
				{
#ifdef DEBUG
					debug( LOG_WZ, "rebuildSearchPath: Removing [%s] from search path", curSearchPath->path );
#endif // DEBUG
					// Remove maps and mods
					removeSubdirs( curSearchPath->path, "maps", NULL );
					removeSubdirs( curSearchPath->path, "mods/global", global_mods );
					removeSubdirs( curSearchPath->path, "mods/global/autoload", NULL );
					removeSubdirs( curSearchPath->path, "mods/campaign", campaign_mods );
					removeSubdirs( curSearchPath->path, "mods/campaign/autoload", NULL );
					removeSubdirs( curSearchPath->path, "mods/multiplay", multiplay_mods );
					removeSubdirs( curSearchPath->path, "mods/multiplay/autoload", NULL );

					// Remove multiplay patches
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "mp");
					PHYSFS_removeFromSearchPath( tmpstr );
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "mp.wz");
					PHYSFS_removeFromSearchPath( tmpstr );

					// Remove base files
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base");
					PHYSFS_removeFromSearchPath( tmpstr );
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base.wz");
					PHYSFS_removeFromSearchPath( tmpstr );

					curSearchPath = curSearchPath->higherPriority;
				}
				break;
			case mod_campaign:
				debug( LOG_WZ, "rebuildSearchPath: Switching to campaign mods" );

				while( curSearchPath )
				{
#ifdef DEBUG
					debug( LOG_WZ, "rebuildSearchPath: Adding [%s] to search path", curSearchPath->path );
#endif // DEBUG
					// Add global and campaign mods
					PHYSFS_addToSearchPath( curSearchPath->path, PHYSFS_APPEND );

					addSubdirs( curSearchPath->path, "mods/global", PHYSFS_APPEND, global_mods );
					addSubdirs( curSearchPath->path, "mods/global/autoload", PHYSFS_APPEND, NULL );
					addSubdirs( curSearchPath->path, "mods/campaign", PHYSFS_APPEND, campaign_mods );
					addSubdirs( curSearchPath->path, "mods/campaign/autoload", PHYSFS_APPEND, NULL );
					PHYSFS_removeFromSearchPath( curSearchPath->path );

					// Add base files
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base.wz");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );

					curSearchPath = curSearchPath->higherPriority;
				}
				break;
			case mod_multiplay:
				debug( LOG_WZ, "rebuildSearchPath: Switching to multiplay mods" );

				while( curSearchPath )
				{
#ifdef DEBUG
					debug( LOG_WZ, "rebuildSearchPath: Adding [%s] to search path", curSearchPath->path );
#endif // DEBUG
					// Add maps and global and multiplay mods
					PHYSFS_addToSearchPath( curSearchPath->path, PHYSFS_APPEND );
					addSubdirs( curSearchPath->path, "maps", PHYSFS_APPEND, NULL );
					addSubdirs( curSearchPath->path, "mods/global", PHYSFS_APPEND, global_mods );
					addSubdirs( curSearchPath->path, "mods/global/autoload", PHYSFS_APPEND, NULL );
					addSubdirs( curSearchPath->path, "mods/multiplay", PHYSFS_APPEND, multiplay_mods );
					addSubdirs( curSearchPath->path, "mods/multiplay/autoload", PHYSFS_APPEND, NULL );
					PHYSFS_removeFromSearchPath( curSearchPath->path );

					// Add multiplay patches
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "mp");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "mp.wz");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );

					// Add base files
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );
					sstrcpy(tmpstr, curSearchPath->path);
					sstrcat(tmpstr, "base.wz");
					PHYSFS_addToSearchPath( tmpstr, PHYSFS_APPEND );

					curSearchPath = curSearchPath->higherPriority;
				}
				break;
			default:
				debug( LOG_ERROR, "rebuildSearchPath: Can't switch to unknown mods %i", mode );
				return false;
		}

		// User's home dir must be first so we allways see what we write
		PHYSFS_removeFromSearchPath( PHYSFS_getWriteDir() );
		PHYSFS_addToSearchPath( PHYSFS_getWriteDir(), PHYSFS_PREPEND );

#ifdef DEBUG
		printSearchPath();
#endif // DEBUG
	}
	return true;
}


BOOL buildMapList(void)
{
	char ** filelist, ** file;
	size_t len;

	if ( !loadLevFile( "gamedesc.lev", mod_campaign ) ) {
		return false;
	}
	loadLevFile( "addon.lev", mod_multiplay );

	filelist = PHYSFS_enumerateFiles("");
	for ( file = filelist; *file != NULL; ++file ) {
		len = strlen( *file );
		if ( len > 10 // Do not add addon.lev again
				&& !strcasecmp( *file+(len-10), ".addon.lev") ) {
			loadLevFile( *file, mod_multiplay );
		}
	}
	PHYSFS_freeList( filelist );
	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Called once on program startup.
//
BOOL systemInitialise(void)
{
	if (!widgInitialise())
	{
		return false;
	}

	buildMapList();

	// Initialize render engine
	war_SetFog(false);
	if (!pie_Initialise()) {
		debug(LOG_ERROR, "Unable to initialise renderer");
		return false;
	}

	pie_SetGammaValue((float)gammaValue / 20.0f);

	if ( war_getSoundEnabled() )
	{
		if (!audio_Init(droidAudioTrackStopped))
		{
			debug(LOG_SOUND, "Could not initialise audio system: Continuing without audio");
		}
		cdAudio_Open(UserMusicPath);
	}
	else
	{
		debug(LOG_SOUND, "Sound disabled");
	}

	if (!dataInitLoadFuncs()) // Pass all the data loading functions to the framework library
	{
		return false;
	}

	if (!fpathInitialise())
	{
		return false;
	}

#ifdef ARROWS
	arrowInit();
#endif

	// Initialize the iVis text rendering module
	iV_TextInit();

	iV_Reset();								// Reset the IV library.
	initLoadingScreen(true);

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Called once at program shutdown.
//
void systemShutdown(void)
{
//	unsigned int i;
#ifdef ARROWS
	arrowShutDown();
#endif

	keyClearMappings();

	// free up all the load functions (all the data should already have been freed)
	resReleaseAll();

	if (!multiShutdown()) // ajl. init net stuff
	{
		return;
	}

	debug(LOG_MAIN, "shutting down audio subsystems");

	debug(LOG_MAIN, "shutting down CD audio");
	cdAudio_Stop();
	cdAudio_Close();

	if ( audio_Disabled() == false && !audio_Shutdown() )
	{
		return;
	}

	debug(LOG_MAIN, "shutting down graphics subsystem");
	iV_ShutDown();
	levShutDown();
	widgShutDown();
	pie_TerrainCleanup();

	fpathShutdown();

	return;
}

/***************************************************************************/

static BOOL
init_ObjectDead( void * psObj )
{
	BASE_OBJECT	*psBaseObj = (BASE_OBJECT *) psObj;
	DROID		*psDroid;
	STRUCTURE	*psStructure;

	CHECK_OBJECT(psBaseObj);

	if ( psBaseObj->died == true )
	{
		switch ( psBaseObj->type )
		{
			case OBJ_DROID:
				psDroid = (DROID *) psBaseObj;
				psDroid->psCurAnim = NULL;
				break;

			case OBJ_STRUCTURE:
				psStructure = (STRUCTURE *) psBaseObj;
				psStructure->psCurAnim = NULL;
				break;

			default:
				debug( LOG_ERROR, "init_ObjectAnimRemoved: unrecognised object type" );
				abort();
		}
	}

	return psBaseObj->died;
}

// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Called At Frontend Startup.

BOOL frontendInitialise(const char *ResourceFile)
{
	debug(LOG_MAIN, "Initialising frontend : %s", ResourceFile);

	if(!InitialiseGlobals())				// Initialise all globals and statics everywhere.
	{
		return false;
	}

	iV_Reset();								// Reset the IV library.

	if (!scrTabInitialise())				// Initialise the script system
	{
		return false;
	}

	if (!stringsInitialise())				// Initialise the string system
	{
		return false;
	}

	if (!objInitialise())					// Initialise the object system
	{
		return false;
	}

	if (!anim_Init())
	{
		return false;
	}

	if ( !animObj_Init( init_ObjectDead ) )
	{
		return false;
	}

	if (!allocPlayerPower())	 //set up the PlayerPower for each player - this should only be done ONCE now
	{
		return false;
	}

	debug(LOG_MAIN, "frontEndInitialise: loading resource file .....");
	if (!resLoad(ResourceFile, 0))
	{
		//need the object heaps to have been set up before loading in the save game
		return false;
	}

	if (!dispInitialise())					// Initialise the display system
	{
		return false;
	}

#ifdef BUCKET
	if ( !bucketSetupList() )				// reset object list
	{
		return false;
	}
#endif

	FrontImages = (IMAGEFILE*)resGetData("IMG", "frontend.img");
   	/* Shift the interface initialisation here temporarily so that it
   		can pick up the stats after they have been loaded */
	if (!intInitialise())
	{
		return false;
	}

	// keymappings
	// clear out any existing mappings
	keyClearMappings();
	keyInitMappings(false);

	// Set the default uncoloured cursor here, since it looks slightly
	// better for menus and such.
	pie_SetMouse(CURSOR_DEFAULT, false);

	SetFormAudioIDs(-1,ID_SOUND_WINDOWCLOSE);			// disable the open noise since distorted in 3dfx builds.

	initMiscVars();

	gameTimeInit();

	// hit me with some funky beats....
	cdAudio_PlayTrack(SONG_FRONTEND);

	return true;
}


BOOL frontendShutdown(void)
{
	debug(LOG_WZ, "== Shuting down frontend ==");

	saveConfig();// save settings to registry.

	if (!mechShutdown())
	{
		return false;
	}

	releasePlayerPower();

	intShutDown();
	scrShutDown();

	//do this before shutting down the iV library
	resReleaseAllData();

	if (!objShutdown())
	{
		return false;
	}

	ResearchRelease();

	if ( !anim_Shutdown() )
	{
		return false;
	}

	if ( !animObj_Shutdown() )
	{
		return false;
	}

	debug(LOG_TEXTURE, "=== frontendShutdown ===");
	pie_TexShutDown();
	pie_TexInit(); // ready for restart

	return true;
}


/******************************************************************************/
/*                       Initialisation before data is loaded                 */



BOOL stageOneInitialise(void)
{
	debug(LOG_WZ, "== stageOneInitalise ==");

	// Initialise all globals and statics everwhere.
	if(!InitialiseGlobals())
	{
		return false;
	}

	iV_Reset(); // Reset the IV library

	if (!stringsInitialise())	/* Initialise the string system */
	{
		return false;
	}

	if (!objInitialise())		/* Initialise the object system */
	{
		return false;
	}

	if (!droidInit())
	{
		return false;
	}

	if (!initViewData())
	{
		return false;
	}

	if (!grpInitialise())
	{
		return false;
	}

   	if (!aiInitialise())		/* Initialise the AI system */ // pregame
	{
		return false;
	}

	if (!anim_Init())
	{
		return false;
	}

	if ( !animObj_Init( init_ObjectDead ) )
	{
		return false;
	}

	if (!allocPlayerPower())	/*set up the PlayerPower for each player - this should only be done ONCE now*/
	{
		return false;
	}

	if (!formationInitialise())		// Initialise the formation system
	{
		return false;
	}

	// initialise the visibility stuff
	if (!visInitialise())
	{
		return false;
	}

	/* Initialise the movement system */
	if (!moveInitialise())
	{
		return false;
	}

	if (!proj_InitSystem())
	{
		return false;
	}

	if (!scrTabInitialise())	// Initialise the script system
	{
		return false;
	}

	if (!gridInitialise())
	{
		return false;
	}


    if (!environInit())
    {
        return false;
    }
	// reset speed limiter
	moveSetFormationSpeedLimiting(true);


	initMission();
	initTransporters();

    //do this here so that the very first mission has it initialised
    initRunData();

	gameTimeInit();
    //need to reset the event timer too - AB 14/01/99
    eventTimeReset(gameTime/SCR_TICKRATE);

	return true;
}


/******************************************************************************/
/*                       Shutdown after data is released                      */

BOOL stageOneShutDown(void)
{
	debug(LOG_WZ, "== stageOneShutDown ==");

	if ( audio_Disabled() == false )
	{
		sound_CheckAllUnloaded();
	}

	proj_Shutdown();

    releaseMission();

	if (!aiShutdown())
	{
		return false;
	}

	if (!objShutdown())
	{
		return false;
	}

	grpShutDown();

	formationShutDown();
	releasePlayerPower();

	ResearchRelease();

	//free up the gateway stuff?
	gwShutDown();

	if (!mapShutdown())
	{
		return false;
	}

	scrShutDown();
	environShutDown();
	gridShutDown();

	if ( !anim_Shutdown() )
	{
		return false;
	}

	if ( !animObj_Shutdown() )
	{
		return false;
	}

	debug(LOG_TEXTURE, "=== stageOneShutDown ===");
	pie_TexShutDown();
	pie_TexInit(); // restart it

	initMiscVars();

	return true;
}


// ////////////////////////////////////////////////////////////////////////////
// Initialise after the base data is loaded but before final level data is loaded

BOOL stageTwoInitialise(void)
{
	int i;

	debug(LOG_WZ, "== stageTwoInitalise ==");

	// make sure we clear on loading; this a bad hack to fix a bug when
	// loading a savegame where we are building a lassat
	for (i = 0; i < MAX_PLAYERS; i++)
	{
		setLasSatExists(false, i);
	}

	if(bMultiPlayer)
	{
		if (!multiTemplateSetup())
		{
			return false;
		}
	}

	if (!dispInitialise())		/* Initialise the display system */
	{
		return false;
	}

	LOADBARCALLBACK();	//	loadingScreenCallback();

	if(!InitRadar()) 	// After resLoad cause it needs the game palette initialised.
	{
		return false;
	}

	LOADBARCALLBACK();	//	loadingScreenCallback();

	if(!initMiscImds())			/* Set up the explosions */
	{
		iV_ShutDown();
		debug( LOG_ERROR, "Can't find all the explosions PCX's" );
		abort();
		return false;
	}

	if (!cmdDroidInit())
	{
		return false;
	}

	LOADBARCALLBACK();	//	loadingScreenCallback();

#ifdef BUCKET
	if ( !bucketSetupList() )	/* reset object list */
	{
		return false;
	}
#endif

   	/* Shift the interface initialisation here temporarily so that it
   		can pick up the stats after they have been loaded */

	LOADBARCALLBACK();	//	loadingScreenCallback();

	if (!intInitialise())
	{
		return false;
	}

	LOADBARCALLBACK();	//	loadingScreenCallback();

	if (!initMessage())			/* Initialise the message heaps */
	{
		return false;
	}

	if (!gwInitialise())
	{
		return false;
	}

	// keymappings
	LOADBARCALLBACK();	//	loadingScreenCallback();
	keyClearMappings();
	keyInitMappings(false);
	LOADBARCALLBACK();	//	loadingScreenCallback();

	// Set the default uncoloured cursor here, since it looks slightly
	// better for menus and such.
	pie_SetMouse(CURSOR_DEFAULT, false);

	SetFormAudioIDs(ID_SOUND_WINDOWOPEN,ID_SOUND_WINDOWCLOSE);

	debug(LOG_MAIN, "stageTwoInitialise: done");

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Free up after level specific data has been released but before base data is released
//
BOOL stageTwoShutDown(void)
{
	debug(LOG_WZ, "== stageTwoShutDown ==");

	cdAudio_Stop();

	freeAllStructs();
	freeAllDroids();
	freeAllFeatures();
	freeAllFlagPositions();

	if (!messageShutdown())
	{
		return false;
	}

	if (!mechShutdown())
	{
		return false;
	}


	if(!ShutdownRadar()) {
		return false;
	}

	intShutDown();

	cmdDroidShutDown();

	//free up the gateway stuff?
	gwShutDown();

	if (!mapShutdown())
	{
		return false;
	}

	return true;
}

BOOL stageThreeInitialise(void)
{
	STRUCTURE *psStr;
	UDWORD i;
	DROID		*psDroid;

	debug(LOG_WZ, "== stageThreeInitalise ==");
	bTrackingTransporter = false;

	loopMissionState = LMS_NORMAL;

	// reset the clock to normal speed
	gameTimeResetMod();

	if (!init3DView())	// Initialise 3d view stuff. After resLoad cause it needs the game palette initialised.
	{
		return false;
	}

	effectResetUpdates();
	initLighting(0, 0, mapWidth, mapHeight);

	if(bMultiPlayer)
	{
		// FIXME Is this really needed?
		debug( LOG_WZ, "multiGameInit()\n" );
		multiGameInit();
		cmdDroidMultiExpBoost(true);
	}
	else
	{
		//ensure single player games do not have this set
		game.maxPlayers = 0;
	}

	preProcessVisibility();
	closeLoadingScreen();			// reset the loading screen.

	if (!fpathInitialise())
	{
		return false;
	}

	clustInitialise();

	gridReset();

	//if mission screen is up, close it.
	if(MissionResUp)
	{
		intRemoveMissionResultNoAnim();
	}

	// determine if to use radar
	for(psStr = apsStructLists[selectedPlayer];psStr;psStr=psStr->psNext){
		if(psStr->pStructureType->type == REF_HQ)
		{
			radarOnScreen = true;
			setHQExists(true,selectedPlayer);
			break;
		}
	}

	// Re-inititialise some static variables.

	driveInitVars(false);
	displayInitVars();
	resizeRadar();

	setAllPauseStates(false);

	/* decide if we have to create teams */
	if(game.alliance == ALLIANCES_TEAMS && game.type == SKIRMISH)
	{
		createTeamAlliances();

		/* Update ally vision for pre-placed structures and droids */
		for(i=0;i<MAX_PLAYERS;i++)
		{
			if(i != selectedPlayer)
			{
				/* Structures */
				for(psStr=apsStructLists[i]; psStr; psStr=psStr->psNext)
				{
					if(aiCheckAlliances(psStr->player,selectedPlayer))
					visTilesUpdate((BASE_OBJECT *)psStr, rayTerrainCallback);
				}

				/* Droids */
				for(psDroid=apsDroidLists[i]; psDroid; psDroid=psDroid->psNext)
				{
					if(aiCheckAlliances(psDroid->player,selectedPlayer))
					visTilesUpdate((BASE_OBJECT *)psDroid, rayTerrainCallback);
				}
			}
		}
	}

	// ffs JS   (and its a global!)
	if (getLevelLoadType() != GTYPE_SAVE_MIDMISSION)
	{
		eventFireCallbackTrigger((TRIGGER_TYPE)CALL_GAMEINIT);
	}

	return true;
}





/*****************************************************************************/
/*      Shutdown before any data is released                                 */

BOOL stageThreeShutDown(void)
{
	debug(LOG_WZ, "== stageThreeShutDown ==");

	// make sure any button tips are gone.
	widgReset();

	audio_StopAll();

	if(bMultiPlayer)
	{
		multiGameShutdown();
	}

	cmdDroidMultiExpBoost(false);

	eventReset();

	// reset the script values system
	scrvReset();

	//call this here before mission data is released
	if (!missionShutDown())
	{
		return false;
	}

	/*
		When this line wasn't at this point. The PSX version always failed on the next script after the tutorial ... unexplained why?
	*/
//	bInTutorial=false;
	scrExternReset();

    //reset the run data so that doesn't need to be initialised in the scripts
    initRunData();

	resetVTOLLandingPos();

// Restore player colours since the scripts might of changed them.

	if(!bMultiPlayer)
	{
		int temp = getPlayerColour(selectedPlayer);
		initPlayerColours();
		setPlayerColour(selectedPlayer,temp);
	}
	else
	{
		initPlayerColours();		// reset colours leaving multiplayer game.
	}

	setScriptWinLoseVideo(PLAY_NONE);

	return true;
}

// Reset the game between campaigns
BOOL campaignReset(void)
{
	debug(LOG_MAIN, "campaignReset");
	gwShutDown();
	mapShutdown();
	return true;
}

// Reset the game when loading a save game
BOOL saveGameReset(void)
{
	debug(LOG_MAIN, "saveGameReset");

	cdAudio_Stop();

	freeAllStructs();
	freeAllDroids();
	freeAllFeatures();
	freeAllFlagPositions();
	initMission();
	initTransporters();

	//free up the gateway stuff?
	gwShutDown();
	intResetScreen(true);
	intResetPreviousObj();

	if (!mapShutdown())
	{
		return false;
	}

    //clear out any messages
    freeMessages();

	return true;
}

// --- Miscellaneous Initialisation stuff that really should be done each restart
static void	initMiscVars(void)
{
	selectedPlayer = 0;
	godMode = false;

	// ffs am

	radarOnScreen = true;
	enableConsoleDisplay(true);

	setSelectedGroup(UBYTE_MAX);
	processDebugMappings(false);
}
