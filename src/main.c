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
 * main.c
 *
 */

// Get platform defines before checking for them!
#include "lib/framework/frame.h"

#include <SDL/SDL.h>
#include <physfs.h>

/* For SHGetFolderPath */
#ifdef WZ_OS_WIN
// FIXME HACK Workaround DATADIR definition in objbase.h
// This works since DATADIR is never used on Windows.
# undef DATADIR
# include <shlobj.h>
#endif // WZ_OS_WIN

#include "lib/framework/configfile.h"
#include "lib/gamelib/gtime.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_opengl/screen.h"
#include "lib/netplay/netplay.h"
#include "lib/script/script.h"
#include "lib/widget/widget.h"

#include "clparse.h"
#include "configuration.h"
#include "display.h"
#include "frontend.h"
#include "game.h"
#include "init.h"
#include "levels.h"
#include "lighting.h"
#include "loadsave.h"
#include "loop.h"
#include "modding.h"
#include "multiplay.h"
#include "research.h"
#include "seqdisp.h"
#include "warzoneconfig.h"
#include "winmain.h"
#include "wrappers.h"

#ifndef DATADIR
# define DATADIR "/usr/share/warzone2100/"
#endif

#if defined(WZ_OS_WIN)
# define WZ_WRITEDIR "Warzone 2100"
#elif defined(WZ_OS_MAC)
# include <CoreServices/CoreServices.h>
# define WZ_WRITEDIR "Warzone 2100"
#else
# define WZ_WRITEDIR ".warzone2100"
#endif

char datadir[MAX_PATH] = "\0"; // Global that src/clparse.c:ParseCommandLine can write to, so it can override the default datadir on runtime. Needs to be \0 on startup for ParseCommandLine to work!

char * global_mods[MAX_MODS] = { NULL };
char * campaign_mods[MAX_MODS] = { NULL };
char * multiplay_mods[MAX_MODS] = { NULL };


// Warzone 2100 . Pumpkin Studios

// Start game in title mode:
GS_GAMEMODE gameStatus = GS_TITLE_SCREEN, lastStatus = GS_TITLE_SCREEN;
//flag to indicate when initialisation is complete
BOOL	videoInitialised = FALSE;
BOOL	gameInitialised = FALSE;
BOOL	frontendInitialised = FALSE;
BOOL	bDisableLobby;
char	SaveGamePath[MAX_PATH];
char	ScreenDumpPath[MAX_PATH];
char	MultiForcesPath[MAX_PATH];
char	MultiCustomMapsPath[MAX_PATH];
char	MultiPlayersPath[MAX_PATH];
char	KeyMapPath[MAX_PATH];
char	UserMusicPath[MAX_PATH];

extern void debug_callback_stderr( void**, const char * );
extern void debug_callback_win32debug( void**, const char * );

static BOOL inList( char * list[], const char * item )
{
	int i = 0;
#ifdef DEBUG
	debug( LOG_NEVER, "inList: Current item: [%s]", item );
#endif
	while( list[i] != NULL )
	{
#ifdef DEBUG
		debug( LOG_NEVER, "inList: Checking for match with: [%s]", list[i] );
#endif
		if ( strcmp( list[i], item ) == 0 )
			return TRUE;
		i++;
	}
	return FALSE;
}

void addSubdirs( const char * basedir, const char * subdir, const BOOL appendToPath, char * checkList[] )
{
	char tmpstr[MAX_PATH];
	char ** subdirlist = PHYSFS_enumerateFiles( subdir );
	char ** i = subdirlist;
	while( *i != NULL )
	{
#ifdef DEBUG
		debug( LOG_NEVER, "addSubdirs: Examining subdir: [%s]", *i );
#endif // DEBUG
		if( !checkList || inList( checkList, *i ) )
		{
			strcpy( tmpstr, basedir );
			strcat( tmpstr, subdir );
			strcat( tmpstr, PHYSFS_getDirSeparator() );
			strcat( tmpstr, *i );
#ifdef DEBUG
			debug( LOG_NEVER, "addSubdirs: Adding [%s] to search path", tmpstr );
#endif // DEBUG
			PHYSFS_addToSearchPath( tmpstr, appendToPath );
		}
		i++;
	}
	PHYSFS_freeList( subdirlist );
}

void removeSubdirs( const char * basedir, const char * subdir, char * checkList[] )
{
	char tmpstr[MAX_PATH];
	char ** subdirlist = PHYSFS_enumerateFiles( subdir );
	char ** i = subdirlist;
	while( *i != NULL )
	{
#ifdef DEBUG
		debug( LOG_NEVER, "removeSubdirs: Examining subdir: [%s]", *i );
#endif // DEBUG
		if( !checkList || inList( checkList, *i ) )
		{
			strcpy( tmpstr, basedir );
			strcat( tmpstr, subdir );
			strcat( tmpstr, PHYSFS_getDirSeparator() );
			strcat( tmpstr, *i );
#ifdef DEBUG
			debug( LOG_NEVER, "removeSubdirs: Removing [%s] from search path", tmpstr );
#endif // DEBUG
			PHYSFS_removeFromSearchPath( tmpstr );
		}
		i++;
	}
	PHYSFS_freeList( subdirlist );
}

void printSearchPath( void )
{
	char ** i, ** searchPath;

	debug(LOG_WZ, "Search paths:");
	searchPath = PHYSFS_getSearchPath();
	for (i = searchPath; *i != NULL; i++) {
		debug(LOG_WZ, "    [%s]", *i);
	}
	PHYSFS_freeList( searchPath );
}


static void getPlatformUserDir(char * tmpstr)
{
#if defined(WZ_OS_WIN)
	if ( SUCCEEDED( SHGetFolderPathA( NULL, CSIDL_PERSONAL|CSIDL_FLAG_CREATE, NULL, SHGFP_TYPE_CURRENT, tmpstr ) ) )
		strcat( tmpstr, PHYSFS_getDirSeparator() );
	else
#elif defined(WZ_OS_MAC)
	short vol_ref;
	long dir_id;
	FSSpec fsspec;
	FSRef fsref;
	OSErr error = FindFolder(kUserDomain, kApplicationSupportFolderType, FALSE, &vol_ref, &dir_id);
	if (!error)
		error = FSMakeFSSpec(vol_ref, dir_id, (const unsigned char *) "", &fsspec);
	if (!error)
		error = FSpMakeFSRef(&fsspec, &fsref);
	if (!error)
		error = FSRefMakePath(&fsref, tmpstr, MAX_PATH);
	if (!error)
		strcat( tmpstr, PHYSFS_getDirSeparator() );
	else
#endif
		strcpy( tmpstr, PHYSFS_getUserDir() ); // Use PhysFS supplied UserDir (As fallback on Windows / Mac, default on Linux)
}


/***************************************************************************
	Initialize the PhysicsFS library.
***************************************************************************/
static void initialize_PhysicsFS(void)
{
	PHYSFS_Version compiled;
	PHYSFS_Version linked;
	char tmpstr[MAX_PATH] = { '\0' };

	PHYSFS_VERSION(&compiled);
	PHYSFS_getLinkedVersion(&linked);

	debug(LOG_WZ, "Compiled against PhysFS version: %d.%d.%d",
	      compiled.major, compiled.minor, compiled.patch);
	debug(LOG_WZ, "Linked against PhysFS version: %d.%d.%d",
	      linked.major, linked.minor, linked.patch);

	getPlatformUserDir(tmpstr);

	if ( !PHYSFS_setWriteDir( tmpstr ) ) // Workaround for PhysFS not creating the writedir as expected.
	{
		debug( LOG_ERROR, "Error setting write directory to \"%s\": %s",
			PHYSFS_getUserDir(), PHYSFS_getLastError() );
		exit(1);
	}

	if ( !PHYSFS_mkdir( WZ_WRITEDIR ) ) // s.a.
	{
		debug( LOG_ERROR, "Error creating directory \"%s\": %s",
			WZ_WRITEDIR, PHYSFS_getLastError() );
		exit(1);
	}

	// Append the Warzone subdir
	strcat( tmpstr, WZ_WRITEDIR );
	strcat( tmpstr, PHYSFS_getDirSeparator() );

	if ( !PHYSFS_setWriteDir( tmpstr ) ) {
		debug( LOG_ERROR, "Error setting write directory to \"%s\": %s",
			tmpstr, PHYSFS_getLastError() );
		exit(1);
	}

	// User's home dir first so we allways see what we write
	PHYSFS_addToSearchPath( PHYSFS_getWriteDir(), PHYSFS_PREPEND );

	PHYSFS_permitSymbolicLinks(1);

	debug( LOG_WZ, "Write dir: %s", PHYSFS_getWriteDir() );
	debug( LOG_WZ, "Base dir: %s", PHYSFS_getBaseDir() );
}

/*
 * \fn void scanDataDirs( void )
 * \brief Adds default data dirs
 *
 * Priority:
 * Lower loads first. Current:
 * -datadir > User's home dir > SVN data > AutoPackage > BaseDir > DEFAULT_DATADIR
 *
 * Only -datadir and home dir are allways examined. Others only if data still not found.
 *
 * We need ParseCommandLine, before we can add any mods...
 *
 * \sa rebuildSearchPath
 */
static void scanDataDirs( void )
{
	char tmpstr[MAX_PATH] = {'\0'}, prefix[MAX_PATH] = {'\0'};

	// Find out which PREFIX we are in...
	strcpy( tmpstr, PHYSFS_getBaseDir() );
	*strrchr( tmpstr, *PHYSFS_getDirSeparator() ) = '\0'; // Trim ending '/', which getBaseDir always provides

	strncpy( prefix, PHYSFS_getBaseDir(), // Skip the last dir from base dir
		strrchr( tmpstr, *PHYSFS_getDirSeparator() ) - tmpstr );

	atexit( cleanSearchPath );

	// Commandline supplied datadir
	if( strlen( datadir ) != 0 )
		registerSearchPath( datadir, 1 );

	// User's home dir
	registerSearchPath( PHYSFS_getWriteDir(), 2 );
	rebuildSearchPath( mod_multiplay, TRUE );

	if( !PHYSFS_exists("gamedesc.lev") )
	{
		// Data in SVN dir
		strcpy( tmpstr, prefix );
		strcat( tmpstr, "/data/" );
		registerSearchPath( tmpstr, 3 );
		rebuildSearchPath( mod_multiplay, TRUE );

		if( !PHYSFS_exists("gamedesc.lev") )
		{
			// Relocation for AutoPackage
			strcpy( tmpstr, prefix );
			strcat( tmpstr, "/share/warzone2100/" );
			registerSearchPath( tmpstr, 4 );
			rebuildSearchPath( mod_multiplay, TRUE );

			if( !PHYSFS_exists("gamedesc.lev") )
			{
				// Program dir
				registerSearchPath( PHYSFS_getBaseDir(), 5 );
				rebuildSearchPath( mod_multiplay, TRUE );

				if( !PHYSFS_exists("gamedesc.lev") )
				{
					// Guessed fallback default datadir on Unix
					registerSearchPath( DATADIR, 6 );
					rebuildSearchPath( mod_multiplay, TRUE );
				}
			}
		}
	}

#ifdef WZ_OS_MAC
	if( !PHYSFS_exists("gamedesc.lev") ) {
		CFURLRef resourceURL = CFBundleCopyResourcesDirectoryURL(CFBundleGetMainBundle());
		char resourcePath[PATH_MAX];
		if( CFURLGetFileSystemRepresentation( resourceURL, true,
							(UInt8 *) resourcePath,
							PATH_MAX) ) {
			chdir( resourcePath );
			registerSearchPath( "data", 7 );
			rebuildSearchPath( mod_multiplay, TRUE );
		} else {
			debug( LOG_ERROR, "Could not change to resources directory." );
		}
	}
#endif

	/** Debugging and sanity checks **/

	printSearchPath();

	if( PHYSFS_exists("gamedesc.lev") )
	{
		debug( LOG_WZ, "gamedesc.lev found at %s", PHYSFS_getRealDir( "gamedesc.lev" ) );
	}
	else
	{
		debug( LOG_ERROR, "Could not find game data. Aborting." );
		exit(1);
	}
}

/***************************************************************************
	Make a directory in write path and set a variable to point to it.
***************************************************************************/
static void make_dir(char *dest, const char *dirname, const char *subdir)
{
	strcpy(dest, dirname);
	if (subdir != NULL) {
		strcat(dest, "/");
		strcat(dest, subdir);
	}
	{
		size_t l = strlen(dest);

		if (dest[l-1] != '/') {
			dest[l] = '/';
			dest[l+1] = '\0';
		}
	}
	PHYSFS_mkdir(dest);
	if ( !PHYSFS_mkdir(dest) ) {
		debug(LOG_ERROR, "Unable to create directory \"%s\" in write dir \"%s\"!",
		      dest, PHYSFS_getWriteDir());
		exit(1);
	}
}


int main(int argc, char *argv[])
{
	BOOL quit = FALSE;
	BOOL Restart = FALSE;
	BOOL lostFocus = FALSE;
	int loopStatus = 0;
	iColour* psPaletteBuffer = NULL;
	UDWORD pSize = 0;

	/*** Initialize the debug subsystem ***/
#if defined(WZ_CC_MSVC) && defined(DEBUG)
	int tmpDbgFlag;
	_CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_DEBUG ); // Output CRT info to debugger

	tmpDbgFlag = _CrtSetDbgFlag( _CRTDBG_REPORT_FLAG ); // Grab current flags
# if defined(DEBUG_MEMORY)
	tmpDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF; // Check every (de)allocation
# endif // DEBUG_MEMORY
	tmpDbgFlag |= _CRTDBG_ALLOC_MEM_DF; // Check allocations
	tmpDbgFlag |= _CRTDBG_LEAK_CHECK_DF; // Check for memleaks
	_CrtSetDbgFlag( tmpDbgFlag );
#endif // WZ_CC_MSVC && DEBUG

	setupExceptionHandler(argv[0]);

	debug_init();
	atexit( debug_exit );

	debug_register_callback( debug_callback_stderr, NULL, NULL, NULL );
#if defined(WZ_OS_WIN) && defined(DEBUG)
//	debug_register_callback( debug_callback_win32debug, NULL, NULL, NULL );
#endif // WZ_OS_WIN && DEBUG

	// find early boot info
	if ( !ParseCommandLineEarly(argc, argv) ) {
		return -1;
	}

#ifdef DEBUG
	debug( LOG_WZ, "Warzone 2100 - Version %s - Built %s - DEBUG", VERSION, __DATE__ );
#else
	debug( LOG_WZ, "Warzone 2100 - Version %s - Built %s", VERSION, __DATE__ );
#endif
	/*** Initialize translations ***/
	setlocale(LC_ALL, "");
	(void)bindtextdomain(PACKAGE, LOCALEDIR);
	(void)textdomain(PACKAGE);

	/*** Initialize PhysicsFS ***/

	PHYSFS_init(argv[0]);
	initialize_PhysicsFS();

	make_dir(ScreenDumpPath, "screendumps", NULL);
	make_dir(SaveGamePath, "savegame", NULL);
	make_dir(MultiPlayersPath, "multiplay", NULL);
	make_dir(MultiPlayersPath, "multiplay", "players");
	make_dir(MultiForcesPath, "multiplay", "forces");
	make_dir(MultiCustomMapsPath, "multiplay", "custommaps");

	/* Put these files in the writedir root */
	strcpy(RegFilePath, "config");
	strcpy(KeyMapPath, "keymap.map");
	strcpy(UserMusicPath, "music");

	// initialise all the command line states
	war_SetDefaultStates();

	debug(LOG_MAIN, "initializing");

	bDisableLobby = FALSE;

	loadConfig();
	atexit( closeConfig );
	loadRenderMode(); //get the registry entry for clRendMode

	// parse the command line
	if (!ParseCommandLine(argc, argv)) {
		return -1;
	}

	saveConfig();

	scanDataDirs();

	// find out if the lobby stuff has been disabled
	if (!bDisableLobby && !lobbyInitialise()) // ajl. Init net stuff. Lobby can modify startup conditions like commandline.
	{
		return -1;
	}

	if (!frameInitialise( "Warzone 2100", pie_GetVideoBufferWidth(), pie_GetVideoBufferHeight(), pie_GetVideoBufferDepth(), war_getFullscreen() ))
	{
		return -1;
	}
	atexit(frameShutDown);

	pie_SetFogStatus(FALSE);
	pie_ScreenFlip(CLEAR_BLACK);

	//load palette
	psPaletteBuffer = (iColour*)malloc(256 * sizeof(iColour)+1);
	if (psPaletteBuffer == NULL)
	{
		debug( LOG_ERROR, "Out of memory" );
		abort();
		return -1;
	}
	if ( !loadFileToBuffer("palette.bin", (char*)psPaletteBuffer, ( 256 * sizeof(iColour) + 1 ), &pSize) )
	{
		debug( LOG_ERROR, "Couldn't load palette data" );
		abort();
		return -1;
	}
	pal_AddNewPalette(psPaletteBuffer);
	free(psPaletteBuffer);
	atexit(pal_ShutDown);

	pie_LoadBackDrop(SCREEN_RANDOMBDROP);
	pie_SetFogStatus(FALSE);
	pie_ScreenFlip(CLEAR_BLACK);

	if (!systemInitialise())
	{
		return -1;
	}
	atexit(systemShutdown);

	//set all the pause states to false
	setAllPauseStates(FALSE);

	while (!quit)
	{
		// Do the game mode specific initialisation.
		switch(gameStatus)
		{
			case GS_TITLE_SCREEN:
				screen_RestartBackDrop();
				if (!frontendInitialise("wrf/frontend.wrf"))
				{
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}

				frontendInitialised = TRUE;
				frontendInitVars();
				break;

			case GS_SAVEGAMELOAD:
				screen_RestartBackDrop();
				SetGameMode(GS_NORMAL);
				// load up a save game
				if (!loadGameInit(saveGameName))
				{
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}
				screen_StopBackDrop();
				break;
			case GS_NORMAL:
				if (!levLoadData(pLevelName, NULL, 0))
				{
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}
				//after data is loaded check the research stats are valid
				if (!checkResearchStats())
				{
					debug( LOG_ERROR, "Invalid Research Stats" );
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}
				//and check the structure stats are valid
				if (!checkStructureStats())
				{
					debug( LOG_ERROR, "Invalid Structure Stats" );
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}

				//set a flag for the trigger/event system to indicate initialisation is complete
				gameInitialised = TRUE;
				screen_StopBackDrop();
				break;

			default:
				debug( LOG_ERROR, "Unknown game status on shutdown!" );
		}

		debug(LOG_MAIN, "Entering main loop");

		Restart = FALSE;

		while (!Restart)
		{
			// Event handling, etc.
			switch (frameUpdate())
			{
				case FRAME_KILLFOCUS:
					lostFocus = TRUE;
					gameTimeStop();
					audio_StopAll();
					break;
				case FRAME_SETFOCUS:
					lostFocus = FALSE;
					gameTimeStart();
					break;
				case FRAME_QUIT:
					debug(LOG_MAIN, "frame quit");
					quit = TRUE;
					Restart = TRUE;
					break;
				default:
					break;
			}

			lastStatus = gameStatus;

			if (!lostFocus && !quit)
			{
				if (loop_GetVideoStatus())
				{
					videoLoop();
				}
				else switch(gameStatus)
				{
					case GS_TITLE_SCREEN:
						switch(titleLoop()) {
							case TITLECODE_QUITGAME:
								debug(LOG_MAIN, "TITLECODE_QUITGAME");
								Restart = TRUE;
								quit = TRUE;
								break;
							case TITLECODE_SAVEGAMELOAD:
								debug(LOG_MAIN, "TITLECODE_SAVEGAMELOAD");
								gameStatus = GS_SAVEGAMELOAD;
								Restart = TRUE;
								break;
							case TITLECODE_STARTGAME:
								debug(LOG_MAIN, "TITLECODE_STARTGAME");
								SetGameMode(GS_NORMAL);
								Restart = TRUE;
								break;

							case TITLECODE_SHOWINTRO:
								debug(LOG_MAIN, "TITLECODE_SHOWINTRO");
								seq_ClearSeqList();
								seq_AddSeqToList("eidos-logo.rpl", NULL, NULL, FALSE);
								seq_AddSeqToList("pumpkin.rpl", NULL, NULL, FALSE);
								seq_AddSeqToList("titles.rpl", NULL, NULL, FALSE);
								seq_AddSeqToList("devastation.rpl", NULL, "devastation.txa", FALSE);
								seq_StartNextFullScreenVideo();
								break;

							case TITLECODE_CONTINUE:
								break;

							default:
								debug(LOG_ERROR, "Unknown code returned by titleLoop");
						}
						break;

					case GS_NORMAL:
						loopStatus = gameLoop();
						switch(loopStatus) {
							case GAMECODE_QUITGAME:
								debug(LOG_MAIN, "GAMECODE_QUITGAME");
								SetGameMode(GS_TITLE_SCREEN);
								Restart = TRUE;
								if(NetPlay.bLobbyLaunched)
								{
									quit = TRUE;
								}
								break;
							case GAMECODE_FASTEXIT:
								debug(LOG_MAIN, "GAMECODE_FASTEXIT");
								Restart = TRUE;
								quit = TRUE;
								break;

							case GAMECODE_LOADGAME:
								debug(LOG_MAIN, "GAMECODE_LOADGAME");
								Restart = TRUE;
								gameStatus = GS_SAVEGAMELOAD;
								break;

							case GAMECODE_PLAYVIDEO:
								debug(LOG_MAIN, "GAMECODE_PLAYVIDEO");
								Restart = FALSE;
								break;

							case GAMECODE_NEWLEVEL:
							case GAMECODE_RESTARTGAME:
								debug(LOG_MAIN, "GAMECODE_RESTARTGAME");
								Restart = TRUE;
								break;

							case GAMECODE_CONTINUE:
								break;

							default:
								debug(LOG_ERROR, "Unknown code returned by gameLoop");
						}
						break;

					default:
						debug(LOG_ERROR, "Weirdy game status, I'm afraid!!");
						break;
				}

				gameTimeUpdate();
			} // !lostFocus && !quit
			else if (lostFocus && !quit)
			{
				// Prevent CPU hogging when we've lost focus
				SDL_Delay(1);
			}
		} // End of !Restart loop.

		debug(LOG_MAIN, "Preparing for shutdown/restart");

		// Do game mode specific shutdown.
		switch(lastStatus)
		{
			case GS_TITLE_SCREEN:
				if (!frontendShutdown())
				{
					debug(LOG_ERROR, "Shutting down after failure");
					exit(EXIT_FAILURE);
				}
				frontendInitialised = FALSE;
				break;

			case GS_NORMAL:
				if (loopStatus != GAMECODE_NEWLEVEL)
				{
					initLoadingScreen(TRUE); // returning to f.e. do a loader.render not active
					pie_EnableFog(FALSE);//dont let the normal loop code set status on
					fogStatus = 0;
					if (loopStatus != GAMECODE_LOADGAME)
					{
						levReleaseAll();
					}
				}
				gameInitialised = FALSE;
				break;

			default:
				debug(LOG_ERROR, "Unknown game status on shutdown!");
				break;
		}
	} // End of !quit loop.

	debug(LOG_MAIN, "Shutting down Warzone 2100");

	return EXIT_SUCCESS;
}


GS_GAMEMODE GetGameMode(void)
{
	return gameStatus;
}


void SetGameMode(GS_GAMEMODE status)
{
	gameStatus = status;
}
