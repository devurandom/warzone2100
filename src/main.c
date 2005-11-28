/*
 * main.c
 *
 */
#include "frame.h"

#include <physfs.h>

#include "widget.h"
#include "script.h"
#include "init.h"
#include "loop.h"
#include "objects.h"
#include "display.h"
#include "piestate.h"
#include "gtime.h"
#include "winmain.h"
#include "wrappers.h"
#include "scripttabs.h"
#include "deliverance.h"
#include "frontend.h"
#include "seqdisp.h"
#include "audio.h"
#include "console.h"
#include "rendmode.h"
#include "piemode.h"
#include "levels.h"
#include "research.h"
#include "warzoneconfig.h"
#include "clparse.h"
#include "cdspan.h"
#include "configuration.h"
#include "multiplay.h"
#include "netplay.h"
#include "loadsave.h"
#include "game.h"
#include "lighting.h"
#include "mixer.h"
#include "screen.h"

#ifndef DEFAULT_DATA_PATH
#define DEFAULT_DATA_PATH ""
#endif

// Warzone 2100 . Pumpkin Studios

UDWORD	gameStatus = GS_TITLE_SCREEN;	// Start game in title mode.
UDWORD	lastStatus = GS_TITLE_SCREEN;
//flag to indicate when initialisation is complete
BOOL	videoInitialised = FALSE;
BOOL	gameInitialised = FALSE;
BOOL	frontendInitialised = FALSE;
BOOL	reInit = FALSE;
BOOL	bDisableLobby;
BOOL pQUEUE=TRUE;			//This is used to control our pQueue list. Always ON except for SP games! -Q
char	SaveGamePath[255];
char	ScreenDumpPath[255];
char	MultiForcesPath[255];
char	MultiCustomMapsPath[255];
char	MultiPlayersPath[255];
char	KeyMapPath[255];
char*	UserMusicPath;
char default_data_path[MAX_PATH];
char	__UserMusicPath[255];
char	RegFilePath[255];

/*
BOOL checkDisableLobby(void)
{
	BOOL	disable;

	disable = FALSE;
	if(!openWarzoneKey())
	{
		return FALSE;
	}

	if (!getWarzoneKeyNumeric("DisableLobby",(DWORD*)&disable))
	{
		return FALSE;
	}

	if (!closeWarzoneKey())
	{
		return FALSE;
	}

	return disable;
}
*/

/***************************************************************************
	Initialize the PhysicsFS library
***************************************************************************/
static void initialize_PhysicsFS(char *mod)
{
	PHYSFS_Version compiled;
	PHYSFS_Version linked;
	char **i;
	char datapath[MAX_PATH], overridepath[MAX_PATH], modpath[MAX_PATH], mappath[MAX_PATH];

	PHYSFS_VERSION(&compiled);
	PHYSFS_getLinkedVersion(&linked);

	debug(LOG_WZ, "Compiled against PhysFS version: %d.%d.%d",
	      compiled.major, compiled.minor, compiled.patch);
	debug(LOG_WZ, "Linked against PhysFS version: %d.%d.%d",
	      linked.major, linked.minor, linked.patch);

	/* NOTE: Whenever major version is increased, the second string below MUST
   * be changed too.  This should be in the release documentation in the
   * wiki. */
	if (!PHYSFS_setSaneConfig("warzone", "warzone-0.2", "wz", 0, 1)) {
		debug(LOG_ERROR, "Error setting initial paths: %s", PHYSFS_getLastError());
		exit(1);
	}
	if (PHYSFS_getWriteDir() == NULL) {
		debug(LOG_ERROR, "Error getting PhysicsFS write directory.");
		exit(1);
	}

	if (*default_data_path != '\0') {
		strcpy(datapath, default_data_path);
		strcat(datapath, PHYSFS_getDirSeparator());
		strcat(datapath, "warzone.wz");
		if (!PHYSFS_addToSearchPath(datapath, 1)) {
			debug(LOG_WZ, "Could not find warzone.wz in \"%s\".", default_data_path);
			*default_data_path = '\0'; // try current dir instead
		}
	}
	if (*default_data_path == '\0') {
		strcpy(datapath, PHYSFS_getBaseDir());
		if (!PHYSFS_addToSearchPath(datapath, 1)
		    || !PHYSFS_exists("gamedesc.lev")) {
			debug(LOG_WZ, "%s is not data directory", datapath);
			strcat(datapath, "data");
			debug(LOG_WZ, "Checking if %s is the data directory", datapath);
			if (!PHYSFS_addToSearchPath(datapath, 1)) {
				debug(LOG_ERROR, "No game data found anywhere!  Aborting...");
				exit(1);
			}
		}
	}
	snprintf(overridepath, sizeof(overridepath), "%soverride", 
	         PHYSFS_getBaseDir());
	strcpy(mappath, PHYSFS_getBaseDir());
	strcat(mappath, "maps");
	if (mod) {
		snprintf(modpath, sizeof(modpath), "%smods%s%s.wz", PHYSFS_getBaseDir(), 
		         PHYSFS_getDirSeparator(), mod);
	}

	/* The 1 below means append to search path, while 0 means prepend. */
	if (!PHYSFS_addToSearchPath(overridepath, 0)) {
		debug(LOG_WZ, "Error adding override path %s: %s", overridepath,
		      PHYSFS_getLastError());
	}
	if (!PHYSFS_addToSearchPath(mappath, 0)) {
		debug(LOG_WZ, "Error adding map path %s: %s", mappath,
		      PHYSFS_getLastError());
	}
	if (mod && !PHYSFS_addToSearchPath(modpath, 0)) {
		debug(LOG_ERROR, "Error adding mod path %s: %s", modpath,
		      PHYSFS_getLastError());
	}

	/** Debugging and sanity checks **/

	debug(LOG_WZ, "Search paths:");
	for (i = PHYSFS_getSearchPath(); *i != NULL; i++) {
		debug(LOG_WZ, "    [%s]", *i);
	}
	PHYSFS_freeList(PHYSFS_getSearchPath());
	debug(LOG_WZ, "Write path: %s", PHYSFS_getWriteDir());

	PHYSFS_permitSymbolicLinks(1);

	if (!PHYSFS_exists("gamedesc.lev")) {
		debug(LOG_ERROR, "Could not find the file gamedesc.lev in archives!");
		exit(1);
	}
	debug(LOG_WZ, "gamedesc.lev found at %s", PHYSFS_getRealDir("gamedesc.lev"));
}

/***************************************************************************
	Make a directory in write path and set a variable to point to it.
***************************************************************************/
static void make_dir(char *dest, char *dirname, char *subdir)
{
	strcpy(dest, dirname);
	if (subdir != NULL) {
		strcat(dest, PHYSFS_getDirSeparator());
		strcat(dest, subdir);
	}
	PHYSFS_mkdir(dest);
	if (PHYSFS_isDirectory(dest) == 0) {
		debug(LOG_ERROR, "Unable to create directory \"%s\" in write dir \"%s\"!",
		      dest, PHYSFS_getWriteDir());
		exit(1);
	}
}

int main(int argc, char *argv[])
{
	FRAME_STATUS		frameRet;
	BOOL			quit = FALSE;
	BOOL			Restart = FALSE;
	BOOL			paused = FALSE;//, firstTime = TRUE;
	BOOL			bVidMem = FALSE;
	SDWORD			dispBitDepth = DISP_BITDEPTH;
	SDWORD			introVideoControl = 3;
	GAMECODE		loopStatus = 0;
	iColour*		psPaletteBuffer;
	SDWORD			pSize;

	/*** Initialize the debug subsystem ***/
	/* Debug stuff for .net, don't delete :) */
#ifdef _MSC_VER	
#ifdef _DEBUG
	{
		int tmpFlag; //debug stuff for VC -Q

		tmpFlag = _CrtSetDbgFlag( _CRTDBG_REPORT_FLAG );
		tmpFlag |= (_CRTDBG_CHECK_ALWAYS_DF | _CRTDBG_ALLOC_MEM_DF);
		_CrtSetDbgFlag( tmpFlag );			// just turning on VC debug stuff...
	}
#endif
	#ifndef MAX_PATH
	#define MAX_PATH 512
	#endif
#endif

	debug_init();

	strcpy(default_data_path, DEFAULT_DATA_PATH);
	// find early boot info
	if (!ParseCommandLineEarly(argc, argv)) {
		return -1;
	}

	/*** Initialize PhysicsFS ***/

#ifdef WIN32
	PHYSFS_init(NULL);
#else
	PHYSFS_init(argv[0]);
#endif
	initialize_PhysicsFS(NULL);

	make_dir(ScreenDumpPath, "screendumps", NULL);
	make_dir(SaveGamePath, "savegame", NULL);
	make_dir(MultiPlayersPath, "multiplay", "players");
	make_dir(MultiForcesPath, "multiplay", "forces");
	make_dir(MultiCustomMapsPath, "multiplay", "custommaps");

	/* Put these files in the writedir root */
	strcpy(RegFilePath, "config");
	strcpy(KeyMapPath, "keymap.map");
	strcpy(__UserMusicPath, "music");
	UserMusicPath = __UserMusicPath;


	/*** etc ***/

	// initialise all the command line states
	clIntroVideo = FALSE;
	war_SetDefaultStates();

init://jump here from the end if re_initialising

	if (!blkInitialise())
	{
		return FALSE;
	}

	loadRenderMode();//get the registry entry for clRendMode

	bDisableLobby = FALSE;

	// parse the command line
	if (!reInit) {
		if (!ParseCommandLine(argc, argv)) {
			return -1;
		}
	}
  debug(LOG_MAIN, "reinitializing");

	// find out if the lobby stuff has been disabled
//	bDisableLobby = checkDisableLobby();
	if (!bDisableLobby &&
		!lobbyInitialise())			// ajl. Init net stuff. Lobby can modify startup conditions like commandline.
	{
		return -1;
	}

	reInit = FALSE;//just so we dont restart again

#ifdef USE_FILE_PATH
	chdir(FILE_PATH);
#endif

	bVidMem = FALSE;
	dispBitDepth = DISP_BITDEPTH;

//	frameDDEnumerate();

	if (!frameInitialise(NULL, "Warzone 2100", DISP_WIDTH,DISP_HEIGHT,dispBitDepth, war_getFullscreen(), bVidMem))
	{
		return -1;
	}

	pie_SetFogStatus(FALSE);
	pie_ScreenFlip(CLEAR_BLACK);
	pie_ScreenFlip(CLEAR_BLACK);

	if(gameStatus == GS_VIDEO_MODE) 
	{
		introVideoControl = 0;//play video
		gameStatus = GS_TITLE_SCREEN;
	}

	//load palette
	psPaletteBuffer = (iColour*)MALLOC(256 * sizeof(iColour)+1);
	if (psPaletteBuffer == NULL)
	{
		DBERROR(("Out of memory"));
		return -1;
	}
	if (!loadFileToBuffer("palette.bin", psPaletteBuffer, (256 * sizeof(iColour)+1),(UDWORD*)&pSize))
	{
		DBERROR(("Couldn't load palette data"));
		return -1;
	}
	pal_AddNewPalette(psPaletteBuffer);
	FREE(psPaletteBuffer);

	pie_LoadBackDrop(SCREEN_RANDOMBDROP,FALSE);
	pie_SetFogStatus(FALSE);
	pie_ScreenFlip(CLEAR_BLACK);

	quit = FALSE;

	/* check CDROM drive available */
	if ( cdspan_CheckCDAvailable() == FALSE )
	{
		DBERROR( ("Cannot detect CDROM drive\n") );
		quit = TRUE;
	}

	if (!systemInitialise())
	{
		return -1;
	}
	
	//set all the pause states to false
	setAllPauseStates(FALSE);

	while (!quit)
	{
// Do the game mode specific initialisation.
		switch(gameStatus)
		{
			case GS_TITLE_SCREEN:
				screen_RestartBackDrop();

				if (!frontendInitialise("wrf\\frontend.wrf"))
				{
					goto exit;
				}

				frontendInitialised = TRUE;
				frontendInitVars();
				//if intro required set up the video
				if (introVideoControl <= 1)
				{
					seq_ClearSeqList();
					seq_AddSeqToList("eidos-logo.rpl",NULL, NULL, FALSE,0);
					seq_AddSeqToList("pumpkin.rpl",NULL, NULL, FALSE,0);
					seq_AddSeqToList("titles.rpl",NULL, NULL, FALSE,0);
					seq_AddSeqToList("devastation.rpl",NULL,"devastation.txa", FALSE,0);

					seq_StartNextFullScreenVideo();
					introVideoControl = 2;
				}
				break;

			case GS_SAVEGAMELOAD:
				screen_RestartBackDrop();
				gameStatus = GS_NORMAL;
				// load up a save game
				if (!loadGameInit(saveGameName,FALSE))
				{
					goto exit;
				}
				/*if (!levLoadData(pLevelName, saveGameName)) {
					return -1;
				}*/
				screen_StopBackDrop();
				break;
			case GS_NORMAL:
				if (!levLoadData(pLevelName, NULL, 0)) {
					goto exit;
				}
				//after data is loaded check the research stats are valid
				if (!checkResearchStats())
				{
					DBERROR(("Invalid Research Stats"));
					goto exit;
				}
				//and check the structure stats are valid
				if (!checkStructureStats())
				{
					DBERROR(("Invalid Structure Stats"));
					goto exit;
				}

				//set a flag for the trigger/event system to indicate initialisation is complete
				gameInitialised = TRUE;
				screen_StopBackDrop();
				break;
			case GS_VIDEO_MODE:
				DBERROR(("Video_mode no longer valid"));
				if (introVideoControl == 0)
				{
					videoInitialised = TRUE;
				}
				break;

			default:
				debug(LOG_ERROR, "Unknown game status on shutdown!");
		}

		
		debug(LOG_MAIN, "Entering main loop");

		Restart = FALSE;
		//firstTime = TRUE;

		while (!Restart)
		{
			frameRet = frameUpdate();
			
			if (pie_GetRenderEngine() == ENGINE_OPENGL)	//Was ENGINE_D3D -Q
			{
				if ( frameRet == FRAME_SETFOCUS )
				{
//					D3DTestCooperativeLevel( TRUE );
				}
				else
				{
//					D3DTestCooperativeLevel( FALSE );
				}
			}

			switch (frameRet)
			{
			case FRAME_KILLFOCUS:
				paused = TRUE;
				gameTimeStop();

				mixer_SaveIngameVols();
				mixer_RestoreWinVols();
				audio_StopAll();
				break;
			case FRAME_SETFOCUS:
				paused = FALSE;
				gameTimeStart();
				if (!dispModeChange())
				{
					quit = TRUE;
					Restart = TRUE;
				}

				else if (pie_GetRenderEngine() == ENGINE_OPENGL)	//Was ENGINE_D3D -Q
				{
//					dtm_RestoreTextures();
				}
				mixer_SaveWinVols();
				mixer_RestoreIngameVols();
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

			if ((!paused) && (!quit))
			{
				switch(gameStatus)
				{
				case	GS_TITLE_SCREEN:
					pie_SetSwirlyBoxes(TRUE);
					if (loop_GetVideoStatus())
					{
						videoLoop();
					}
					else
					{
						switch(titleLoop()) {
							case TITLECODE_QUITGAME:
								debug(LOG_MAIN, "TITLECODE_QUITGAME");
								Restart = TRUE;
								quit = TRUE;
								break;

	//						case TITLECODE_ATTRACT:
	//							DBPRINTF(("TITLECODE_ATTRACT\n"));
	//							break;

							case TITLECODE_SAVEGAMELOAD:
								debug(LOG_MAIN, "TITLECODE_SAVEGAMELOAD");
								gameStatus = GS_SAVEGAMELOAD;
								Restart = TRUE;
								break;
							case TITLECODE_STARTGAME:
								debug(LOG_MAIN, "TITLECODE_STARTGAME");
								gameStatus = GS_NORMAL;
								Restart = TRUE;
								break;

							case TITLECODE_SHOWINTRO:	
								debug(LOG_MAIN, "TITLECODE_SHOWINTRO");
								seq_ClearSeqList();
								seq_AddSeqToList("eidos-logo.rpl",NULL,NULL, FALSE,0);
								seq_AddSeqToList("pumpkin.rpl",NULL,NULL, FALSE,0);
								seq_AddSeqToList("titles.rpl",NULL,NULL, FALSE,0);
								seq_AddSeqToList("devastation.rpl",NULL,"devastation.txa", FALSE,0);
								seq_StartNextFullScreenVideo();
								introVideoControl = 2;//play the video but dont init the sound system
								break;

							case TITLECODE_CONTINUE:
								break;

							default:
								debug(LOG_ERROR, "Unknown code returned by titleLoop");
						}
					}
					pie_SetSwirlyBoxes(FALSE);
					break;
			
/*				case GS_SAVEGAMELOAD:
					if (loopNewLevel)
					{
						//the start of a campaign/expand mission
						DBPRINTF(("GAMECODE_NEWLEVEL\n"));
						loopNewLevel = FALSE;
						// gameStatus is unchanged, just loading additional data
						Restart = TRUE;
					}
					break;
*/
				case	GS_NORMAL:
					if (loop_GetVideoStatus())
					{
						videoLoop();
					}
					else
					{
						loopStatus = gameLoop();
						switch(loopStatus) {
							case GAMECODE_QUITGAME:
								debug(LOG_MAIN, "GAMECODE_QUITGAME");
								gameStatus = GS_TITLE_SCREEN;
								Restart = TRUE;
								if(NetPlay.bLobbyLaunched)
								{
//									changeTitleMode(QUIT);
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
//dont schange mode any more								gameStatus = GS_VIDEO_MODE;
								Restart = FALSE;
								break;

							case GAMECODE_NEWLEVEL:
								debug(LOG_MAIN, "GAMECODE_NEWLEVEL");
								// gameStatus is unchanged, just loading additional data
								Restart = TRUE;
								break;

							case GAMECODE_RESTARTGAME:
								debug(LOG_MAIN, "GAMECODE_RESTARTGAME");
								Restart = TRUE;
								break;

							case GAMECODE_CONTINUE:
								break;

							default:
								debug(LOG_ERROR, "Unknown code returned by gameLoop");
						}
					}
					break;

				case	GS_VIDEO_MODE:
					debug(LOG_ERROR, "Video_mode no longer valid");
					if (loop_GetVideoStatus())
					{
						videoLoop();
					}
					else
					{
						if (introVideoControl <= 1)
						{
								seq_ClearSeqList();

								seq_AddSeqToList("factory.rpl",NULL,NULL, FALSE,0);
								seq_StartNextFullScreenVideo();//"sequences\\factory.rpl","sequences\\factory.wav");
								introVideoControl = 2;
						}
						else
						{
								debug(LOG_MAIN, "VIDEO_QUIT");
								if (introVideoControl == 2)//finished playing intro video
								{
									gameStatus = GS_TITLE_SCREEN;
									if (videoInitialised)
									{
										Restart = TRUE;
									}
									introVideoControl = 3;
								}
								else
								{
									gameStatus = GS_NORMAL;
								}
						}
					}

					break;
		
				default:
					debug(LOG_ERROR, "Weirdy game status, I'm afraid!!");
					break;
				}

				gameTimeUpdate();
			}
		}	// End of !Restart loop.

// Do game mode specific shutdown.	
		switch(lastStatus) {
			case GS_TITLE_SCREEN:
				if (!frontendShutdown())
				{
					goto exit;
				}
				frontendInitialised = FALSE;
				break;

/*			case GS_SAVEGAMELOAD:
				//get the next level to load up
				gameStatus = GS_NORMAL;
				break;*/
			case GS_NORMAL:
				if (loopStatus != GAMECODE_NEWLEVEL)
				{
					initLoadingScreen(TRUE,FALSE);	// returning to f.e. do a loader.render not active
					pie_EnableFog(FALSE);//dont let the normal loop code set status on
					fogStatus = 0;
					if (loopStatus != GAMECODE_LOADGAME)
					{
						levReleaseAll();
					}
				}
				gameInitialised = FALSE;
				break;

			case	GS_VIDEO_MODE:
				debug(LOG_ERROR, "Video_mode no longer valid");
				if (videoInitialised)
				{
					videoInitialised = FALSE;
				}
				break;

			default:
				debug(LOG_ERROR, "Unknown game status on shutdown!");
				break;
		}
	
	} // End of !quit loop.

  debug(LOG_MAIN, "Shuting down application");

	systemShutdown();
	pal_ShutDown();
	frameShutDown();

	if (reInit) goto init;

	return 0;

exit:

	debug(LOG_ERROR, "Shutting down after failure");
	systemShutdown();
	pal_ShutDown();
	frameShutDown();

	return 1;
}


UDWORD GetGameMode(void)
{
	return gameStatus;
}

void SetGameMode(UDWORD status)
{
	ASSERT((status == GS_TITLE_SCREEN ||
			status == GS_MISSION_SCREEN ||
			status == GS_NORMAL ||
			status == GS_VIDEO_MODE ||
			status == GS_SAVEGAMELOAD,
		"SetGameMode: invalid game mode"));

	gameStatus = status;
}

