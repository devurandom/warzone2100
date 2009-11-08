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
 * clParse.c
 *
 * Parse command line arguments
 *
 */

#include <string.h>
#include "lib/framework/frame.h"
#include <popt.h>

#include "main.h"
#include "frontend.h"

#include "lib/ivis_common/pieclip.h"
#include "lib/netplay/netplay.h"
#include "warzoneconfig.h"

#include "clparse.h"
#include "loadsave.h"
#include "objects.h"
#include "advvis.h"
#include "multiplay.h"
#include "wrappers.h"
#include "cheat.h"
#include "init.h"
#include "keybind.h"
 // To set the shadow config:
#include "display.h"
#include "version.h"

// these are all global variables
extern char	datadir[PATH_MAX];
extern char	configdir[PATH_MAX];
extern char * global_mods[MAX_MODS];
extern char * campaign_mods[MAX_MODS];
extern char * multiplay_mods[MAX_MODS];
extern char iptoconnect[PATH_MAX];
extern BOOL hostlaunch;
extern bool CauseCrash;

//! Let the end user into debug mode....
BOOL	bAllowDebugMode = false;

typedef enum
{
	// We don't want to use zero, so start at one (1)
	CLI_CHEAT = 1,
	CLI_CONFIGDIR,
	CLI_DATADIR,
	CLI_DEBUG,
	CLI_DEBUGFILE,
	CLI_FLUSHDEBUGSTDERR,
	CLI_FULLSCREEN,
	CLI_GAME,
	CLI_HELP,
	CLI_MOD_GLOB,
	CLI_MOD_CA,
	CLI_MOD_MP,
	CLI_SAVEGAME,
	CLI_USAGE,
	CLI_WINDOW,
	CLI_VERSION,
	CLI_RESOLUTION,
	CLI_SHADOWS,
	CLI_NOSHADOWS,
	CLI_SOUND,
	CLI_NOSOUND,
	CLI_SELFTEST,
	CLI_CONNECTTOIP,
	CLI_HOSTLAUNCH,
	CLI_NOASSERT,
	CLI_CRASH,
} CLI_OPTIONS;

static const struct poptOption* getOptionsTable(void)
{
	static const struct poptOption optionsTable[] =
	{
		{ "cheat",      '\0', POPT_ARG_NONE,   NULL, CLI_CHEAT,      N_("Run in cheat mode"),                 NULL },
		{ "datadir",    '\0', POPT_ARG_STRING, NULL, CLI_DATADIR,    N_("Set default data directory"),        N_("data directory") },
		{ "configdir",  '\0', POPT_ARG_STRING, NULL, CLI_CONFIGDIR,  N_("Set configuration directory"),       N_("configuration directory") },
		{ "debug",      '\0', POPT_ARG_STRING, NULL, CLI_DEBUG,      N_("Show debug for given level"),        N_("debug level") },
		{ "debugfile",  '\0', POPT_ARG_STRING, NULL, CLI_DEBUGFILE,  N_("Log debug output to file"),          N_("file") },
		{ "flush-debug-stderr", '\0', POPT_ARG_NONE, NULL, CLI_FLUSHDEBUGSTDERR, N_("Flush all debug output written to stderr"), NULL },
		{ "fullscreen", '\0', POPT_ARG_NONE,   NULL, CLI_FULLSCREEN, N_("Play in fullscreen mode"),           NULL },
		{ "game",       '\0', POPT_ARG_STRING, NULL, CLI_GAME,       N_("Load a specific game"),              N_("game-name") },
		{ "help",       'h',  POPT_ARG_NONE,   NULL, CLI_HELP,       N_("Show this help message and exit"),   NULL },
		{ "mod",        '\0', POPT_ARG_STRING, NULL, CLI_MOD_GLOB,   N_("Enable a global mod"),               N_("mod") },
		{ "mod_ca",     '\0', POPT_ARG_STRING, NULL, CLI_MOD_CA,     N_("Enable a campaign only mod"),        N_("mod") },
		{ "mod_mp",     '\0', POPT_ARG_STRING, NULL, CLI_MOD_MP,     N_("Enable a multiplay only mod"),       N_("mod") },
		{ "noassert",	'\0', POPT_ARG_NONE,   NULL, CLI_NOASSERT,   N_("Disable asserts"),                   NULL },
		{ "crash",		'\0', POPT_ARG_NONE,   NULL, CLI_CRASH,      N_("Causes a crash to test the crash handler"), NULL },
		{ "savegame",   '\0', POPT_ARG_STRING, NULL, CLI_SAVEGAME,   N_("Load a saved game"),                 N_("savegame") },
		{ "usage",      '\0', POPT_ARG_NONE
		          | POPT_ARGFLAG_DOC_HIDDEN,   NULL, CLI_USAGE,      NULL,                                    NULL, },
		{ "window",     '\0', POPT_ARG_NONE,   NULL, CLI_WINDOW,     N_("Play in windowed mode"),             NULL },
		{ "version",    '\0', POPT_ARG_NONE,   NULL, CLI_VERSION,    N_("Show version information and exit"), NULL },
		{ "resolution", '\0', POPT_ARG_STRING, NULL, CLI_RESOLUTION, N_("Set the resolution to use"),         N_("WIDTHxHEIGHT") },
		{ "shadows",    '\0', POPT_ARG_NONE,   NULL, CLI_SHADOWS,    N_("Enable shadows"),                    NULL },
		{ "noshadows",  '\0', POPT_ARG_NONE,   NULL, CLI_NOSHADOWS,  N_("Disable shadows"),                   NULL },
		{ "sound",      '\0', POPT_ARG_NONE,   NULL, CLI_SOUND,      N_("Enable sound"),                      NULL },
		{ "nosound",    '\0', POPT_ARG_NONE,   NULL, CLI_NOSOUND,    N_("Disable sound"),                     NULL },
		{ "selftest",   '\0', POPT_ARG_NONE,   NULL, CLI_SELFTEST,   N_("Activate self-test"),                NULL },
		{ "join",       '\0', POPT_ARG_STRING, NULL, CLI_CONNECTTOIP,N_("connect directly to IP/hostname"),   N_("host") },
		{ "host",       '\0', POPT_ARG_NONE,   NULL, CLI_HOSTLAUNCH, N_("go directly to host screen"),        NULL },
		// Terminating entry
		{ NULL,         '\0', 0,               NULL, 0,              NULL,                                    NULL },
	};

	static struct poptOption TranslatedOptionsTable[sizeof(optionsTable) / sizeof(struct poptOption)];
	static bool translated = false;

	if (translated == false)
	{
		unsigned int table_size = sizeof(optionsTable) / sizeof(struct poptOption) - 1;
		unsigned int i;

		for (i = 0; i < table_size; ++i)
		{
			TranslatedOptionsTable[i] = optionsTable[i];

			// If there is a description, make sure to translate it with gettext
			if (TranslatedOptionsTable[i].descrip != NULL)
				TranslatedOptionsTable[i].descrip = gettext(TranslatedOptionsTable[i].descrip);

			if (TranslatedOptionsTable[i].argDescrip != NULL)
				TranslatedOptionsTable[i].argDescrip = gettext(TranslatedOptionsTable[i].argDescrip);
		}

		translated = true;
	}

	return TranslatedOptionsTable;
}

//! Early parsing of the commandline
/**
 * First half of the command line parsing. Also see ParseCommandLine()
 * below. The parameters here are needed early in the boot process,
 * while the ones in ParseCommandLine can benefit from debugging being
 * set up first.
 * \param argc number of arguments given
 * \param argv string array of the arguments
 * \return Returns true on success, false on error */
bool ParseCommandLineEarly(int argc, const char** argv)
{
	poptContext poptCon = poptGetContext(NULL, argc, argv, getOptionsTable(), 0);
	int iOption;

#if defined(WZ_OS_MAC) && defined(DEBUG)
	debug_enable_switch( "all" );
#endif /* WZ_OS_MAC && DEBUG */

	/* loop through command line */
	while ((iOption = poptGetNextOpt(poptCon)) > 0 || iOption == POPT_ERROR_BADOPT)
	{
		CLI_OPTIONS option = iOption;
		const char* token;

		if (iOption == POPT_ERROR_BADOPT)
		{
			debug(LOG_ERROR, "Unrecognized option: %s", poptBadOption(poptCon, 0));
			exit(1);
		}

		switch (option)
		{
			case CLI_DEBUG:
				// retrieve the debug section name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Usage: --debug <flag>");
					poptFreeContext(poptCon);
					return false;
				}

				// Attempt to enable the given debug section
				if (!debug_enable_switch(token))
				{
					debug(LOG_ERROR, "Debug flag \"%s\" not found!", token);
					poptFreeContext(poptCon);
					return false;
				}
				break;

			case CLI_DEBUGFILE:
				// find the file name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Missing debugfile filename?");
					poptFreeContext(poptCon);
					return false;
				}
				debug_register_callback( debug_callback_file, debug_callback_file_init, debug_callback_file_exit, (void*)token );
				break;

			case CLI_FLUSHDEBUGSTDERR:
				// Tell the debug stderr output callback to always flush its output
				debugFlushStderr();
				break;

			case CLI_CONFIGDIR:
				// retrieve the configuration directory
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Unrecognised configuration directory");
					poptFreeContext(poptCon);
					return false;
				}
				sstrcpy(configdir, token);
				break;

			case CLI_HELP:
				poptPrintHelp(poptCon, stdout, 0);
				poptFreeContext(poptCon);
				return false;

			case CLI_USAGE:
				poptPrintUsage(poptCon, stdout, 0);
				poptFreeContext(poptCon);
				return false;

			case CLI_VERSION:
				printf("Warzone 2100 - %s\n", version_getFormattedVersionString());
				poptFreeContext(poptCon);
				return false;

			default:
				break;
		};
	}

	poptFreeContext(poptCon);

	return true;
}

//! second half of parsing the commandline
/**
 * Second half of command line parsing. See ParseCommandLineEarly() for
 * the first half. Note that render mode must come before resolution flag.
 * \param argc number of arguments given
 * \param argv string array of the arguments
 * \return Returns true on success, false on error */
bool ParseCommandLine(int argc, const char** argv)
{
	poptContext poptCon = poptGetContext(NULL, argc, argv, getOptionsTable(), 0);
	int iOption;

	/* loop through command line */
	while ((iOption = poptGetNextOpt(poptCon)) > 0)
	{
		const char* token;
		CLI_OPTIONS option = iOption;

		switch (option)
		{
			case CLI_DEBUG:
			case CLI_DEBUGFILE:
			case CLI_FLUSHDEBUGSTDERR:
			case CLI_CONFIGDIR:
			case CLI_HELP:
			case CLI_USAGE:
			case CLI_VERSION:
				// These options are parsed in ParseCommandLineEarly() already, so ignore them
				break;

			case CLI_NOASSERT:
				kf_NoAssert();
				break;

			// NOTE: The sole purpose of this is to test the crash handler.
			case CLI_CRASH:
				CauseCrash = true;
				NetPlay.bComms = false;
				sstrcpy(aLevelName, "CAM_3A");
				SetGameMode(GS_NORMAL);
				break;

			case CLI_CHEAT:
				printf("  ** DEBUG MODE UNLOCKED! **\n");
				bAllowDebugMode = true;
				break;

			case CLI_DATADIR:
				// retrieve the quoted path name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Unrecognised datadir");
					poptFreeContext(poptCon);
					return false;
				}
				sstrcpy(datadir, token);
				break;

			case CLI_FULLSCREEN:
				war_setFullscreen(true);
				break;
			case CLI_CONNECTTOIP:
				//get the ip we want to connect with, and go directly to join screen.
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "No IP/hostname given");
					poptFreeContext(poptCon);
					return false;
				}
				sstrcpy(iptoconnect, token);
				break;
			case CLI_HOSTLAUNCH:
				// go directly to host screen, bypass all others.
				hostlaunch = true;
				break;
			case CLI_GAME:
				// retrieve the game name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "No game name");
					poptFreeContext(poptCon);
					return false;
				}
				if (strcmp(token, "CAM_1A") && strcmp(token, "CAM_2A") && strcmp(token, "CAM_3A")
				    && strcmp(token, "TUTORIAL3") && strcmp(token, "FASTPLAY"))
				{
					debug(LOG_ERROR, "The game parameter requires one of the following keywords:");
					debug(LOG_ERROR, "CAM_1A, CAM_2A, CAM_3A, TUTORIAL3, or FASTPLAY");
					return false;
				}
				NetPlay.bComms = false;
				sstrcpy(aLevelName, token);
				SetGameMode(GS_NORMAL);
				break;
			case CLI_MOD_GLOB:
			{
				unsigned int i;

				// retrieve the file name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Missing mod name?");
					poptFreeContext(poptCon);
					return false;
				}

				// Find an empty place in the global_mods list
				for (i = 0; i < 100 && global_mods[i] != NULL; ++i);
				if (i >= 100 || global_mods[i] != NULL)
				{
					debug(LOG_ERROR, "Too many mods registered! Aborting!");
					poptFreeContext(poptCon);
					return false;
				}
				global_mods[i] = strdup(token);
				break;
			}
			case CLI_MOD_CA:
			{
				unsigned int i;

				// retrieve the file name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Missing mod name?");
					poptFreeContext(poptCon);
					return false;
				}

				// Find an empty place in the campaign_mods list
				for (i = 0; i < 100 && campaign_mods[i] != NULL; ++i);
				if (i >= 100 || campaign_mods[i] != NULL)
				{
					debug(LOG_ERROR, "Too many mods registered! Aborting!");
					poptFreeContext(poptCon);
					return false;
				}
				campaign_mods[i] = strdup(token);
				break;
			}
			case CLI_MOD_MP:
			{
				unsigned int i;

				// retrieve the file name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Missing mod name?");
					poptFreeContext(poptCon);
					return false;
				}

				for (i = 0; i < 100 && multiplay_mods[i] != NULL; ++i);
				if (i >= 100 || multiplay_mods[i] != NULL)
				{
					debug(LOG_ERROR, "Too many mods registered! Aborting!");
					poptFreeContext(poptCon);
					return false;
				}
				multiplay_mods[i] = strdup(token);
				break;
			}
			case CLI_RESOLUTION:
			{
				unsigned int width, height;

				token = poptGetOptArg(poptCon);
				if (sscanf(token, "%ix%i", &width, &height ) != 2 )
				{
					debug(LOG_ERROR, "Invalid parameter specified (format is WIDTHxHEIGHT, e.g. 800x600)");
					return false;
				}
				if (width < 640) {
					debug(LOG_ERROR, "Screen width < 640 unsupported, using 640");
					width = 640;
				}
				if (height < 480) {
					debug(LOG_ERROR, "Screen height < 480 unsupported, using 480");
					height = 480;
				}
				// tell the display system of the desired resolution
				pie_SetVideoBufferWidth(width);
				pie_SetVideoBufferHeight(height);
				// and update the configuration
				war_SetWidth(width);
				war_SetHeight(height);
				break;
			}
			case CLI_SAVEGAME:
				// retrieve the game name
				token = poptGetOptArg(poptCon);
				if (token == NULL)
				{
					debug(LOG_ERROR, "Unrecognised savegame name");
					poptFreeContext(poptCon);
					return false;
				}
				snprintf(saveGameName, sizeof(saveGameName), "%s/%s", SaveGamePath, token);
				SetGameMode(GS_SAVEGAMELOAD);
				break;

			case CLI_WINDOW:
				war_setFullscreen(false);
				break;

			case CLI_SHADOWS:
				setDrawShadows(true);
				break;

			case CLI_NOSHADOWS:
				setDrawShadows(false);
				break;

			case CLI_SOUND:
				war_setSoundEnabled(true);
				break;

			case CLI_NOSOUND:
				war_setSoundEnabled(false);
				break;

			case CLI_SELFTEST:
				selfTest = true;
				break;
		};
	}

	poptFreeContext(poptCon);

	return true;
}
