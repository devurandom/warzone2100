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
/**
 * @file cheat.c
 * Handles cheat codes for Warzone.
 */
/* Alex M 19th - Jan. 1999 */

#include "lib/framework/frame.h"
#include "lib/framework/string_ext.h"
#include "lib/exceptionhandler/dumpinfo.h"
#include "cheat.h"
#include "console.h"
#include "keybind.h"

typedef struct _cheat_entry
{
	const char *pName;
	void (*function)(void);	// pointer to void* function
} CHEAT_ENTRY;

bool Cheated = false;
static CHEAT_ENTRY cheatCodes[] =
{
//	{"VQKZMY^\\Z",kf_ToggleOverlays},//interface
//	{"LWPH R^OOVQXL",kf_ShowMappings},//show mappings
//	{"KZROS^KZL",kf_GiveTemplateSet},//templates
//	{"LZSZ\\K ^SS",kf_SelectAllCombatUnits},//select all
//	{"SZK KWZMZ ]Z SVXWK",kf_RecalcLighting},//let there be light
//	{"PJKSVQZ,",kf_ToggleOutline},
//	{"L\\MZZQ[JRO",kf_ScreenDump},	//screendump

	{"noassert", kf_NoAssert}, // turn off asserts
	{"count me", kf_ShowNumObjects}, // give a count of objects in the world
	{"give all", kf_AllAvailable},	// give all
	{"research all", kf_FinishAllResearch}, // research everything at once
	{"superpower", kf_MaxPower}, // get tons of power
	{"more power", kf_UpThePower}, // get tons of power
	{"deity", kf_ToggleGodMode},	//from above
	{"droidinfo", kf_DebugDroidInfo},	//show unit stats
	{"sensors", kf_ToggleSensorDisplay},	//show sensor ranges
	{"let me win", kf_AddMissionOffWorld},	//let me win
	{"timedemo", kf_FrameRate},	 //timedemo
	{"kill", kf_KillSelected},	//kill slected
	{"demo", kf_ToggleDemoMode},	//demo mode
	{"john kettley", kf_ToggleWeather},	//john kettley
	{"shakey", kf_ToggleShakeStatus},	//shakey
	{"mouseflip", kf_ToggleMouseInvert},	//mouseflip
	{"biffer baker", kf_SetKillerLevel},	//indestructive units
	{"easy", kf_SetEasyLevel},	//easy
	{"normal", kf_SetNormalLevel},	//normal
	{"hard", kf_SetHardLevel},	//hard
	{"double up", kf_SetToughUnitsLevel},	// your units take half the damage
	{"whale fin", kf_TogglePower},	// turns on/off infinte power
	{"get off my land", kf_KillEnemy},	// kills all enemy units and structures
	{"build info", kf_BuildInfo},	// tells you when the game was built
	{"time toggle", kf_ToggleMissionTimer},
	{"work harder", kf_FinishResearch},
	{"tileinfo", kf_TileInfo}, // output debug info about a tile
	{"showfps", kf_ToggleFPS},	//displays your average FPS
	{"showsamples", kf_ToggleSamples}, //displays the # of Sound samples in Queue & List
	{"showorders", kf_ToggleOrders}, //displays unit order/action state.
};

BOOL attemptCheatCode(const char* cheat_name)
{
	const CHEAT_ENTRY * curCheat;
	static const CHEAT_ENTRY * const EndCheat = &cheatCodes[ARRAY_SIZE(cheatCodes)];

	for (curCheat = cheatCodes; curCheat != EndCheat; ++curCheat)
	{
		if (strcmp(cheat_name, curCheat->pName) == 0)
		{
			char buf[256];

			/* We've got our man... */
			curCheat->function();	// run it

			// Copy this info to be used by the crash handler for the dump file
			ssprintf(buf, "User has used cheat code: %s", curCheat->pName);
			addDumpInfo(buf);

			/* And get out of here */
			Cheated = true;
			return true;
		}
	}

	/* We didn't find it. Report only for single player games. */
	if (!runningMultiplayer())
	{
		char	errorString[255];

		sstrcpy(errorString, cheat_name);
		sstrcat(errorString, "?");

		addConsoleMessage(errorString, LEFT_JUSTIFY,SYSTEM_MESSAGE);
	}

	return false;
}
