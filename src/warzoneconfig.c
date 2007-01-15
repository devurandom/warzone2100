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
/***************************************************************************/
/*
 * warzoneConfig.c
 *
 * warzone Global configuration functions.
 *
 */
/***************************************************************************/

#include "lib/framework/frame.h"
#include "warzoneconfig.h"
#include "lib/ivis_common/piestate.h"
#include "advvis.h"

/***************************************************************************/
/*
 *	Global Variables
 */
/***************************************************************************/


/***************************************************************************/
/*
 *	Local Definitions
 */
/***************************************************************************/

typedef struct _warzoneGlobals
{
	SEQ_MODE	seqMode;
	BOOL		bFog;
	BOOL		bTranslucent;
	BOOL		bAdditive;
	SWORD		effectsLevel;
	BOOL		allowSubtitles;
	BOOL		playAudioCDs;
	BOOL		Fullscreen;
	BOOL		soundEnabled;
} WARZONE_GLOBALS;

/***************************************************************************/
/*
 *	Local Variables
 */
/***************************************************************************/

static WARZONE_GLOBALS	warGlobs;//STATIC use or write an access function if you need any of this

/***************************************************************************/
/*
 *	Local ProtoTypes
 */
/***************************************************************************/

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/
void war_SetDefaultStates(void)//Sets all states
{
	//set those here and reset in clParse or loadConfig
	pie_SetFogCap(FOG_CAP_UNDEFINED);
	war_SetFog(FALSE);
	war_SetTranslucent(TRUE);	// SHURCOOL: These two should be true (ie. enabled) by default; not false
	war_SetAdditive(TRUE);		// SHURCOOL: It means that the renderer should be allowed to use translucency/additive rendering modes

	war_SetPlayAudioCDs(TRUE);

	war_setSoundEnabled( TRUE );
}

void war_SetPlayAudioCDs(BOOL b) {
	warGlobs.playAudioCDs = b;
}

BOOL war_GetPlayAudioCDs(void) {
	return warGlobs.playAudioCDs;
}

void war_SetAllowSubtitles(BOOL b) {
	warGlobs.allowSubtitles = b;
}

BOOL war_GetAllowSubtitles(void) {
	return warGlobs.allowSubtitles;
}

void war_setFullscreen(BOOL b) {
	warGlobs.Fullscreen = b;
}

BOOL war_getFullscreen(void) {
	return warGlobs.Fullscreen;
}

/***************************************************************************/
/***************************************************************************/
void war_SetFog(BOOL val)
{
	if (warGlobs.bFog != val)
	{
		warGlobs.bFog = val;
	}
	if (warGlobs.bFog == TRUE)
	{
		setRevealStatus(FALSE);
	}
	else
	{
		setRevealStatus(TRUE);
		pie_SetFogColour(0);
	}
}

BOOL war_GetFog(void)
{
	return  warGlobs.bFog;
}

/***************************************************************************/
/***************************************************************************/
void war_SetTranslucent(BOOL val)
{
	pie_SetTranslucent(val);
	if (warGlobs.bTranslucent != val)
	{
		warGlobs.bTranslucent = val;
	}
}

BOOL war_GetTranslucent(void)
{
	return  warGlobs.bTranslucent;
}

/***************************************************************************/
/***************************************************************************/
void war_SetAdditive(BOOL val)
{
	pie_SetAdditive(val);
	if (warGlobs.bAdditive != val)
	{
		warGlobs.bAdditive = val;
	}
}

BOOL war_GetAdditive(void)
{
	return  warGlobs.bAdditive;
}

/***************************************************************************/
/***************************************************************************/
void war_SetSeqMode(SEQ_MODE mode)
{
	warGlobs.seqMode = mode;
}

SEQ_MODE war_GetSeqMode(void)
{
	return  warGlobs.seqMode;
}


void war_setSoundEnabled( BOOL soundEnabled )
{
	warGlobs.soundEnabled = soundEnabled;
}

BOOL war_getSoundEnabled( void )
{
	return warGlobs.soundEnabled;
}
