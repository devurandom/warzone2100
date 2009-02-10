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

#ifndef __INCLUDED_SRC_LOADSAVE_H__
#define __INCLUDED_SRC_LOADSAVE_H__

/***************************************************************************/
/*
 *	Global Definitions
 */
/***************************************************************************/

typedef enum _loadsave_mode
{
LOAD_FRONTEND,
LOAD_MISSIONEND,
SAVE_MISSIONEND,
LOAD_INGAME,
SAVE_INGAME,
LOAD_FORCE,
SAVE_FORCE
}LOADSAVE_MODE;

/***************************************************************************/
/*
 *	Global Variables
 */
/***************************************************************************/

extern BOOL		bLoadSaveUp;							// true when interface is up and should be run.
//the name of the save game to load from the front end
extern char saveGameName[256];
extern char	sRequestResult[PATH_MAX];
extern BOOL		bRequestLoad;

/***************************************************************************/
/*
 *	Global ProtoTypes
 */
/***************************************************************************/

extern void		drawBlueBox		(UDWORD x,UDWORD y, UDWORD w, UDWORD h);

extern BOOL		addLoadSave(LOADSAVE_MODE mode, const char *defaultdir, const char *extension, const char *title);
extern BOOL		closeLoadSave	(void);
extern BOOL		runLoadSave		(BOOL bResetMissionWidgets);
extern BOOL		displayLoadSave	(void);

extern void		removeWildcards	(char *pStr);

// return whether the save screen was displayed in the mission results screen
BOOL saveInMissionRes(void);

// return whether the save screen was displayed in the middle of a mission
BOOL saveMidMission(void);


extern void deleteSaveGame(char* saveGameName);

#endif // __INCLUDED_SRC_LOADSAVE_H__
