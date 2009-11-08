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
 *  Data loading functions, deals with loading of all resource types.
 */

#ifndef __INCLUDED_SRC_DATA_H__
#define __INCLUDED_SRC_DATA_H__

// used for multiplayer data integrity check.
enum {
DATA_SWEAPON,
DATA_SBODY,
DATA_SCONSTR,
DATA_SECM,
DATA_SPROP,
DATA_SSENSOR,
DATA_SREPAIR,
DATA_SBRAIN,
DATA_SPROPTY,
DATA_STERRT,
DATA_SWEAPMOD,
DATA_STEMP,
DATA_STEMPWEAP,
DATA_SSTRUCT,
DATA_SSTRWEAP,
DATA_STRFUNC,
DATA_SSTRMOD,
DATA_SFEAT,
DATA_SFUNC,
DATA_RESCH,
DATA_RPREREQ,
DATA_RCOMPRED,
DATA_RSTRREQ,
DATA_RCOMPRES,
DATA_RSTRRED,
DATA_RSTRRES,
DATA_RFUNC,
DATA_SCRIPT,
DATA_SCRIPTVAL,
DATA_MAXDATA		// must be last
};
extern void resetDataHash(void);
extern UDWORD DataHash[DATA_MAXDATA];

/* Pass all the data loading functions to the framework library */
extern BOOL dataInitLoadFuncs(void);

extern void dataSetSaveFlag(void);
extern void dataClearSaveFlag(void);

#endif // __INCLUDED_SRC_DATA_H__
