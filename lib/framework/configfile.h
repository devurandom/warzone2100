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
/*! \file configfile.h
 * \brief load and save favourites to the registry.
 */

#ifndef _INCLUDE_LIB_FRAMEWORK_CONFIGFILE_H_
#define _INCLUDE_LIB_FRAMEWORK_CONFIGFILE_H_

#include "types.h"

extern void registry_clear(void);
extern bool openWarzoneKey(void);
extern bool closeWarzoneKey(void);
extern bool getWarzoneKeyNumeric(const char* key, int* val);
extern bool setWarzoneKeyNumeric(const char* key, int val);
extern bool getWarzoneKeyString(const char* key, char* val);
extern bool setWarzoneKeyString(const char* key, const char* val);
extern void setRegistryFilePath(const char* fileName);

#endif // _INCLUDE_LIB_FRAMEWORK_CONFIGFILE_H_
