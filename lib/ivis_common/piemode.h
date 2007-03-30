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
 * pieMode.h
 *
 * renderer control for pumpkin library functions.
 *
 */
/***************************************************************************/

#ifndef _pieMode_h
#define _pieMode_h

/***************************************************************************/

#include "lib/framework/frame.h"


/***************************************************************************/
/*
 *	Global Definitions
 */
/***************************************************************************/
#define CLEAR_MODE_MASK                   0x03
#define CLEAR_OFF                         0x00
#define CLEAR_OFF_AND_NO_BUFFER_DOWNLOAD  0x01
#define CLEAR_BLACK                       0x02
#define CLEAR_FOG                         0x03

#define CLEAR_SHADOW_MASK                 0x04
#define CLEAR_SHADOW                      0x04

/***************************************************************************/
/*
 *	Global Variables
 */
/***************************************************************************/

extern Sint32	_iVPRIM_DIVTABLE[];

/***************************************************************************/
/*
 *	Global ProtoTypes
 */
/***************************************************************************/
extern BOOL pie_Initialise(void);
extern void pie_ShutDown(void);
extern void pie_ScreenFlip(int ClearMode);
extern UDWORD	pie_GetResScalingFactor( void );

#endif
