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
 * pieTypes.h
 *
 * type defines for simple pies.
 *
 */
/***************************************************************************/

#ifndef _pieTypes_h
#define _pieTypes_h

#include "lib/framework/frame.h"

#include "pievector.h"

//*************************************************************************
//
// Simple derived types
//
//*************************************************************************
typedef struct { Vector3i p, r; } iView;

typedef struct { unsigned char r, g, b; } iColour;

typedef unsigned char iBitmap;
typedef struct { unsigned int width, height, depth; unsigned char *bmp; } iV_Image;
typedef iV_Image iTexture;

#endif // _pieTypes_h
