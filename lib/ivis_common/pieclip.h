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
 * pieclip.h
 *
 * clipping for all pumpkin image library functions.
 *
 */
/***************************************************************************/

#ifndef _pieclip_h
#define _pieclip_h

/***************************************************************************/

#include "lib/framework/frame.h"
#include "piedef.h"


/***************************************************************************/
/*
 *	Global Definitions
 */
/***************************************************************************/

#define CLIP_BORDER	0

/***************************************************************************/
/*
 *	Global ProtoTypes
 */
/***************************************************************************/
void pie_Set2DClip(int x0, int y0, int x1, int y1);
int	pie_ClipTextured(int npoints, PIEVERTEX *points, PIEVERTEX *clip);
BOOL	pie_SetVideoBufferDepth(UDWORD depth);
BOOL	pie_SetVideoBufferWidth(UDWORD width);
BOOL	pie_SetVideoBufferHeight(UDWORD height);
UDWORD	pie_GetVideoBufferDepth( void ) WZ_DECL_PURE;
UDWORD	pie_GetVideoBufferWidth( void ) WZ_DECL_PURE;
UDWORD	pie_GetVideoBufferHeight( void ) WZ_DECL_PURE;

#endif // _pieclip_h
