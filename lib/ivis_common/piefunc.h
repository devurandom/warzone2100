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
 * piefunc.h
 *
 * type defines for extended image library functions.
 *
 */
/***************************************************************************/

#ifndef _piefunc_h
#define _piefunc_h

#include "lib/framework/frame.h"
#include "lib/ivis_common/piedef.h"

extern void pie_InitMaths(void);
extern UBYTE pie_ByteScale(UBYTE a, UBYTE b) WZ_DECL_CONST;
extern void	pie_TransColouredTriangle( PIEVERTEX *vrt, UDWORD rgb );
extern void pie_DrawSkybox(float scale, int u, int v, int w, int h);
extern void pie_DrawFogBox(float left, float right, float front, float back, float height, float wider);
extern void	pie_DrawViewingWindow( Vector3i *v, UDWORD x1, UDWORD y1, UDWORD x2, UDWORD y2, UDWORD colour);

#endif // _piedef_h
