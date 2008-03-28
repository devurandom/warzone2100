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

#include "lib/framework/frame.h"

#include <SDL.h>
#include <SDL_opengl.h>

#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/piemode.h"
#include "piematrix.h"
#include "lib/ivis_common/ivi.h"
#include "lib/ivis_common/piefunc.h"
#include "lib/ivis_common/tex.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/pieclip.h"
#include "screen.h"


/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

BOOL pie_Initialise(void)
{
	pie_TexInit();

	rendSurface.flags = REND_SURFACE_UNDEFINED;
	rendSurface.buffer = NULL;
	rendSurface.size = 0;

	/* Find texture compression extension */
	if (check_extension("GL_ARB_texture_compression"))
	{
		debug(LOG_TEXTURE, "Texture compression: Yes");
		wz_texture_compression = GL_COMPRESSED_RGBA_ARB;
	} else {
		debug(LOG_TEXTURE, "Texture compression: No");
		wz_texture_compression = GL_RGBA;
	}

	pie_MatInit();
	_TEX_INDEX = 0;

	rendSurface.buffer	= 0;
	rendSurface.flags	= REND_SURFACE_SCREEN;
	rendSurface.width	= pie_GetVideoBufferWidth();
	rendSurface.height	= pie_GetVideoBufferHeight();
	rendSurface.xcentre	= pie_GetVideoBufferWidth()/2;
	rendSurface.ycentre	= pie_GetVideoBufferHeight()/2;
	rendSurface.clip.left	= 0;
	rendSurface.clip.top	= 0;
	rendSurface.clip.right	= pie_GetVideoBufferWidth();
	rendSurface.clip.bottom	= pie_GetVideoBufferHeight();
	rendSurface.xpshift	= 10;
	rendSurface.ypshift	= 10;

	pie_SetDefaultStates();
	iV_RenderAssign(&rendSurface);

	return true;
}


void pie_ShutDown(void) {
	rendSurface.buffer = NULL;
	rendSurface.flags = REND_SURFACE_UNDEFINED;
	rendSurface.size = 0;

	pie_CleanUp();
}

/***************************************************************************/

void pie_ScreenFlip(int clearMode)
{
	GLbitfield clearFlags = 0;

	screenDoDumpToDiskIfRequired();
	SDL_GL_SwapBuffers();
	if (!(clearMode & CLEAR_OFF_AND_NO_BUFFER_DOWNLOAD))
	{
		glDepthMask(GL_TRUE);
		clearFlags = GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT;
		if (clearMode & CLEAR_SHADOW)
		{
			clearFlags |= GL_STENCIL_BUFFER_BIT;
		}
	}
	if (clearFlags)
	{
		glClear(clearFlags);
	}
	if (screen_GetBackDrop())
	{
		screen_Upload(NULL);
	}
}

/***************************************************************************/
UDWORD	pie_GetResScalingFactor(void)
{
	if (pie_GetVideoBufferWidth() * 4 > pie_GetVideoBufferHeight() * 5) {
		return pie_GetVideoBufferHeight()*5/4/6;
	} else {
		return pie_GetVideoBufferWidth()/6;
	}
}

