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
/** \file
 *  Renderer setup and state control routines for 3D rendering.
 */

#include "lib/ivis_opengl/GLee.h"
#include "lib/framework/frame.h"

#include <SDL.h>
#include <SDL_mouse.h>

#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/tex.h"
#include "lib/ivis_common/piepalette.h"
#include "lib/ivis_common/rendmode.h"

/*
 *	Global Variables
 */

// Variables for the coloured mouse cursor
static CURSOR MouseCursor = 0;
static bool ColouredMouse = false;
static IMAGEFILE* MouseCursors = NULL;
static uint16_t MouseCursorIDs[CURSOR_MAX];
static bool MouseVisible = true;

extern RENDER_STATE	rendStates;

/*
 *	Source
 */

void pie_SetDepthBufferStatus(DEPTH_MODE depthMode)
{
	switch(depthMode)
	{
		case DEPTH_CMP_LEQ_WRT_ON:
			glEnable(GL_DEPTH_TEST);
			glDepthFunc(GL_LEQUAL);
			glDepthMask(GL_TRUE);
			break;

		case DEPTH_CMP_ALWAYS_WRT_ON:
			glDisable(GL_DEPTH_TEST);
			glDepthMask(GL_TRUE);
			break;

		case DEPTH_CMP_LEQ_WRT_OFF:
			glEnable(GL_DEPTH_TEST);
			glDepthFunc(GL_LEQUAL);
			glDepthMask(GL_FALSE);
			break;

		case DEPTH_CMP_ALWAYS_WRT_OFF:
			glDisable(GL_DEPTH_TEST);
			glDepthMask(GL_FALSE);
			break;
	}
}

/// Set the depth (z) offset
/// Negative values are closer to the screen
void pie_SetDepthOffset(float offset)
{
	if(offset == 0.0f)
	{
		glDisable (GL_POLYGON_OFFSET_FILL);
	}
	else
	{
		glPolygonOffset(offset, offset);
		glEnable (GL_POLYGON_OFFSET_FILL);
	}
}

/// Set the OpenGL fog start and end
void pie_UpdateFogDistance(float begin, float end)
{
	glFogf(GL_FOG_START, begin);
	glFogf(GL_FOG_END, end);
}

//
// pie_SetFogStatus(BOOL val)
//
// Toggle fog on and off for rendering objects inside or outside the 3D world
//

void pie_SetFogStatus(BOOL val)
{
	float fog_colour[4];

	if (rendStates.fogEnabled)
	{
		//fog enabled so toggle if required
		if (rendStates.fog != val)
		{
			rendStates.fog = val;
			if (rendStates.fog) {
				PIELIGHT fog = pie_GetFogColour();

				fog_colour[0] = fog.byte.r/255.0f;
				fog_colour[1] = fog.byte.g/255.0f;
				fog_colour[2] = fog.byte.b/255.0f;
				fog_colour[3] = fog.byte.a/255.0f;

				glFogi(GL_FOG_MODE, GL_LINEAR);
				glFogfv(GL_FOG_COLOR, fog_colour);
				glFogf(GL_FOG_DENSITY, 0.35f);
				glHint(GL_FOG_HINT, GL_DONT_CARE);
				glEnable(GL_FOG);
			} else {
				glDisable(GL_FOG);
			}
		}
	}
	else
	{
		//fog disabled so turn it off if not off already
		if (rendStates.fog != false)
		{
			rendStates.fog = false;
		}
	}
}

/** Selects a texture page and binds it for the current texture unit
 *  \param num a number indicating the texture page to bind. If this is a
 *         negative value (doesn't matter what value) it will disable texturing.
 */
void pie_SetTexturePage(SDWORD num)
{
	// Only bind textures when they're not bound already
	if (num != rendStates.texPage)
	{
		switch (num)
		{
			case TEXPAGE_NONE:
				glDisable(GL_TEXTURE_2D);
				break;
			case TEXPAGE_FONT:
				// GLC will set the texture, we just need to enable texturing
				glEnable(GL_TEXTURE_2D);
				break;
			default:
				if (rendStates.texPage == TEXPAGE_NONE || rendStates.texPage == TEXPAGE_FONT)
				{
					glEnable(GL_TEXTURE_2D);
				}
				glBindTexture(GL_TEXTURE_2D, _TEX_PAGE[num].id);
		}
		rendStates.texPage = num;
	}
}

void pie_SetAlphaTest(BOOL keyingOn)
{
	if (keyingOn != rendStates.keyingOn)
	{
		rendStates.keyingOn = keyingOn;
		pieStateCount++;

		if (keyingOn == true) {
			glEnable(GL_ALPHA_TEST);
			glAlphaFunc(GL_GREATER, 0.1f);
		} else {
			glDisable(GL_ALPHA_TEST);
		}
	}
}

void pie_SetTranslucencyMode(TRANSLUCENCY_MODE transMode)
{
	if (transMode != rendStates.transMode) {
		rendStates.transMode = transMode;
		switch (transMode) {
			case TRANS_ALPHA:
				glEnable(GL_BLEND);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
				break;
			case TRANS_ADDITIVE:
				glEnable(GL_BLEND);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE);
				break;
			default:
				rendStates.transMode = TRANS_DECAL;
				glDisable(GL_BLEND);
				break;
		}
	}
}

void pie_InitColourMouse(IMAGEFILE* img, const uint16_t cursorIDs[CURSOR_MAX])
{
	MouseCursors = img;
	memcpy(MouseCursorIDs, cursorIDs, sizeof(MouseCursorIDs));
}

/** Selects the given mouse cursor.
 *  \param cursor   mouse cursor to render
 *  \param coloured wether a coloured or black&white cursor should be used
 */
void pie_SetMouse(CURSOR cursor, bool coloured)
{
	ASSERT(cursor < CURSOR_MAX, "Attempting to load non-existent cursor: %u", (unsigned int)cursor);

	MouseCursor = cursor;

	frameSetCursor(MouseCursor);
	ColouredMouse = coloured;
}

/** Draws the current mouse cursor at the given coordinates
 *  \param X,Y mouse coordinates
 */
void pie_DrawMouse(unsigned int X, unsigned int Y)
{
	if (ColouredMouse && MouseVisible)
	{
		ASSERT(MouseCursors != NULL, "Drawing coloured mouse cursor while no coloured mouse cursors have been loaded yet!");

		iV_DrawImage(MouseCursors, MouseCursorIDs[MouseCursor], X, Y);
	}
}

/** Set the visibility of the mouse cursor */
void pie_ShowMouse(bool visible)
{
	MouseVisible = visible;
	if (MouseVisible && !ColouredMouse)
	{
		SDL_ShowCursor(SDL_ENABLE);
	}
	else
	{
		SDL_ShowCursor(SDL_DISABLE);
	}
}

