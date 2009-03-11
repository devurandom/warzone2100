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
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/piepalette.h"
#include "lib/ivis_common/rendmode.h"
#include "screen.h"

PIELIGHT psPalette[WZCOL_MAX];

void pal_Init(void)
{
	// TODO: Read these from file so that mod-makers can change them
	WZCOL_WHITE = pal_Colour(UBYTE_MAX, UBYTE_MAX, UBYTE_MAX);
	WZCOL_BLACK = pal_Colour(1, 1, 1);
	WZCOL_GREEN = pal_Colour(0, UBYTE_MAX, 0);
	WZCOL_RED = pal_Colour(UBYTE_MAX, 0, 0);
	WZCOL_YELLOW = pal_Colour(UBYTE_MAX, UBYTE_MAX, 0);

	WZCOL_RELOAD_BAR	= WZCOL_WHITE;
	WZCOL_RELOAD_BACKGROUND	= WZCOL_BLACK;
	WZCOL_HEALTH_HIGH	= WZCOL_GREEN;
	WZCOL_HEALTH_MEDIUM	= WZCOL_YELLOW;
	WZCOL_HEALTH_LOW	= WZCOL_RED;
	WZCOL_CURSOR		= WZCOL_WHITE;

	WZCOL_MENU_BACKGROUND = pal_Colour(0, 1, 97);
	WZCOL_MENU_BORDER = pal_Colour(0, 21, 240);

	WZCOL_MENU_LOAD_BORDER		= WZCOL_BLACK;
	WZCOL_MENU_LOAD_BORDER.byte.r	= 133;

	WZCOL_MENU_SCORES_INTERIOR	= WZCOL_BLACK;
	WZCOL_MENU_SCORES_INTERIOR.byte.b = 33;

	WZCOL_MENU_SEPARATOR = pal_Colour(0x64, 0x64, 0xa0);

	WZCOL_TEXT_BRIGHT = WZCOL_WHITE;
	WZCOL_TEXT_MEDIUM.byte.r = 0.627451f * UBYTE_MAX;
	WZCOL_TEXT_MEDIUM.byte.g = 0.627451f * UBYTE_MAX;
	WZCOL_TEXT_MEDIUM.byte.b = UBYTE_MAX;
	WZCOL_TEXT_MEDIUM.byte.a = UBYTE_MAX;
	WZCOL_TEXT_DARK.byte.r = 0.376471f * UBYTE_MAX;
	WZCOL_TEXT_DARK.byte.g = 0.376471f * UBYTE_MAX;
	WZCOL_TEXT_DARK.byte.b = UBYTE_MAX;
	WZCOL_TEXT_DARK.byte.a = UBYTE_MAX;

	WZCOL_SCORE_BOX_BORDER = WZCOL_BLACK;
	WZCOL_SCORE_BOX = pal_Colour(0, 0, 88);
	WZCOL_SCORE_BOX.byte.a = 128;

	WZCOL_TOOLTIP_TEXT = WZCOL_WHITE;

	WZCOL_UNIT_SELECT_BORDER = pal_Colour(0, 0, 128);
	WZCOL_UNIT_SELECT_BOX = WZCOL_WHITE;
	WZCOL_UNIT_SELECT_BOX.byte.a = 16;

	WZCOL_RADAR_BACKGROUND = WZCOL_MENU_BACKGROUND;
	WZCOL_RADAR_BACKGROUND.byte.a = 0; // fully transparent

	WZCOL_MAP_OUTLINE_OK = WZCOL_WHITE;
	WZCOL_MAP_OUTLINE_BAD = WZCOL_RED;

	WZCOL_KEYMAP_ACTIVE = pal_Colour(0, 128, 0);
	WZCOL_KEYMAP_FIXED = pal_Colour(128, 0, 0);

	WZCOL_MENU_SCORE_LOSS = pal_Colour(255, 35, 0);
	WZCOL_MENU_SCORE_DESTROYED = pal_Colour(55, 239, 111);
	WZCOL_MENU_SCORE_BUILT = pal_Colour(39, 49, 185);
	WZCOL_MENU_SCORE_RANK = pal_Colour(235, 235, 19);

	WZCOL_FRAME_BORDER_NORMAL = pal_Colour(145, 0, 195);

	WZCOL_CONS_TEXT_SYSTEM = pal_Colour(200, 200, 200);
	WZCOL_CONS_TEXT_USER = WZCOL_TEXT_BRIGHT;
	WZCOL_CONS_TEXT_USER_ALLY = WZCOL_YELLOW;
	WZCOL_CONS_TEXT_USER_ENEMY = WZCOL_RED;
	WZCOL_CONS_TEXT_DEBUG = pal_Colour(150, 150, 150);
	WZCOL_MAP_PREVIEW_AIPLAYER = pal_Colour(0, 0x7f, 0);
	WZCOL_GREY = pal_Colour(0x55, 0x55, 0x55);
	WZCOL_MENU_SHADOW = WZCOL_BLACK;
	WZCOL_DBLUE = pal_Colour(0x0f,0x08,0x56);
	WZCOL_LBLUE = pal_Colour(0x1c,0x9f,0xfb);
	WZCOL_BLUEPRINT_VALID = pal_Colour(80,255,120);
	WZCOL_BLUEPRINT_INVALID = pal_Colour(255,80,80);
	WZCOL_BLUEPRINT_PLANNED = pal_Colour(50,160,75);
}

void pal_ShutDown(void)
{
	// placeholder
}
