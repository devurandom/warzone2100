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
/*
 * WidgInt.h
 *
 * Internal widget library definitions
 */
#ifndef _widgint_h
#define _widgint_h

#include "lib/framework/input.h"

/* Set the id number for widgRunScreen to return */
extern void widgSetReturn(WIDGET *psWidget);

/* Find a widget in a screen from its ID number */
extern WIDGET *widgGetFromID(W_SCREEN *psScreen, UDWORD id);

/* Release a list of widgets */
extern void widgReleaseWidgetList(WIDGET *psWidgets);

/* Call the correct function for mouse over */
extern void widgHiLite(WIDGET *psWidget, W_CONTEXT *psContext);

/* Call the correct function for mouse moving off */
extern void widgHiLiteLost(WIDGET *psWidget, W_CONTEXT *psContext);

/* Call the correct function for loss of focus */
extern void widgFocusLost(WIDGET *psWidget);

/* Set the keyboard focus for the screen */
extern void screenSetFocus(W_SCREEN *psScreen, WIDGET *psWidget);

/* Clear the keyboard focus */
extern void screenClearFocus(W_SCREEN *psScreen);

#endif

