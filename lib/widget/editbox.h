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
 * EditBox.h
 *
 * Definitions for the edit box functions.
 */
#ifndef _editbox_h
#define _editbox_h

#include "widgbase.h"

/* Edit Box states */
#define WEDBS_FIXED		0x0001		// No editing is going on
#define WEDBS_INSERT	0x0002		// Insertion editing
#define WEDBS_OVER		0x0003		// Overwrite editing
#define WEDBS_MASK		0x000f		// 
#define WEDBS_HILITE	0x0010		//
#define WEDBS_DISABLE   0x0020		// disable button from selection

typedef struct _w_editbox
{
	/* The common widget data */
	WIDGET_BASE;

	UDWORD		state;						// The current edit box state
	char		aText[WIDG_MAXSTR];			// The text in the edit box
	int FontID;
	UWORD		insPos;						// The insertion point in the buffer
	UWORD		printStart;					// Where in the string appears at the far left of the box
	UWORD		printChars;					// The number of characters appearing in the box
	UWORD		printWidth;					// The pixel width of the characters in the box
	WIDGET_DISPLAY	pBoxDisplay;			// Optional callback to display the edit box background.
	FONT_DISPLAY pFontDisplay;				// Optional callback to display a string.
	SWORD HilightAudioID;					// Audio ID for form clicked sound
	SWORD ClickedAudioID;					// Audio ID for form hilighted sound
	WIDGET_AUDIOCALLBACK AudioCallback;		// Pointer to audio callback function
} W_EDITBOX;

/* Create an edit box widget data structure */
extern W_EDITBOX* editBoxCreate(const W_EDBINIT* psInit);

/* Free the memory used by an edit box */
extern void editBoxFree(W_EDITBOX *psWidget);

/* Initialise an edit box widget */
extern void editBoxInitialise(W_EDITBOX *psWidget);

/* Set the current string for the edit box */
extern void editBoxSetString(W_EDITBOX *psWidget, const char *pText);

/* Respond to loss of focus */
extern void editBoxFocusLost(W_SCREEN* psScreen, W_EDITBOX *psWidget);

/* Run an edit box widget */
extern void editBoxRun(W_EDITBOX *psWidget, W_CONTEXT *psContext);

/* Respond to a mouse click */
extern void editBoxClicked(W_EDITBOX *psWidget, W_CONTEXT *psContext);

/* Respond to a mouse button up */
extern void editBoxReleased(W_EDITBOX *psWidget);

/* Respond to a mouse moving over an edit box */
extern void editBoxHiLite(W_EDITBOX *psWidget);

/* Respond to the mouse moving off an edit box */
extern void editBoxHiLiteLost(W_EDITBOX *psWidget);

/* The edit box display function */
extern void editBoxDisplay(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

/* set state of edit box */
extern void editBoxSetState(W_EDITBOX *psEditBox, UDWORD state);

#endif
