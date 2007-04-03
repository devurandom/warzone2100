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
 * Label.h
 *
 * Definitions for the label widget.
 */
#ifndef _label_h
#define _label_h

/* The widget heaps */
extern OBJ_HEAP	*psLabHeap;

// label states.
#define WLABEL_HILITE	0x0004		// label is hilited

typedef struct _w_label
{
	/* The common widget data */
	WIDGET_BASE;

	UDWORD		state;					// The current button state
	char		aText[WIDG_MAXSTR];		// Text on the label
	int FontID;
	char		*pTip;					// The tool tip for the button
} W_LABEL;

/* Create a button widget data structure */
extern BOOL labelCreate(W_LABEL **ppsWidget, W_LABINIT *psInit);

/* Free the memory used by a button */
extern void labelFree(W_LABEL *psWidget);

/* label display function */
extern void labelDisplay(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);

/* Respond to a mouse moving over a label */
extern void labelHiLite(W_LABEL *psWidget, W_CONTEXT *psContext);

/* Respond to the mouse moving off a label */
extern void labelHiLiteLost(W_LABEL *psWidget);

#endif

