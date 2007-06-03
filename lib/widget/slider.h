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
 * Slider.h
 *
 * Slider bar interface definitions.
 */
#ifndef _slider_h
#define _slider_h

/* The widget heaps */
extern OBJ_HEAP	*psSldHeap;

/* Slider state */
#define SLD_DRAG		0x0001		// Slider is being dragged
#define SLD_HILITE		0x0002		// Slider is hilited

typedef struct _w_slider
{
	/* The common widget data */
	WIDGET_BASE;

	UWORD		orientation;		// The orientation of the slider
	UWORD		numStops;			// Number of stop positions on the slider
	UWORD		barSize;			// Thickness of slider bar
	UWORD		pos;				// Current stop position of the slider
	UWORD		state;				// Slider state
	const char	*pTip;				// Tool tip
} W_SLIDER;

/* Create a slider widget data structure */
extern BOOL sliderCreate(W_SLIDER **ppsWidget, W_SLDINIT *psInit);

/* Free the memory used by a slider */
extern void sliderFree(W_SLIDER *psWidget);

/* Initialise a slider widget before running it */
extern void sliderInitialise(W_SLIDER *psWidget);

/* Run a slider widget */
extern void sliderRun(W_SLIDER *psWidget, W_CONTEXT *psContext);

/* Respond to a mouse click */
extern void sliderClicked(W_SLIDER *psWidget, W_CONTEXT *psContext);

/* Respond to a mouse up */
extern void sliderReleased(W_SLIDER *psWidget);

/* Respond to a mouse moving over a slider */
extern void sliderHiLite(W_SLIDER *psWidget);

/* Respond to the mouse moving off a slider */
extern void sliderHiLiteLost(W_SLIDER *psWidget);

/* The slider display function */
extern void sliderDisplay(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset,
						  UDWORD *pColours);


#endif

