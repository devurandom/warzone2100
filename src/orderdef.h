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
//
// orderdef.h
//
// order releated structures.

#ifndef _orderdef_h
#define _orderdef_h

// data for barbarians retreating
typedef struct _run_data
{
	Vector2i    sPos; // position to retreat to
	uint8_t     forceLevel; // number of units below which might run
	uint8_t     healthLevel; // %health value below which to turn and run - FOR GROUPS ONLY
	uint8_t     leadership; // basic chance to run
} RUN_DATA;

typedef struct _droid_order_data
{
	SDWORD			order;
	UWORD			x,y;
	UWORD			x2,y2;
	//Watermelon:multiple target info;
	BASE_OBJECT		*psObj[DROID_MAXWEAPS];
	BASE_STATS		*psStats;
} DROID_ORDER_DATA;


extern RUN_DATA asRunData[MAX_PLAYERS]; // retreat positions for the players
extern void orderDroidBase(DROID *psDroid, DROID_ORDER_DATA *psOrder);

#endif
