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
#ifndef _environ_h
#define _environ_h

// -------------------------------------------------------------------------------
extern BOOL	waterOnMap					( void );
extern BOOL environInit					( void );
extern void	environUpdate				( void );
extern UDWORD	environGetValue			( UDWORD x, UDWORD y );
extern UDWORD	environGetData			( UDWORD x, UDWORD y );
extern UDWORD map_MistValue				( UDWORD x, UDWORD y );
extern UDWORD map_TileMistValue(UDWORD x, UDWORD y);
extern void	environShutDown				( void );
//this function is called whenever the map changes - load new level or return from an offWorld map
extern void environReset(void);

// -------------------------------------------------------------------------------
#endif
