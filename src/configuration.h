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
/** @file
 *  load and save favourites to the config file
 */

#ifndef __INCLUDED_SRC_CONFIGURATION_H__
#define __INCLUDED_SRC_CONFIGURATION_H__

extern BOOL loadConfig(void);
extern BOOL loadRenderMode(void);
extern BOOL saveConfig(void);
extern void closeConfig( void );
extern void setSinglePlayerFrameLimit(SDWORD limit);
extern SDWORD getSinglePlayerFrameLimit(void);
extern void setDefaultFrameRateLimit(void);

/* Frame limit for multiplayer games (excluding skirmish and campaign) */
#define	MP_FRAME_LIMIT	45

/* Default frame limit for single player: skirmish ans campaign */
#define	SP_FRAME_LIMIT	60

/// Default map for Mayhem
#define DEFAULTCAMPAIGNMAP	"Rush"
/// Default map for Skirmish
#define DEFAULTSKIRMISHMAP	"Sk-Rush"


#endif // __INCLUDED_SRC_CONFIGURATION_H__
