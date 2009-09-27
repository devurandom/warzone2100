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
 *  Interface to the combat routines.
 */

#ifndef __INCLUDED_SRC_COMBAT_H__
#define __INCLUDED_SRC_COMBAT_H__

#include "lib/framework/frame.h"
#include "objects.h"

/* The range out of which the random number for the to hit should be taken */
#define HIT_DICE	100

/* set a variable to the role of a die between 0 and HIT_DICE */
#define HIT_ROLL(d)  (d) = rand() % HIT_DICE

// maximum difference in direction for a fixed turret to fire
#define FIXED_TURRET_DIR	1

// %age at which a unit is considered to be heavily damaged
#define HEAVY_DAMAGE_LEVEL	25

/* who specified the target? */
#define ORIGIN_UNKNOWN				0	///< Default value if unknown
#define ORIGIN_PLAYER				1	///< Came directly from player (droids only)
#define ORIGIN_VISUAL				2	///< I can see you clearly :-)
#define ORIGIN_ALLY 				3	///< Came from allied unit/structure  (droids only?)
#define ORIGIN_COMMANDER			4	///< Came from commander
#define ORIGIN_SENSOR				5	///< Came from standard sensor
#define ORIGIN_CB_SENSOR			6	///< Came from counter-battery sensor
#define ORIGIN_AIRDEF_SENSOR		7	///< Came from Air Defense sensor
#define ORIGIN_RADAR_DETECTOR		8	///< Came from Radar Detector sensor

/* Initialise the combat system */
extern BOOL combInitialise(void);

/* Shutdown the combat system */
extern BOOL combShutdown(void);

/* Fire a weapon at something added int weapon_slot*/
extern void combFire(WEAPON *psWeap, BASE_OBJECT *psAttacker, BASE_OBJECT *psTarget, int weapon_slot);

/*checks through the target players list of structures and droids to see
if any support a counter battery sensor*/
extern void counterBatteryFire(BASE_OBJECT *psAttacker, BASE_OBJECT *psTarget);

extern float objDamage(BASE_OBJECT *psObj, UDWORD damage, UDWORD originalhp, UDWORD weaponClass,
                       UDWORD weaponSubClass, HIT_SIDE impactSide);

#endif // __INCLUDED_SRC_COMBAT_H__
