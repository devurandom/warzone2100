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
 *  Definitions for the Power Functionality.
 */

#ifndef __INCLUDED_SRC_POWER_H__
#define __INCLUDED_SRC_POWER_H__

/** Free power on collection of oildrum. */
#define OILDRUM_POWER		100

#define	POWER_PER_CYCLE		5

/** Used to determine the power cost of repairing a droid.
    Definately DON'T WANT the brackets round 1/2 - it will equate to zero! */
#define REPAIR_POWER_FACTOR 1/5

/** Used to multiply all repair calculations by to avaoid rounding errors. */
#define POWER_FACTOR        100

typedef struct _player_power
{
	float currentPower;         ///< The current amount of power avaialble to the player.
	float extractedPower;       ///< The power extracted but not converted.
	float powerProduced;        ///< Power produced
	float powerRequested;       ///< Power requested
	float economyThrottle;      ///< Which percentage of the requested power is actually delivered
} PLAYER_POWER;

/** Allocate the space for the playerPower. */
extern BOOL allocPlayerPower(void);

/** Clear the playerPower. */
extern void clearPlayerPower(void);

/** Reset the power levels when a power_gen or resource_extractor is destroyed. */
extern BOOL resetPlayerPower(UDWORD player, STRUCTURE *psStruct);

/** Free the space used for playerPower. */
extern void releasePlayerPower(void);

/** Check the available power. */
extern BOOL checkPower(int player, float quantity);

extern float requestPower(int player, float amount);
extern int requestPowerFor(int player, float amount, int points);

extern void addPower(int player, float quantity);
extern void addExtractedPower(int player, float quantity);

BOOL checkPower(int player, float quantity);
void usePower(int player, float quantity);

/** Update current power based on what was extracted during the last cycle and what Power Generators exist. */
extern void updatePlayerPower(UDWORD player);

/** Used in multiplayer to force power levels. */
extern void setPower(int player, float power);
extern void setExtractedPower(int player, float power);

/** Get the amount of power current held by the given player. */
extern float getPower(int player);
extern float getExtractedPower(int player);

/** Resets the power levels for all players when power is turned back on. */
void powerCalc(BOOL on);

/** Temp function to give all players some power when a new game has been loaded. */
void newGameInitPower(void);

/** Accrue the power in the facilities that require it. */
extern BOOL accruePower(BASE_OBJECT *psObject);

/**	Returns the next res. Ext. in the list from the one passed in. returns 1st one
	in list if passed in is NULL and NULL if there's none?
*/
extern STRUCTURE *getRExtractor(STRUCTURE *psStruct);

/** Defines which structure types draw power - returns true if use power. */
extern BOOL structUsesPower(STRUCTURE *psStruct);

/** Defines which droid types draw power - returns true if use power. */
extern BOOL droidUsesPower(DROID *psDroid);

/** Flag used to check for power calculations to be done or not. */
extern	BOOL			powerCalculated;

extern void updatePowerSystem(void);

#endif // __INCLUDED_SRC_POWER_H__
