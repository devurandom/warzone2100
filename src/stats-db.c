/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
	Copyright (C) 2005-2008  Warzone Resurrection Project
	Copyright (C) 2008  Giel van Schijndel

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
 *  Store/Load common stats for weapons, components, brains, etc.
 */

#include "lib/framework/frame.h"
#include "lib/framework/frameresource.h"
#include "stats-db.h"
#include "stats.h"
#include "main.h"
#include "projectile.h"
#include "lib/sound/audio_id.h"
#include "lib/sqlite3/sqlite3.h"

static bool prepareStatement(sqlite3* db, const char* statement, sqlite3_stmt** stmt)
{
	int rc = sqlite3_prepare_v2(db, statement, -1, stmt, NULL);

	if (rc != SQLITE_OK)
	{
		debug(LOG_ERROR, "prepareStatement: SQL error: %s", sqlite3_errmsg(db));
		return false;
	}

	return true;
}

/** Load the weapon stats from the given SQLite database file
 *  \param filename name of the database file to load the weapon stats from.
 */
bool loadWeaponStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `weapons`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of weapons we're about to fetch. Then make sure
	 * to allocate memory for that amount of weapons. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocWeapons(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`hitpoints`,"
	                                 "`systempoints`,"
	                                 "`body`,"
	                                 "`GfxFile`,"
	                                 "`mountGfx`,"
	                                 "`muzzleGfx`,"
	                                 "`flightGfx`,"
	                                 "`hitGfx`,"
	                                 "`missGfx`,"
	                                 "`waterGfx`,"
	                                 "`trailGfx`,"
	                                 "`short_range`,"
	                                 "`long_range`,"
	                                 "`short_range_accuracy`,"
	                                 "`long_range_accuracy`,"
	                                 "`firePause`,"
	                                 "`numExplosions`,"
	                                 "`rounds_per_salvo`,"
	                                 "`reload_time_per_salvo`,"
	                                 "`damage`,"
	                                 "`radius`,"
	                                 "`radiusHit`,"
	                                 "`radiusDamage`,"
	                                 "`incenTime`,"
	                                 "`incenDamage`,"
	                                 "`incenRadius`,"
	                                 "`directLife`,"
	                                 "`radiusLife`,"
	                                 "`flightSpeed`,"
	                                 "`indirectHeight`,"
	                                 "`fireOnMove`,"
	                                 "`weaponClass`,"
	                                 "`weaponSubClass`,"
	                                 "`movement`,"
	                                 "`weaponEffect`,"
	                                 "`rotate`,"
	                                 "`maxElevation`,"
	                                 "`minElevation`,"
	                                 "`facePlayer`,"
	                                 "`faceInFlight`,"
	                                 "`recoilValue`,"
	                                 "`minRange`,"
	                                 "`lightWorld`,"
	                                 "`effectSize`,"
	                                 "`surfaceToAir`,"
	                                 "`numAttackRuns`,"
	                                 "`designable`,"
	                                 "`penetrate` "
	                          "FROM `weapons`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		WEAPON_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int weapon_id = sqlite3_column_int(stmt, colnum++);
		const char* str;
		unsigned int longRange;

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_WEAPON_START + weapon_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// hitpoints             NUMERIC NOT NULL, -- Component's hitpoints - SEEMS TO BE UNUSED
		stats->hitPoints = sqlite3_column_double(stmt, colnum++);
		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);
		// body                  NUMERIC NOT NULL, -- Component's body points
		stats->body = sqlite3_column_double(stmt, colnum++);

		// Get the IMD for the component
		// GfxFile               TEXT,             -- The IMD to draw for this component
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the weapon PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pIMD = NULL;
			++colnum;
		}

		// Get the rest of the IMDs
		// mountGfx              TEXT,             -- The turret mount to use
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pMountGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pMountGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the mount PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pMountGraphic = NULL;
			++colnum;
		}

		if(GetGameMode() == GS_NORMAL)
		{
			// muzzleGfx             TEXT,             -- The muzzle flash
			stats->pMuzzleGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pMuzzleGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the muzzle PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}

			// flightGfx             TEXT,             -- The ammo in flight
			stats->pInFlightGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pInFlightGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the flight PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}

			// hitGfx                TEXT,             -- The ammo hitting a target
			stats->pTargetHitGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pTargetHitGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the target hit PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}

			// missGfx               TEXT,             -- The ammo missing a target
			stats->pTargetMissGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pTargetMissGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the target miss PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}

			// waterGfx              TEXT,             -- The ammo hitting water
			stats->pWaterHitGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pWaterHitGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the water hit PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}

			// trailGfx              TEXT,             -- The trail used for in flight
			// Trail graphic can be null
			if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
			{
				stats->pTrailGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
				if (stats->pTrailGraphic == NULL)
				{
					debug( LOG_ERROR, "Cannot find the trail PIE for record %s", getStatName(stats) );
					abort();
					goto in_statement_err;
				}
			}
			else
			{
				stats->pTrailGraphic = NULL;
				++colnum;
			}
		}
		else
		{
			colnum += 6;
		}

		// short_range           NUMERIC NOT NULL, -- Max distance to target for short range shot
		stats->shortRange = sqlite3_column_double(stmt, colnum++);
		// long_range            NUMERIC NOT NULL, -- Max distance to target for long range shot
		stats->longRange = sqlite3_column_double(stmt, colnum++);
		// short_range_accuracy  NUMERIC NOT NULL, -- Chance to hit at short range
		stats->shortHit = sqlite3_column_double(stmt, colnum++);
		// long_range_accuracy   NUMERIC NOT NULL, -- Chance to hit at long range
		stats->longHit = sqlite3_column_double(stmt, colnum++);
		// firePause             NUMERIC NOT NULL, -- Time between each weapon fire
		stats->firePause = sqlite3_column_double(stmt, colnum++);

		// numExplosions         INTEGER NOT NULL, -- The number of explosions per shot
		stats->numExplosions = sqlite3_column_int(stmt, colnum++);
		// rounds_per_salvo      INTEGER NOT NULL, -- The number of rounds per salvo(magazine)
		stats->numRounds = sqlite3_column_int(stmt, colnum++);

		// reload_time_per_salvo NUMERIC NOT NULL, -- Time to reload the round of ammo (salvo fire)
		stats->reloadTime = sqlite3_column_double(stmt, colnum++);
		// damage                NUMERIC NOT NULL, -- How much damage the weapon causes
		stats->damage = sqlite3_column_double(stmt, colnum++);
		// radius                NUMERIC NOT NULL, -- Basic blast radius of weapon
		stats->radius = sqlite3_column_double(stmt, colnum++);
		// radiusHit             NUMERIC NOT NULL, -- Chance to hit in the blast radius
		stats->radiusHit = sqlite3_column_double(stmt, colnum++);
		// radiusDamage          NUMERIC NOT NULL, -- Damage done in the blast radius
		stats->radiusDamage = sqlite3_column_double(stmt, colnum++);
		// incenTime             NUMERIC NOT NULL, -- How long the round burns
		stats->incenTime = sqlite3_column_double(stmt, colnum++);
		// incenDamage           NUMERIC NOT NULL, -- Damage done each burn cycle
		stats->incenDamage = sqlite3_column_double(stmt, colnum++);
		// incenRadius           NUMERIC NOT NULL, -- Burn radius of the round
		stats->incenRadius = sqlite3_column_double(stmt, colnum++);
		// directLife            NUMERIC NOT NULL, -- How long a direct fire weapon is visible. Measured in 1/100 sec.
		stats->directLife = sqlite3_column_double(stmt, colnum++);
		// radiusLife            NUMERIC NOT NULL, -- How long a blast radius is visible
		stats->radiusLife = sqlite3_column_double(stmt, colnum++);

		// flightSpeed           NUMERIC NOT NULL, -- speed ammo travels at
		stats->flightSpeed = sqlite3_column_double(stmt, colnum++);
//#ifdef DEBUG
// Hack to get the current stats working... a zero flight speed value will cause an assert in projectile.c line 957
//  I'm not sure if this should be on debug only...
//    ... the last thing we want is for a zero value to get through on release (with no asserts!)
//
// Anyway if anyone has a problem with this, take it up with Tim ... we have a frank and open discussion about it.
#define DEFAULT_FLIGHTSPEED (500)
		if (stats->flightSpeed == 0)
		{
			debug(LOG_NEVER, "STATS: Zero Flightspeed for %s - using default of %d\n", getStatName(stats), DEFAULT_FLIGHTSPEED);
			stats->flightSpeed = DEFAULT_FLIGHTSPEED;
		}

		// indirectHeight        NUMERIC NOT NULL, -- how high the ammo travels for indirect fire
		stats->indirectHeight = sqlite3_column_double(stmt, colnum++);

		// fireOnMove            TEXT    NOT NULL, -- indicates whether the droid has to stop before firing
		str = (const char*)sqlite3_column_text(stmt, colnum++);
		if (!strcmp(str, "NO"))
		{
			stats->fireOnMove = FOM_NO;
		}
		else if (!strcmp(str,"PARTIAL"))
		{
			stats->fireOnMove = FOM_PARTIAL;
		}
		else if (!strcmp(str,"YES"))
		{
			stats->fireOnMove = FOM_YES;
		}
		else
		{
			debug(LOG_ERROR, "Invalid fire on move flag for weapon %s", getStatName(stats));
			abort();
			goto in_statement_err;
		}

		// weaponClass           TEXT    NOT NULL, -- the class of weapon
		str = (const char*)sqlite3_column_text(stmt, colnum++);
		if (!strcmp(str, "KINETIC"))
		{
			stats->weaponClass = WC_KINETIC;
		}
		else if (!strcmp(str,"EXPLOSIVE"))
		{
			//stats->weaponClass = WC_EXPLOSIVE;
			stats->weaponClass = WC_KINETIC; 	// explosives were removed from release version of Warzone
		}
		else if (!strcmp(str,"HEAT"))
		{
			stats->weaponClass = WC_HEAT;
		}
		else if (!strcmp(str,"MISC"))
		{
			//stats->weaponClass = WC_MISC;
			stats->weaponClass = WC_HEAT;		// removed from release version of Warzone
		}
		else
		{
			debug(LOG_ERROR, "Invalid weapon class for weapon %s", getStatName(stats));
			abort();
			goto in_statement_err;
		}

		// weaponSubClass        TEXT    NOT NULL, -- the subclass to which the weapon belongs
		stats->weaponSubClass = getWeaponSubClass((const char*)sqlite3_column_text(stmt, colnum++));
		if (stats->weaponSubClass == INVALID_SUBCLASS)
		{
			goto in_statement_err;
		}

		// movement              TEXT    NOT NULL, -- which projectile model to use for the bullet
		stats->movementModel = getMovementModel((const char*)sqlite3_column_text(stmt, colnum++));
		if (stats->movementModel == INVALID_MOVEMENT)
		{
			goto in_statement_err;
		}

		// weaponEffect          TEXT    NOT NULL, -- which type of warhead is associated with the weapon
		stats->weaponEffect = getWeaponEffect((const char*)sqlite3_column_text(stmt, colnum++));
		if (stats->weaponEffect == INVALID_WEAPON_EFFECT)
		{
			debug(LOG_ERROR, "loadWepaonStats: Invalid weapon effect for weapon %s", getStatName(stats));
			abort();
			goto in_statement_err;
		}

		// rotate                NUMERIC NOT NULL, -- amount the weapon(turret) can rotate 0 = none
		if (sqlite3_column_int(stmt, colnum) > UBYTE_MAX)
		{
			ASSERT(false, "loadWeaponStats: rotate is greater than %u for weapon %s", (unsigned int)UBYTE_MAX, getStatName(stats));
			goto in_statement_err;
		}
		stats->rotate = sqlite3_column_double(stmt, colnum++);

		// maxElevation          NUMERIC NOT NULL, -- max amount the turret can be elevated up
		if (sqlite3_column_int(stmt, colnum) > UBYTE_MAX)
		{
			ASSERT(false, "loadWeaponStats: maxElevation is greater than %u for weapon %s", (unsigned int)UBYTE_MAX, getStatName(stats));
			goto in_statement_err;
		}
		stats->maxElevation = sqlite3_column_double(stmt, colnum++);

		// minElevation          NUMERIC NOT NULL, -- min amount the turret can be elevated down
		if (sqlite3_column_int(stmt, colnum) > SBYTE_MAX
		 || sqlite3_column_int(stmt, colnum) < SBYTE_MIN)
		{
			ASSERT(false, "loadWeaponStats: minElevation is outside of limits for weapon %s", getStatName(stats));
			goto in_statement_err;
		}
		stats->minElevation = sqlite3_column_double(stmt, colnum++);

		// facePlayer            TEXT    NOT NULL, -- flag to make the (explosion) effect face the player when drawn
		stats->facePlayer = compareYes((const char*)sqlite3_column_text(stmt, colnum++), getStatName(stats)) ? true : false;
		// faceInFlight          TEXT    NOT NULL, -- flag to make the inflight effect face the player when drawn
		stats->faceInFlight = compareYes((const char*)sqlite3_column_text(stmt, colnum++), getStatName(stats)) ? true : false;

		// recoilValue           NUMERIC NOT NULL, -- used to compare with weight to see if recoils or not
		stats->recoilValue = sqlite3_column_double(stmt, colnum++);
		// minRange              NUMERIC NOT NULL, -- Min distance to target for shot
		stats->minRange = sqlite3_column_double(stmt, colnum++);

		// lightWorld            TEXT    NOT NULL, -- flag to indicate whether the effect lights up the world
		stats->lightWorld = compareYes((const char*)sqlite3_column_text(stmt, colnum++), getStatName(stats)) ? true : false;

		// effectSize            NUMERIC NOT NULL, -- size of the effect 100 = normal, 50 = half etc
		if (sqlite3_column_int(stmt, colnum) > UBYTE_MAX)
		{
			ASSERT(false, "loadWeaponStats: effectSize is greater than %u for weapon %s", (unsigned int)UBYTE_MAX, getStatName(stats));
			goto in_statement_err;
		}
		stats->rotate = sqlite3_column_double(stmt, colnum++);

		// surfaceToAir          NUMERIC NOT NULL, -- indicates how good in the air - SHOOT_ON_GROUND, SHOOT_IN_AIR or both
		if (sqlite3_column_int(stmt, colnum) > UBYTE_MAX)
		{
			ASSERT(false, "loadWeaponStats: surfaceToAir is greater than %u for weapon %s", (unsigned int)UBYTE_MAX, getStatName(stats));
			goto in_statement_err;
		}
		stats->surfaceToAir = sqlite3_column_double(stmt, colnum++);
		if (stats->surfaceToAir == 0)
		{
			stats->surfaceToAir = (UBYTE)SHOOT_ON_GROUND;
		}
		else if (stats->surfaceToAir <= 50)
		{
			stats->surfaceToAir = (UBYTE)SHOOT_IN_AIR;
		}
		else
		{
			stats->surfaceToAir = (UBYTE)(SHOOT_ON_GROUND | SHOOT_IN_AIR);
		}

		// numAttackRuns         NUMERIC NOT NULL, -- number of attack runs a VTOL droid can do with this weapon
		if (sqlite3_column_int(stmt, colnum) > UBYTE_MAX)
		{
			ASSERT(false, "loadWeaponStats: numAttackRuns is greater than %u for weapon %s", (unsigned int)UBYTE_MAX, getStatName(stats));
			goto in_statement_err;
		}
		stats->vtolAttackRuns = sqlite3_column_double(stmt, colnum++);

		// designable            INTEGER NOT NULL, -- flag to indicate whether this component can be used in the design screen
		stats->design = sqlite3_column_int(stmt, colnum++) ? true : false;

		// penetrate             NUMERIC NOT NULL  -- flag to indicate whether pentrate droid or not
		stats->penetrate = sqlite3_column_int(stmt, colnum++) ? true : false;

		// error check the ranges
		if (stats->flightSpeed > 0
		 && !proj_Direct(stats))
		{
			longRange = proj_GetLongRange(stats);
		}
		else
		{
			longRange = UINT_MAX;
		}

		if (stats->shortRange > longRange)
		{
			debug(LOG_NEVER, "%s, flight speed is too low to reach short range (max range %d)\n", getStatName(stats), longRange);
		}
		else if (stats->longRange > longRange)
		{
			debug(LOG_NEVER, "%s, flight speed is too low to reach long range (max range %d)\n", getStatName(stats), longRange);
		}

		// Set the max stat values for the design screen
		if (stats->design)
		{
			setMaxWeaponRange(stats->longRange);
			setMaxWeaponDamage(stats->damage);
			setMaxWeaponROF(weaponROF(stats, -1));
			setMaxComponentWeight(stats->weight);
		}

		//multiply time stats
		stats->firePause *= WEAPON_TIME;
		stats->incenTime *= WEAPON_TIME;
		stats->directLife *= WEAPON_TIME;
		stats->radiusLife *= WEAPON_TIME;
		stats->reloadTime *= WEAPON_TIME;

		//set the weapon sounds to default value
		stats->iAudioFireID = NO_SOUND;
		stats->iAudioImpactID = NO_SOUND;

		//save the stats
		statsSetWeapon(stats, weapon_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}

/** Load the body stats from the given SQLite database file
 *  \param filename name of the database file to load the body stats from.
 */
bool loadBodyStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `body`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of bodies we're about to fetch. Then make sure
	 * to allocate memory for that amount of bodies. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocBody(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`size`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`body`,"
	                                 "`GfxFile`,"
	                                 "`systempoints`,"
	                                 "`weapon_slots`,"
	                                 "`power_output`,"
	                                 "`armour_front_kinetic`,"
	                                 "`armour_front_heat`,"
	                                 "`armour_rear_kinetic`,"
	                                 "`armour_rear_heat`,"
	                                 "`armour_left_kinetic`,"
	                                 "`armour_left_heat`,"
	                                 "`armour_right_kinetic`,"
	                                 "`armour_right_heat`,"
	                                 "`armour_top_kinetic`,"
	                                 "`armour_top_heat`,"
	                                 "`armour_bottom_kinetic`,"
	                                 "`armour_bottom_heat`,"
	                                 "`flameIMD`,"
	                                 "`designable`"
	                          "FROM `body`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		BODY_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int body_id = sqlite3_column_int(stmt, colnum++);

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_BODY_START + body_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// size                  TEXT    NOT NULL, -- How big the body is - affects how hit
		if (!getBodySize((const char*)sqlite3_column_text(stmt, colnum++), &stats->size))
		{
			ASSERT(false, "loadBodyStats: unknown body size for %s", getStatName(stats));
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// body                  NUMERIC NOT NULL, -- Component's body points
		stats->body = sqlite3_column_double(stmt, colnum++);

		// Get the IMD for the component
		// GfxFile               TEXT,             -- The IMD to draw for this component
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the body PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pIMD = NULL;
			++colnum;
		}

		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);

		// weapon_slots          NUMERIC NOT NULL, -- The number of weapon slots on the body
		stats->weaponSlots = sqlite3_column_int(stmt, colnum++);

		// power_output          NUMERIC NOT NULL, -- this is the engine output of the body
		stats->powerOutput = sqlite3_column_double(stmt, colnum++);

		// armour_front_kinetic  NUMERIC NOT NULL, -- A measure of how much protection the armour provides. Cross referenced with the weapon types.
		stats->armourValue[HIT_SIDE_FRONT][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_front_heat     NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_FRONT][WC_HEAT] = sqlite3_column_double(stmt, colnum++);
		// armour_rear_kinetic   NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_REAR][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_rear_heat      NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_REAR][WC_HEAT] = sqlite3_column_double(stmt, colnum++);
		// armour_left_kinetic   NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_LEFT][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_left_heat      NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_LEFT][WC_HEAT] = sqlite3_column_double(stmt, colnum++);
		// armour_right_kinetic  NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_RIGHT][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_right_heat     NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_RIGHT][WC_HEAT] = sqlite3_column_double(stmt, colnum++);
		// armour_top_kinetic    NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_TOP][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_top_heat       NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_TOP][WC_HEAT] = sqlite3_column_double(stmt, colnum++);
		// armour_bottom_kinetic NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_BOTTOM][WC_KINETIC] = sqlite3_column_double(stmt, colnum++);
		// armour_bottom_heat    NUMERIC NOT NULL, -- 
		stats->armourValue[HIT_SIDE_BOTTOM][WC_HEAT] = sqlite3_column_double(stmt, colnum++);

		// flameIMD              TEXT,             -- pointer to which flame graphic to use - for VTOLs only at the moment
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pFlameIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pFlameIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the flame PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pFlameIMD = NULL;
			++colnum;
		}

		// designable            NUMERIC NOT NULL  -- flag to indicate whether this component can be used in the design screen
		stats->design = sqlite3_column_int(stmt, colnum++) ? true : false;

		//set the max stat values for the design screen
		if (stats->design)
		{
			//use front armour value to prevent bodyStats corrupt problems
			setMaxBodyArmour(stats->armourValue[HIT_SIDE_FRONT][WC_KINETIC]);
			setMaxBodyArmour(stats->armourValue[HIT_SIDE_FRONT][WC_HEAT]);
			setMaxBodyPower(stats->powerOutput);
			setMaxBodyPoints(stats->body);
			setMaxComponentWeight(stats->weight);
		}

		//save the stats
		statsSetBody(stats, body_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}


/** Load the brain stats from the given SQLite database file
 *  \param filename name of the database file to load the brain stats from.
 */
bool loadBrainStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `brain`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of brains we're about to fetch. Then make sure
	 * to allocate memory for that amount of brains. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocBrain(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`hitpoints`,"
	                                 "`systempoints`,"
	                                 "`weapon`,"
	                                 "`program_capacity`"
	                          "FROM `brain`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		BRAIN_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int brain_id = sqlite3_column_int(stmt, colnum++);

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_BRAIN_START + brain_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// hitpoints             NUMERIC NOT NULL, -- Component's hitpoints - SEEMS TO BE UNUSED
		stats->hitPoints = sqlite3_column_double(stmt, colnum++);
		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);

		// weapon                INTEGER,          -- A reference to `weapons`.`id`, refers to the weapon stats associated with this brain (can be NULL for none) - for Command Droids
		// Check whether a weapon is attached
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			const unsigned int weapon_id = sqlite3_column_int(stmt, colnum++);

			//get the weapon stat
			stats->psWeaponStat = statsGetWeapon(REF_WEAPON_START + weapon_id - 1);

			//if weapon not found - error
			if (stats->psWeaponStat == NULL)
			{
				debug(LOG_ERROR, "Unable to find weapon %u for brain %s", weapon_id, stats->pName);
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->psWeaponStat = NULL;
			++colnum;
		}

		// program_capacity      INTEGER NOT NULL  -- Program's capacity
		stats->progCap = sqlite3_column_int(stmt, colnum++);

		// All brains except ZNULLBRAIN available in design screen
		stats->design = strcmp(stats->pName, "ZNULLBRAIN") ? true : false;

		// save the stats
		statsSetBrain(stats, brain_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}

/** Load the propulsion stats from the given SQLite database file
 *  \param filename name of the database file to load the propulsion stats from.
 */
bool loadPropulsionStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `propulsion`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of propulsions we're about to fetch. Then make
	 * sure to allocate memory for that amount of propulsions. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocPropulsion(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`hitpoints`,"
	                                 "`systempoints`,"
	                                 "`body`,"
	                                 "`GfxFile`,"
					 "`type`,"
					 "`maxSpeed`,"
					 "`designable`"
	                          "FROM `propulsion`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		PROPULSION_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int propulsion_id = sqlite3_column_int(stmt, colnum++);

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_PROPULSION_START + propulsion_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// hitpoints             NUMERIC NOT NULL, -- Component's hitpoints - SEEMS TO BE UNUSED
		stats->hitPoints = sqlite3_column_double(stmt, colnum++);
		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);
		// body                  NUMERIC NOT NULL, -- Component's body points
		stats->body = sqlite3_column_double(stmt, colnum++);

		// Get the IMD for the component
		// GfxFile               TEXT,             -- The IMD to draw for this component
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the propulsion PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pIMD = NULL;
			++colnum;
		}

		// type                  TEXT    NOT NULL, -- Type of propulsion
		stats->propulsionType = getPropulsionType((const char*)sqlite3_column_text(stmt, colnum++));
		if (stats->propulsionType == INVALID_PROP_TYPE)
		{
			debug(LOG_ERROR, "loadPropulsionStatsFromDB: Invalid Propulsion type for %s", getStatName(stats));
			abort();
			goto in_statement_err;
		}

		// maxSpeed              NUMERIC NOT NULL, -- Max speed for droids with this propulsion
		stats->maxSpeed = sqlite3_column_double(stmt, colnum++);
		// designable            NUMERIC NOT NULL  -- flag to indicate whether this component can be used in the design screen
		stats->design = sqlite3_column_int(stmt, colnum++) ? true : false;

		// set the max stats values for the design screen
		if (stats->design)
		{
			setMaxPropulsionSpeed(stats->maxSpeed);
			//setMaxComponentWeight(stats->weight);
		}

		//save the stats
		statsSetPropulsion(stats, propulsion_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}

/** Load the Sensor stats from the given SQLite database file
 *  \param filename name of the database file to load the propulsion stats
 *         from.
 */
bool loadSensorStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `sensor`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of propulsions we're about to fetch. Then make
	 * sure to allocate memory for that amount of propulsions. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocSensor(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`hitpoints`,"
	                                 "`systempoints`,"
	                                 "`body`,"
	                                 "`GfxFile`,"
	                                 "`mountGfx`,"
	                                 "`range`,"
	                                 "`location`,"
	                                 "`type`,"
	                                 "`time`,"
	                                 "`power`,"
	                                 "`designable`"
	                          "FROM `sensor`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		SENSOR_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int sensor_id = sqlite3_column_int(stmt, colnum++);
		const char* str;

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_SENSOR_START + sensor_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// hitpoints             NUMERIC NOT NULL, -- Component's hitpoints - SEEMS TO BE UNUSED
		stats->hitPoints = sqlite3_column_double(stmt, colnum++);
		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);
		// body                  NUMERIC NOT NULL, -- Component's body points
		stats->body = sqlite3_column_double(stmt, colnum++);

		// Get the IMD for the component
		// GfxFile               TEXT,             -- The IMD to draw for this component
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the propulsion PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pIMD = NULL;
			++colnum;
		}

		// Get the rest of the IMDs
		// mountGfx              TEXT,             -- The turret mount to use
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pMountGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pMountGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the mount PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pMountGraphic = NULL;
			++colnum;
		}

		// range                 NUMERIC NOT NULL, -- Sensor range
		stats->range = sqlite3_column_double(stmt, colnum++);

		// location              TEXT    NOT NULL, -- specifies whether the Sensor is default or for the Turret
		str = (const char*)sqlite3_column_text(stmt, colnum++);
		if (!strcmp(str, "DEFAULT"))
		{
			stats->location = LOC_DEFAULT;
		}
		else if(!strcmp(str, "TURRET"))
		{
			stats->location = LOC_TURRET;
		}
		else
		{
			ASSERT(!"invalid sensor location", "Invalid sensor location");
		}

		// type                  TEXT    NOT NULL, -- used for combat
		str = (const char*)sqlite3_column_text(stmt, colnum++);
		if (!strcmp(str,"STANDARD"))
		{
			stats->type = STANDARD_SENSOR;
		}
		else if (!strcmp(str, "INDIRECT CB"))
		{
			stats->type = INDIRECT_CB_SENSOR;
		}
		else if (!strcmp(str, "VTOL CB"))
		{
			stats->type = VTOL_CB_SENSOR;
		}
		else if (!strcmp(str, "VTOL INTERCEPT"))
		{
			stats->type = VTOL_INTERCEPT_SENSOR;
		}
		else if (!strcmp(str, "SUPER"))
		{
			stats->type = SUPER_SENSOR;
		}
		else
		{
			ASSERT(!"invalid sensor type", "Invalid sensor type");
		}

		// time                  NUMERIC NOT NULL, -- time delay before associated weapon droids 'know' where the attack is from
		stats->time = sqlite3_column_double(stmt, colnum++);

		// multiply time stats
		stats->time *= WEAPON_TIME;

		// power                 NUMERIC NOT NULL, -- Sensor power (put against ecm power)
		stats->power = sqlite3_column_double(stmt, colnum++);

		// designable            NUMERIC NOT NULL  -- flag to indicate whether this component can be used in the design screen
		stats->design = sqlite3_column_int(stmt, colnum++) ? true : false;

		// set the max stats values for the design screen
		if (stats->design)
		{
			setMaxSensorRange(stats->range);
			setMaxSensorPower(stats->power);
			setMaxComponentWeight(stats->weight);
		}

		//save the stats
		statsSetSensor(stats, sensor_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}

/** Load the ECM (Electronic Counter Measures) stats from the given SQLite
 *  database file
 *  \param filename name of the database file to load the propulsion stats
 *         from.
 */
bool loadECMStatsFromDB(sqlite3* db)
{
	bool retval = false;
	sqlite3_stmt* stmt;
	int rc;

	// Prepare this SQL statement for execution
	if (!prepareStatement(db, "SELECT MAX(id) FROM `ecm`;", &stmt))
		return false;

	/* Execute and process the results of the above SQL statement to
	 * determine the amount of propulsions we're about to fetch. Then make
	 * sure to allocate memory for that amount of propulsions. */
	rc = sqlite3_step(stmt);
	if (rc != SQLITE_ROW
	 || sqlite3_data_count(stmt) != 1
	 || !statsAllocECM(sqlite3_column_int(stmt, 0)))
	{
		goto in_statement_err;
	}

	sqlite3_finalize(stmt);
	if (!prepareStatement(db, "SELECT `id`,"
	                                 "`name`,"
	                                 "`techlevel`,"
	                                 "`buildPower`,"
	                                 "`buildPoints`,"
	                                 "`weight`,"
	                                 "`hitpoints`,"
	                                 "`systempoints`,"
	                                 "`body`,"
	                                 "`GfxFile`,"
	                                 "`mountGfx`,"
	                                 "`location`,"
	                                 "`power`,"
	                                 "`range`,"
	                                 "`designable`"
	                          "FROM `ecm`;", &stmt))
		return false;

	while ((rc = sqlite3_step(stmt)) == SQLITE_ROW)
	{
		ECM_STATS sStats, * const stats = &sStats;
		unsigned int colnum = 0;
		const unsigned int ecm_id = sqlite3_column_int(stmt, colnum++);
		const char* str;

		memset(stats, 0, sizeof(*stats));

		stats->ref = REF_ECM_START + ecm_id - 1;

		// name                  TEXT    NOT NULL, -- Text id name (short language independant name)
		if (!allocateStatName((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// techlevel             TEXT    NOT NULL, -- Technology level of this component
		if (!setTechLevel((BASE_STATS *)stats, (const char*)sqlite3_column_text(stmt, colnum++)))
		{
			goto in_statement_err;
		}

		// buildPower            NUMERIC NOT NULL, -- Power required to build this component
		stats->buildPower = sqlite3_column_double(stmt, colnum++);
		// buildPoints           NUMERIC NOT NULL, -- Time required to build this component
		stats->buildPoints = sqlite3_column_double(stmt, colnum++);
		// weight                NUMERIC NOT NULL, -- Component's weight (mass?)
		stats->weight = sqlite3_column_double(stmt, colnum++);
		// hitpoints             NUMERIC NOT NULL, -- Component's hitpoints - SEEMS TO BE UNUSED
		stats->hitPoints = sqlite3_column_double(stmt, colnum++);
		// systempoints          NUMERIC NOT NULL, -- Space the component takes in the droid - SEEMS TO BE UNUSED
		stats->systemPoints = sqlite3_column_double(stmt, colnum++);
		// body                  NUMERIC NOT NULL, -- Component's body points
		stats->body = sqlite3_column_double(stmt, colnum++);

		// Get the IMD for the component
		// GfxFile               TEXT,             -- The IMD to draw for this component
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pIMD = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pIMD == NULL)
			{
				debug(LOG_ERROR, "Cannot find the propulsion PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pIMD = NULL;
			++colnum;
		}

		// Get the rest of the IMDs
		// mountGfx              TEXT,             -- The turret mount to use
		if (sqlite3_column_type(stmt, colnum) != SQLITE_NULL)
		{
			stats->pMountGraphic = (iIMDShape *) resGetData("IMD", (const char*)sqlite3_column_text(stmt, colnum++));
			if (stats->pMountGraphic == NULL)
			{
				debug(LOG_ERROR, "Cannot find the mount PIE for record %s", getStatName(stats));
				abort();
				goto in_statement_err;
			}
		}
		else
		{
			stats->pMountGraphic = NULL;
			++colnum;
		}

		// location              TEXT    NOT NULL, -- specifies whether the ECM is default or for the Turret
		str = (const char*)sqlite3_column_text(stmt, colnum++);
		if (!strcmp(str, "DEFAULT"))
		{
			stats->location = LOC_DEFAULT;
		}
		else if(!strcmp(str, "TURRET"))
		{
			stats->location = LOC_TURRET;
		}
		else
		{
			ASSERT(!"invalid ECM location", "Invalid ECM location");
		}

		// power                 NUMERIC NOT NULL, -- ECM power (put against sensor power)
		stats->power = sqlite3_column_double(stmt, colnum++);

		// range                 NUMERIC NOT NULL, -- ECM range
		stats->range = sqlite3_column_double(stmt, colnum++);
		// set a default ECM range for now
		stats->range = TILE_UNITS * 8;

		// designable            NUMERIC NOT NULL  -- flag to indicate whether this component can be used in the design screen
		stats->design = sqlite3_column_int(stmt, colnum++) ? true : false;

		// set the max stats values for the design screen
		if (stats->design)
		{
			setMaxECMPower(stats->power);
			setMaxECMRange(stats->range);
			setMaxComponentWeight(stats->weight);
		}

		//save the stats
		statsSetECM(stats, ecm_id - 1);
	}

	retval = true;

in_statement_err:
	sqlite3_finalize(stmt);

	return retval;
}
