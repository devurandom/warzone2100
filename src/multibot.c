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
 * Multibot.c
 *
 * Alex Lee , 97/98 Pumpkin Studios, Bath
 * Multiplay stuff relevant to droids only.
 */
#include "lib/framework/frame.h"

#include "droid.h"						// for droid sending and ordering.
#include "droiddef.h"
#include "stats.h"
#include "move.h"						// for ordering droids
#include "objmem.h"
#include "power.h"						// for powercalculated
#include "order.h"
#include "geometry.h"					// for formations.
#include "map.h"
#include "group.h"
#include "formation.h"
#include "lib/netplay/netplay.h"					// the netplay library.
#include "multiplay.h"					// warzone net stuff.
#include "multijoin.h"
#include "cmddroid.h"					// command droids
#include "action.h"
#include "console.h"
#include "mapgrid.h"
#include "multirecv.h"

#define ANYPLAYER	99
#define UNKNOWN		99

// ////////////////////////////////////////////////////////////////////////////
// Local Prototypes

static void ProcessDroidOrder(DROID *psDroid, DROID_ORDER order, UDWORD x, UDWORD y, OBJECT_TYPE desttype, UDWORD destid);

// ////////////////////////////////////////////////////////////////////////////
// Command Droids.

// sod em.


// ////////////////////////////////////////////////////////////////////////////
// vtol bits.
// happy vtol = vtol ready to go back to attack.
BOOL sendHappyVtol(const DROID* psDroid)
{
	if (!bMultiPlayer)
		return true;

	if (!myResponsibility(psDroid->player))
	{
		return false;
	}

	NETbeginEncode(NET_VTOL, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droid = psDroid->id;

		NETuint8_t(&player);
		NETuint32_t(&droid);
	}
	return NETend();
}

BOOL recvHappyVtol()
{
	DROID* pD;
	unsigned int i;

	NETbeginDecode(NET_VTOL);
	{
		uint8_t player;
		uint32_t droid;

		NETuint8_t(&player);
		NETuint32_t(&droid);

		if (!IdToDroid(droid, player, &pD))
		{
			NETend();
			return false;
		}
	}
	NETend();

	// Rearming also repairs VTOLs
	pD->body = pD->originalBody;

	for (i = 0; i < pD->numWeaps; i++)
	{
		pD->sMove.iAttackRuns[i] = 0; // finish it for next time round.
		pD->asWeaps[i].ammo = asWeaponStats[pD->asWeaps[i].nStat].numRounds;
		pD->asWeaps[i].lastFired = 0;
	}

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// Secondary Orders.

// Send
BOOL sendDroidSecondary(const DROID* psDroid, SECONDARY_ORDER sec, SECONDARY_STATE state)
{
	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_SECONDARY, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droid = psDroid->id;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETenum(&sec);
		NETenum(&state);
	}
	return NETend();
}

// recv
BOOL recvDroidSecondary()
{
	DROID*          psDroid;
	SECONDARY_ORDER sec = DSO_ATTACK_RANGE;
	SECONDARY_STATE state = DSS_NONE;

	NETbeginDecode(NET_SECONDARY);
	{
		uint8_t player;
		uint32_t droid;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETenum(&sec);
		NETenum(&state);

		// If we can not find the droid should we not ask for it?
		if (!IdToDroid(droid, player, &psDroid))
		{
			NETend();
			return false;
		}
	}
	NETend();

	// Set the droids secondary order
	turnOffMultiMsg(true);
	secondarySetState(psDroid, sec, state);
	turnOffMultiMsg(false);

	return true;
}

BOOL sendDroidSecondaryAll(const DROID* psDroid)
{
	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_SECONDARY_ALL, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droid = psDroid->id;
		uint32_t secOrder = psDroid->secondaryOrder;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETuint32_t(&secOrder);
	}
	return NETend();
}

BOOL recvDroidSecondaryAll()
{
	DROID* psDroid;

	NETbeginDecode(NET_SECONDARY_ALL);
	{
		uint8_t player;
		uint32_t droid, secOrder;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETuint32_t(&secOrder);

		if (!IdToDroid(droid, player, &psDroid))
		{
			NETend();
			return false;
		}

		if (psDroid != NULL)
		{
			psDroid->secondaryOrder = secOrder;
		}
	}
	NETend();

	return true;
}
// broadcast droid & transporter loading information
BOOL sendDroidEmbark(const DROID* psDroid,const DROID* psTransporter )
{
	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_DROIDEMBARK, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droidID = psDroid->id;
		uint32_t transporterID = psTransporter->id;

		NETuint8_t(&player);
		NETuint32_t(&droidID);
		NETuint32_t(&transporterID);
	}
	return NETend();
}
// receive droid & transporter loading information
BOOL recvDroidEmbark()
{
	DROID *psDroid;
	DROID *psTransporterDroid;
	BOOL bDroidRemoved;

	NETbeginDecode(NET_DROIDEMBARK);
	{
		uint8_t player;
		uint32_t droidID;
		uint32_t transporterID;

		NETuint8_t(&player);
		NETuint32_t(&droidID);
		NETuint32_t(&transporterID);

		// we have to find the droid on our (local) list first.
		if (!IdToDroid(droidID, player, &psDroid))
		{
			NETend();
			// Possible it already died? (sync error?)
			debug(LOG_WARNING,"player's %d droid %d wasn't found?",player,droidID);
			return false;
		}
		if (!IdToDroid(transporterID, player, &psTransporterDroid))
		{
			NETend();
			// Possible it already died? (sync error?)
			debug(LOG_WARNING,"player's %d transport droid %d wasn't found?",player,transporterID);
			return false;
		}

		if (psDroid == NULL)
		{
			// how can this happen?
			return true;
		}

		// Take it out of the world without destroying it (just removes it from the droid list)
		bDroidRemoved = droidRemove(psDroid, apsDroidLists);

		// Init the order for when disembark
		psDroid->order = DORDER_NONE;
		setDroidTarget(psDroid, NULL);
		psDroid->psTarStats = NULL;

		if (bDroidRemoved)
		{
			// and now we need to add it to their transporter group!
			grpJoin(psTransporterDroid->psGroup, psDroid);
		}
		else
		{
			// possible sync error?
			debug(LOG_WARNING,"Eh? Where did unit %d go?  Couldn't load droid onto transporter.",droidID);
		}
	}
	NETend();
	return true;
}
// sending information that droid is being unloaded from a transporter
BOOL sendDroidDisEmbark(const DROID* psDroid, const DROID* psTransporter)
{
	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_DROIDDISEMBARK, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droidID = psDroid->id;
		uint32_t transporterID = psTransporter->id;
		Vector3uw pos = psDroid->pos;

		NETuint8_t(&player);
		NETuint32_t(&droidID);
		NETuint32_t(&transporterID);
		NETVector3uw(&pos);
	}
	return NETend();
}
// getting informaton about droid is being unloaded from a transporter
BOOL recvDroidDisEmbark()
{
	DROID *psFoundDroid = NULL, *psTransporterDroid = NULL;
	DROID *psCheckDroid = NULL;

	NETbeginDecode(NET_DROIDDISEMBARK);
	{
		uint8_t player;
		uint32_t droidID;
		uint32_t transporterID;
		Vector3uw pos;

		NETuint8_t(&player);
		NETuint32_t(&droidID);
		NETuint32_t(&transporterID);
		NETVector3uw(&pos);

		NETend();

		// find the transporter first
		if (!IdToDroid(transporterID, player, &psTransporterDroid))
		{
			// Possible it already died? (sync error?)
			debug(LOG_WARNING,"player's %d transport droid %d wasn't found?",player,transporterID);
			return false;
		}
		// we need to find the droid *in* the transporter
		psCheckDroid = psTransporterDroid ->psGroup->psList;
		while (psCheckDroid)
		{
			// is this the one we want?
			if( psCheckDroid->id == droidID)
			{
				psFoundDroid = psCheckDroid;
				break;
			}
			// not found, so check next one in *group*
			psCheckDroid = psCheckDroid->psGrpNext;
		}
		// don't continue if we couldn't find it.
		if (!psFoundDroid)
		{
			// I don't think this could ever be possible...but
			debug(LOG_ERROR,"Couldn't find droid %d to disembark from player %d's transporter?",droidID,player);
			return false;
		}

		// remove it from the transporter
		grpLeave(psFoundDroid->psGroup, psFoundDroid);

		// and add it back to the bloody droid list
		addDroid(psFoundDroid, apsDroidLists);

		// Add it back into the world at the x/y
		psFoundDroid->pos = pos;

		if (!droidOnMap(psFoundDroid))
		{
			debug(LOG_ERROR,"droid %d disembarked was NOT on map?",psFoundDroid->id);
			return false;
		}

		updateDroidOrientation(psFoundDroid);

		// Initialise the movement data
		initDroidMovement(psFoundDroid);
		// must add it to the grid for targeting to work
		gridAddObject((BASE_OBJECT *)psFoundDroid);
	} // NetBeginDecode
	return true;
}


// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Droids

// posibly Send an updated droid movement order.
BOOL SendDroidMove(const DROID* psDroid, uint32_t x, uint32_t y, BOOL formation)
{
	if (!bMultiPlayer)
		return true;

	ASSERT(x > 0 && y > 0, "SendDroidMove: Invalid move order");

	// Don't allow a move to happen at all if it is not our responsibility
	if (!myResponsibility(psDroid->player))
	{
		return false; // Do not allow move
	}

	// If the unit has no actions or orders, allow it to happen but do not send
	if (psDroid->action == DACTION_NONE || psDroid->order == DORDER_MOVE)
	{
		return true;
	}

	NETbeginEncode(NET_DROIDMOVE, NET_ALL_PLAYERS);
	{
		uint8_t player = psDroid->player;
		uint32_t droid = psDroid->id;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETuint32_t(&x);
		NETuint32_t(&y);
		NETbool(&formation);
	}
	return NETend();
}

// recv and updated droid position
BOOL recvDroidMove()
{
	DROID* psDroid;
	uint32_t x, y;
	BOOL formation;

	NETbeginDecode(NET_DROIDMOVE);
	{
		uint8_t player;
		uint32_t droid;

		NETuint8_t(&player);
		NETuint32_t(&droid);
		NETuint32_t(&x);
		NETuint32_t(&y);
		NETbool(&formation);

		NETend();

		if ((x == 0 && y == 0) || x > world_coord(mapWidth) || y > world_coord(mapHeight))
		{
			/* Probably an invalid droid position */
			debug(LOG_ERROR, "Received an invalid droid position from %d", NETgetSource());
			return false;
		}
		if (!IdToDroid(droid, player, &psDroid))
		{
			debug(LOG_ERROR, "recvDroidMove: Packet from %d refers to non-existent droid %d!",
			      NETgetSource(), (int)droid);
			return false;
		}
	}

	turnOffMultiMsg(true);
	if (formation)
	{
		moveDroidTo(psDroid, x, y); // Do the move
	}
	else
	{
		moveDroidToNoFormation(psDroid, x, y); // Move, no form...
	}
	turnOffMultiMsg(false);

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// Send a new Droid to the other players
BOOL SendDroid(const DROID_TEMPLATE* pTemplate, uint32_t x, uint32_t y, uint8_t player, uint32_t id)
{
	if (!bMultiPlayer)
		return true;

	ASSERT(x != 0 && y != 0, "SendDroid: Invalid droid coordinates");

	// Dont send other droids during campaign setup
	if (ingame.localJoiningInProgress)
	{
		return true;
	}

	// Only send the droid if we are responsible
	if (!myResponsibility(player))
	{
		// Don't build if we are not responsible
		return false;
	}

	NETbeginEncode(NET_DROID, NET_ALL_PLAYERS);
	{
		Vector3uw pos = { x, y, 0 };
		uint32_t templateID = pTemplate->multiPlayerID;

		NETuint8_t(&player);
		NETuint32_t(&id);
		NETVector3uw(&pos);
		NETuint32_t(&templateID);
		NETbool(&powerCalculated);
	}
	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// receive droid creation information from other players
BOOL recvDroid()
{
	DROID_TEMPLATE* pT;
	DROID* psDroid;
	uint8_t player;
	uint32_t id;
	Vector3uw pos;
	BOOL power;
	uint32_t templateID;

	NETbeginDecode(NET_DROID);
	{
		NETuint8_t(&player);
		NETuint32_t(&id);
		NETVector3uw(&pos);
		NETuint32_t(&templateID);
		NETbool(&power);

		pT = IdToTemplate(templateID, player);
	}
	NETend();

	if ((pos.x == 0 && pos.y == 0) || pos.x > world_coord(mapWidth) || pos.y > world_coord(mapHeight))
	{
		debug(LOG_ERROR, "recvDroid: Received bad droid position (%d, %d) from %d", (int)pos.x, (int)pos.y, NETgetSource());
		return false;
	}

	// If we can not find the template ask for the entire droid instead
	if (!pT)
	{
		debug(LOG_ERROR, "recvDroid: Packet from %d refers to non-existent template %d!",
		      NETgetSource(), (int)templateID);
		return false;
	}

	// If the power to build the droid has been calculated
	if (power
	// Use the power required to build the droid
	 && !usePower(player, pT->powerPoints))
	{
		debug(LOG_ERROR, "recvDroid: Not enough power to build droid for player = %hhu", player);
		// Build anyway..
	}

	// Create that droid on this machine.
	turnOffMultiMsg(true);
	psDroid = buildDroid(pT, pos.x, pos.y, player, false);
	turnOffMultiMsg(false);

	// If we were able to build the droid set it up
	if (psDroid)
	{
		psDroid->id = id;
		addDroid(psDroid, apsDroidLists);
	}
	else
	{
		debug(LOG_ERROR, "recvDroid: Packet from %d cannot create droid!", NETgetSource());
		DBCONPRINTF(ConsoleString, (ConsoleString, "MULTIPLAYER: Couldn't build a remote droid, relying on checking to resync"));
		return false;
	}

	return true;
}


/*!
 * Type of the target of the movement
 */
typedef enum {
	NET_ORDER_SUBTYPE_POSITION,
	NET_ORDER_SUBTYPE_OBJECT,
	NET_ORDER_SUBTYPE_SPECIAL // x and y are 0, no idea what that means
} NET_ORDER_SUBTYPE;


// ////////////////////////////////////////////////////////////////////////////
/*!
 * Droid Group/selection orders.
 * Minimises comms by sending orders for whole groups, rather than each droid
 */
BOOL SendGroupOrderSelected(uint8_t player, uint32_t x, uint32_t y, const BASE_OBJECT* psObj)
{
	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_GROUPORDER, NET_ALL_PLAYERS);
	{
		DROID_ORDER order = UNKNOWN;
		BOOL subType = (psObj) ? true : false, cmdOrder = false;
		DROID* psDroid;
		uint8_t droidCount;

		NETenum(&order);
		NETbool(&cmdOrder);
		NETbool(&subType);

		// If they are being ordered to `goto' an object
		if (subType)
		{
			uint32_t id = psObj->id;
			uint32_t type = psObj->type;

			NETuint32_t(&id);
			NETenum(&type);
		}
		// Else if the droids are being ordered to `goto' a specific position
		else
		{
			NETuint32_t(&x);
			NETuint32_t(&y);
		}

		// Work out the number of droids to send
		for (psDroid = apsDroidLists[player], droidCount = 0; psDroid; psDroid = psDroid->psNext)
		{
			if (psDroid->selected)
				++droidCount;
		}

		// If there are less than 2 droids don't bother (to allow individual orders)
		if (droidCount < 2)
		{
			return false;
		}

		// Add the number of droids to the message
		NETuint8_t(&droidCount);

		// Add the droids to the message
		for (psDroid = apsDroidLists[player]; psDroid && droidCount; psDroid = psDroid->psNext)
		{
			if (psDroid->selected)
			{
				NETuint32_t(&psDroid->id);
				--droidCount;
			}
		}
	}
	return NETend();
}

BOOL SendGroupOrderGroup(const DROID_GROUP* psGroup, DROID_ORDER order, uint32_t x, uint32_t y, const BASE_OBJECT* psObj)
{
	/* Check if the order is valid */
	if ((psObj && !validOrderForObj(order)) || (!psObj && !validOrderForLoc(order)))
	{
		ASSERT(false, "SendGroupOrderGroup: Bad order");
		return false;
	}

	if (!bMultiPlayer)
		return true;

	NETbeginEncode(NET_GROUPORDER, NET_ALL_PLAYERS);
	{
		BOOL subType = (psObj) ? true : false, cmdOrder = false;
		DROID* psDroid;
		uint8_t droidCount;

		NETenum(&order);
		NETbool(&cmdOrder);
		NETbool(&subType);

		// If they are being ordered to `goto' an object
		if (subType)
		{
			uint32_t id = psObj->id;
			uint32_t type = psObj->type;

			NETuint32_t(&id);
			NETenum(&type);
		}
		// Else if the droids are being ordered to `goto' a specific position
		else
		{
			NETuint32_t(&x);
			NETuint32_t(&y);
		}

		// Work out the number of droids to send
		for (psDroid = psGroup->psList, droidCount = 0; psDroid; psDroid = psDroid->psGrpNext)
		{
			++droidCount;
		}

		// Add the number of droids to the message
		NETuint8_t(&droidCount);

		// Add the droids to the message
		for (psDroid = psGroup->psList; psDroid; psDroid = psDroid->psGrpNext)
		{
			NETuint32_t(&psDroid->id);
		}
	}
	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// receive a group order.
BOOL recvGroupOrder()
{
	DROID_ORDER order = DORDER_NONE;
	BOOL subType, cmdOrder;

	uint32_t destId, x, y;
	OBJECT_TYPE destType = OBJ_DROID; // Dummy initialisation to workaround NETenum macro

	uint8_t droidCount, i;
	uint32_t* droidIDs;

	NETbeginDecode(NET_GROUPORDER);
	{
		NETenum(&order);
		NETbool(&cmdOrder);
		NETbool(&subType);

		// If they are being ordered to `goto' an object
		if (subType)
		{
			NETuint32_t(&destId);
			NETenum(&destType);
		}
		// Else if the droids are being ordered to `goto' a specific position
		else
		{
			NETuint32_t(&x);
			NETuint32_t(&y);
		}

		// Get the droid count
		NETuint8_t(&droidCount);

		// Allocate some space on the stack to hold the droid IDs
		droidIDs = alloca(droidCount * sizeof(uint32_t));

		// Retrieve the droids from the message
		for (i = 0; i < droidCount; ++i)
		{
			// Retrieve the id number for the current droid
			if (!NETuint32_t(&droidIDs[i]))
			{
				// If somehow we fail assume the message got truncated prematurely
				debug(LOG_NET, "recvGroupOrder: error retrieving droid ID number; while there are (supposed to be) still %u droids left",
				      (unsigned int)(droidCount - i));
				NETend();
				return false;
			}
		}
	}
	NETend();

	/* Check if the order is valid */
	if (order != UNKNOWN && ((subType && !validOrderForObj(order)) || (!subType && !validOrderForLoc(order))))
	{
		debug(LOG_ERROR, "recvGroupOrder: Invalid group order received from %d!", NETgetSource());
		return false;
	}

	// Process the given order for all droids we've retrieved
	for (i = 0; i < droidCount; ++i)
	{
		DROID* psDroid;

		// Retrieve the droid associated with the current ID
		if (!IdToDroid(droidIDs[i], ANYPLAYER, &psDroid))
		{
			debug(LOG_ERROR, "recvGroupOrder: Packet from %d refers to non-existent droid %d!",
			      NETgetSource(), (int)droidIDs[i]);
			continue; // continue working on next droid; crossing fingers...
		}

		/*
		 * If the current order not is a command order and we are not a
		 * commander yet are in the commander group remove us from it.
		 */
		if (!cmdOrder && hasCommander(psDroid))
		{
			grpLeave(psDroid->psGroup, psDroid);
		}

		// Process the droid's order
		if (subType)
		{
			/* If they are being ordered to `goto' an object then we don't
			 * have any X and Y coordinate.
			 */
			ProcessDroidOrder(psDroid, order, 0, 0, destType, destId);
		}
		else
		{
			/* Otherwise if the droids are being ordered to `goto' a
			 * specific position. Then we don't have any destination info
			 */
			ProcessDroidOrder(psDroid, order, x, y, 0, 0);
		}
	}

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// Droid update information
BOOL SendDroidInfo(const DROID* psDroid, DROID_ORDER order, uint32_t x, uint32_t y, const BASE_OBJECT* psObj)
{
	if (!bMultiPlayer)
		return true;

	if (!myResponsibility(psDroid->player))
	{
		return true;
	}

	NETbeginEncode(NET_DROIDINFO, NET_ALL_PLAYERS);
	{
		uint32_t droidId = psDroid->id;
		BOOL subType = (psObj) ? true : false;

		// Send the droid's ID
		NETuint32_t(&droidId);

		// Send the droid's order
		NETenum(&order);
		NETbool(&subType);

		if (subType)
		{
			uint32_t destId = psObj->id;
			uint32_t destType = psObj->type;

			NETuint32_t(&destId);
			NETenum(&destType);
		}
		else
		{
			NETuint32_t(&x);
			NETuint32_t(&y);
		}
	}
	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// receive droid information form other players.
BOOL recvDroidInfo()
{
	NETbeginDecode(NET_DROIDINFO);
	{
		uint32_t    droidId;
		DROID*      psDroid;
		DROID_ORDER order = DORDER_NONE;
		BOOL        subType;

		// Get the droid
		NETuint32_t(&droidId);

		if (!IdToDroid(droidId, ANYPLAYER, &psDroid))
		{
			debug(LOG_ERROR, "recvDroidInfo: Packet from %d refers to non-existent droid %d!",
			      NETgetSource(), (int)droidId);
			return false;
		}

		// Get the droid's order
		NETenum(&order);
		NETbool(&subType);

		if (subType)
		{
			uint32_t destId, destType = 0;

			NETuint32_t(&destId);
			NETenum(&destType);

			ProcessDroidOrder(psDroid, order, 0, 0, destType, destId);
		}
		else
		{
			uint32_t x, y;

			NETuint32_t(&x);
			NETuint32_t(&y);

			// If both the X _and_ Y coordinate are zero we've been given a
			// "special" order.
			if (x == 0 && y == 0)
			{
				turnOffMultiMsg(true);
				orderDroid(psDroid, order);
				turnOffMultiMsg(false);
			}
			// Otherwise it is just a normal "goto location" order
			else
			{
				ProcessDroidOrder(psDroid, order, x, y, 0, 0);
			}
		}
	}
	NETend();

	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// process droid order
static void ProcessDroidOrder(DROID *psDroid, DROID_ORDER order, uint32_t x, uint32_t y, OBJECT_TYPE desttype, uint32_t destid)
{
	// Target is a location
	if (destid == 0 && desttype == 0)
	{
		// Don't bother if it is close
		if (abs(psDroid->pos.x - x) < (TILE_UNITS/2)
		 && abs(psDroid->pos.y - y) < (TILE_UNITS/2))
		{
			return;
		}

		// If no specific order was passed work one out based on the location
		if (order == UNKNOWN)
		{
			order = chooseOrderLoc(psDroid, x, y);
		}

		turnOffMultiMsg(true);
		orderDroidLoc(psDroid, order, x, y);
		turnOffMultiMsg(false);
	}
	// Target is an object
	else
	{
		BASE_OBJECT *psObj = NULL;
		DROID		*pD;

		switch (desttype)
		{
			case OBJ_DROID:
				if (IdToDroid(destid, ANYPLAYER, &pD))
				{
					psObj = (BASE_OBJECT*)pD;
				}
				break;
			case OBJ_STRUCTURE:
				psObj = (BASE_OBJECT*)IdToStruct(destid,ANYPLAYER);
				break;
			case OBJ_FEATURE:
				psObj = (BASE_OBJECT*)IdToFeature(destid,ANYPLAYER);
				break;

			// We should not get this!
			case OBJ_PROJECTILE:
				debug(LOG_ERROR, "ProcessDroidOrder: order specified destination as a bullet. what am i to do??");
				break;
			default:
				debug(LOG_ERROR, "ProcessDroidOrder: unknown object type");
				break;
		}

		// If we did not find anything, return
		if (!psObj)													// failed to find it;
		{
			return;
		}

		// If we didn't sepcify an order, then pick one
		if (order == UNKNOWN)
		{
			order = chooseOrderObj(psDroid, psObj);
		}

		turnOffMultiMsg(true);
		orderDroidObj(psDroid, order, psObj);
		turnOffMultiMsg(false);
	}
}


// ////////////////////////////////////////////////////////////////////////////
// Inform other players that a droid has been destroyed
BOOL SendDestroyDroid(const DROID* psDroid)
{
	if (!bMultiPlayer)
	{
		return true;
	}

	NETbeginEncode(NET_DROIDDEST, NET_ALL_PLAYERS);
	{
		uint32_t id = psDroid->id;

		// Send the droid's ID
		debug(LOG_DEATH, "Requested all players to destroy droid %u", (unsigned int)id);
		NETuint32_t(&id);
	}
	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// Accept a droid which was destroyed on another machine
BOOL recvDestroyDroid()
{
	DROID* psDroid;

	NETbeginDecode(NET_DROIDDEST);
	{
		uint32_t id;

		// Retrieve the droid
		NETuint32_t(&id);
		if (!IdToDroid(id, ANYPLAYER, &psDroid))
		{
			return false;
		}
	}
	NETend();

	// If the droid has not died on our machine yet, destroy it
	if(!psDroid->died)
	{
		turnOffMultiMsg(true);
		debug(LOG_DEATH, "Killing droid %d on request from player %d", psDroid->id, NETgetSource());
		destroyDroid(psDroid);
		turnOffMultiMsg(false);
	}

	return true;
}
