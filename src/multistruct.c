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
 * Multistruct.c
 *
 * Alex Lee 98, Pumpkin Studios.
 *
 * files to cope with multiplayer structure related stuff..
 */

#include <string.h>

#include "lib/framework/frame.h"
#include "droid.h"
#include "droiddef.h"
#include "basedef.h"
#include "power.h"
#include "geometry.h"								// for gettilestructure
#include "anim_id.h"
#include "stats.h"
#include "map.h"
#include "console.h"
#include "action.h"
#include "order.h"
#include "projectile.h"
#include "lib/netplay/netplay.h"								// the netplay library.
#include "multiplay.h"
#include "multigifts.h"
#include "multirecv.h"
#include "lib/sound/audio_id.h"
#include "lib/sound/audio.h"

// ////////////////////////////////////////////////////////////////////////////
// structures

// ////////////////////////////////////////////////////////////////////////////
// INFORM others that a building has been started, and base plate should be put down.
BOOL sendBuildStarted(STRUCTURE *psStruct, DROID *psDroid)
{
	NETbeginEncode(NET_BUILD, NET_ALL_PLAYERS);
	
		// Who is building it
		NETuint8_t(&psDroid->player);
		
		// What they are building
		NETuint32_t(&psDroid->psTarStats->ref);
		
		// Where it is being built
		NETuint16_t(&psDroid->orderX);
		NETuint16_t(&psDroid->orderY);
		
		// The droid building it
		NETuint32_t(&psDroid->id);
		
		// The ID assigned to the structure being built
		NETuint32_t(&psStruct->id);
		
		// The droids order
		NETint32_t(&psDroid->order);
	
		if (psDroid->psTarget
		 && psDroid->psTarget->type == OBJ_STRUCTURE)
		{
			// The ID of the droids target (== psStruct->id ?)
			NETuint32_t(&psDroid->psTarget->id);
		}
		else
		{
			NETnull();
		}
	
		// Z coord
		NETuint16_t(&psStruct->pos.z);

	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// put down a base plate and start droid building it!
BOOL recvBuildStarted()
{
	STRUCTURE_STATS *psStats;
	DROID			*psDroid;
	UDWORD			actionX,actionY;
	int				typeIndex;
	uint8_t			player;
	uint16_t		x, y, z;
	int32_t			order;
	uint32_t		structRef, structId, targetId,droidID;

	NETbeginDecode(NET_BUILD);
		NETuint8_t(&player);
		NETuint32_t(&structRef);
		NETuint16_t(&x);
		NETuint16_t(&y);
		NETuint32_t(&droidID);
		NETuint32_t(&structId);
		NETint32_t(&order);
		NETuint32_t(&targetId);
		NETuint16_t(&z);
	NETend();
	// Find structure target
	for (typeIndex = 0;
	     typeIndex < numStructureStats && asStructureStats[typeIndex].ref != structRef;
	     typeIndex++);

	psStats = &asStructureStats[typeIndex];

	if (IdToDroid(droidID, player, &psDroid))
	{
		// Tell the droid to go to where it needs to in order to build the struct
		if (getDroidDestination((BASE_STATS *) psStats, x, y, &actionX, &actionY))
		{
			psDroid->order = order;
			
			if (psDroid->order == DORDER_LINEBUILD)
			{
				psDroid->order = DORDER_BUILD;
			}
			
			psDroid->orderX = x;
			psDroid->orderY = y;
			psDroid->psTarStats = (BASE_STATS *) psStats;
			
			if (targetId)
			{
				setDroidTarget(psDroid, IdToPointer(targetId, ANYPLAYER));
			}
			else
			{
				setDroidTarget(psDroid, NULL);
			}

			if (IsStatExpansionModule(psStats))
			{
				setUpBuildModule(psDroid);
			}
			else
			{
				droidStartBuild(psDroid);
				psDroid->action = DACTION_BUILD;
			}
		}

		// Sync IDs
		if (psDroid->psTarget)
		{
			((STRUCTURE *) psDroid->psTarget)->id = structId;
		}
	}
	
	return true;
}

// ////////////////////////////////////////////////////////////////////////////
// INFORM others that a building has been completed.
BOOL SendBuildFinished(STRUCTURE *psStruct)
{	
	NETbeginEncode(NET_BUILDFINISHED, NET_ALL_PLAYERS);
		// ID of building
		NETuint32_t(&psStruct->id);
		
		// Along with enough info to build it (if needed)
		NETuint32_t(&psStruct->pStructureType->ref);
		NETuint16_t(&psStruct->pos.x);
		NETuint16_t(&psStruct->pos.y);
		NETuint16_t(&psStruct->pos.z);
		NETuint8_t(&psStruct->player);
	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
BOOL recvBuildFinished()
{
	UDWORD structId;
	STRUCTURE *psStruct;
	UWORD	x,y,z;
	UDWORD	type,typeindex;
	UBYTE	player;


	NETbeginDecode(NET_BUILDFINISHED);
		NETuint32_t(&structId);	// get the struct id.
		NETuint32_t(&type); 	// Kind of building.
		NETuint16_t(&x);    	// x pos
		NETuint16_t(&y);    	// y pos
		NETuint16_t(&z);    	// z pos
		NETuint8_t(&player);
	NETend();

	psStruct = IdToStruct(structId,ANYPLAYER);

	if (psStruct)
	{												// make it complete.
		psStruct->currentBuildPts = psStruct->pStructureType->buildPoints+1;

		if (psStruct->status != SS_BUILT)
		{
			psStruct->status = SS_BUILT;
			buildingComplete(psStruct);
		}
		NETlogEntry("building finished ok." ,0,0);
		return true;
	}

	// The building wasn't started, so we'll have to just plonk it down in the map.

	// Find the structures stats
	for (typeindex=0;						// Find structure target
		(typeindex<numStructureStats ) && (asStructureStats[typeindex].ref != type);
		typeindex++);

	// Check for similar buildings, to avoid overlaps
	if (TILE_HAS_STRUCTURE(mapTile(map_coord(x), map_coord(y))))
	{
		// Get the current structure
		psStruct = getTileStructure(map_coord(x), map_coord(y));
		if (asStructureStats[typeindex].type == psStruct->pStructureType->type)
		{
			// Correct type, correct location, just rename the id's to sync it.. (urgh)
			psStruct->id = structId;
			psStruct->status = SS_BUILT;
			buildingComplete(psStruct);
			NETlogEntry("structure id modified", 0, player);

			return true;
		}
	}
	// Build the structure
	psStruct = buildStructure(&(asStructureStats[typeindex]),	// Build the structure.
	                          x, y, player,true);
	
	if (psStruct)
	{
		psStruct->id		= structId;
		psStruct->status	= SS_BUILT;
		buildingComplete(psStruct);

		NETlogEntry("had to plonk down a building" ,0,player);
	}
	else
	{
		NETlogEntry("had to plonk down a building, BUT FAILED OH S**T." ,0,player);
	}
	
	return false;
}


// ////////////////////////////////////////////////////////////////////////////
// demolish message.
BOOL SendDemolishFinished(STRUCTURE *psStruct, DROID *psDroid)
{
	NETbeginEncode(NET_DEMOLISH, NET_ALL_PLAYERS);             
                                                         
		// Send what is being demolish and who is doing it
		NETuint32_t(&psStruct->id);
		NETuint32_t(&psDroid->id);

	return NETend();
}

BOOL recvDemolishFinished()
{
	STRUCTURE	*psStruct;
	DROID		*psDroid;
	uint32_t	structID, droidID;

	NETbeginDecode(NET_DEMOLISH);
		NETuint32_t(&structID);
		NETuint32_t(&droidID);
	NETend();

	psStruct = IdToStruct(structID, ANYPLAYER);
	if (!IdToDroid(droidID, ANYPLAYER, &psDroid))
	{
		debug(LOG_ERROR, "recvDemolishFinished: Packet with bad droid ID received. Discarding!");
		return false;
	}

	if (psStruct)
	{
		// Demolish it
		removeStruct(psStruct, true);
		if (psDroid && psDroid->psTarStats)
		{
			// Update droid if reqd
			psDroid->psTarStats = NULL;
		}
	}
	
	return true;
}


// ////////////////////////////////////////////////////////////////////////////
// Inform others that a structure has been destroyed
BOOL SendDestroyStructure(STRUCTURE *s)
{
	technologyGiveAway(s);
	NETbeginEncode(NET_STRUCTDEST, NET_ALL_PLAYERS);

	// Struct to destroy
	NETuint32_t(&s->id);

	return NETend();
}

// ////////////////////////////////////////////////////////////////////////////
// acknowledge the destruction of a structure, from another player.
BOOL recvDestroyStructure()
{
	uint32_t structID;
	STRUCTURE *psStruct;

	NETbeginDecode(NET_STRUCTDEST);
		NETuint32_t(&structID);
	NETend();
								
	// Struct to destory
	psStruct = IdToStruct(structID,ANYPLAYER);

	if (psStruct)
	{
		turnOffMultiMsg(true);
		// Remove the struct from remote players machine
		destroyStruct(psStruct);
		turnOffMultiMsg(false);
		// NOTE: I do not think this should be here!
		technologyGiveAway(psStruct);
	}
	
	return true;
}

// ////////////////////////////////////////////////////////////////////////////
//lassat is firing

BOOL sendLasSat(UBYTE player, STRUCTURE *psStruct, BASE_OBJECT *psObj)
{
	NETbeginEncode(NET_LASSAT, NET_ALL_PLAYERS);

		NETuint8_t(&player);
		NETuint32_t(&psStruct->id);
		NETuint32_t(&psObj->id);	// Target
		NETuint8_t(&psObj->player);	// Target player

	return NETend();
}

// recv lassat info on the receiving end.
BOOL recvLasSat()
{
	BASE_OBJECT	*psObj;
	UBYTE		player,targetplayer;
	STRUCTURE	*psStruct;
	uint32_t	id,targetid;
	
	NETbeginDecode(NET_LASSAT);
		NETuint8_t(&player);
		NETuint32_t(&id);
		NETuint32_t(&targetid);
		NETuint8_t(&targetplayer);
	NETend();

		psStruct = IdToStruct (id, player);
		psObj	 = IdToPointer(targetid, targetplayer);
	
		if( psStruct && psObj)
		{
			// Give enemy no quarter, unleash the lasat
			proj_SendProjectile(&psStruct->asWeaps[0], NULL, player, psObj->pos.x,
	            psObj->pos.y, psObj->pos.z, psObj, true, false, 0);
	      	// Play 5 second countdown message
			audio_QueueTrackPos( ID_SOUND_LAS_SAT_COUNTDOWN, psObj->pos.x, psObj->pos.y,
	            psObj->pos.z);
		}

	return true;
}
