/*
 * netusers.c
 * functions regarding specific players
 */

#include "lib/framework/frame.h"
#include "netplay.h"

// call to disable/enable ALL comms. Absolute arse of a thing, be very careful!
BOOL NETuseNetwork(BOOL val)
{
	NetPlay.bComms = val;
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// Functions for spectators.

BOOL NETspectate(GUID guidSessionInstance) // FIXME Remove if unused
{
	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////
BOOL NETisSpectator(DPID dpid) // FIXME Remove if unused
{
	UBYTE i;

	// was "dpid = NetPlay.dpidPlayer == dpid", but that didn't make sense.
	if((dpid = NetPlay.dpidPlayer) == dpid)	// checking ourselves
	{
		return NetPlay.bSpectator;
	}

	// could enumerate the spectators and check if he's there!
	// bugger it, just check that dpid isn't a player instead!
	for(i=0; i<MaxNumberOfPlayers; i++)
	{
		if(NetPlay.players[i].dpid == dpid)
		{
			return FALSE;
		}
	}

	return TRUE;
}

