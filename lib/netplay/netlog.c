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
// ////////////////////////////////////////////////////////////////////////
// Includes
#include "lib/framework/frame.h"

#include <time.h>
#include <physfs.h>

#include "netlog.h"
#include "netplay.h"

// ////////////////////////////////////////////////////////////////////////
// Logging for degug only
// ////////////////////////////////////////////////////////////////////////

static const char *packetname[NUM_GAME_PACKETS] =
{
	"NET_DROID",
	"NET_DROIDINFO",
	"NET_DROIDDEST",
	"NET_DROIDMOVE",
	"NET_GROUPORDER",
	"NET_TEMPLATE",
	"NET_TEMPLATEDEST",
	"NET_FEATUREDEST",
	"NET_PING",
	"NET_CHECK_DROID",
	"NET_CHECK_STRUCT",
	"NET_CHECK_POWER",
	"NET_PLAYER_STATS",
	"NET_BUILD",
	"NET_STRUCTDEST",
	"NET_BUILDFINISHED",
	"NET_RESEARCH",
	"NET_TEXTMSG",
	"NET_REQUESTDROID",
	"NET_PLAYERCOMPLETE",
	"NET_REQUESTPLAYER",
	"NET_STRUCT",
	"NET_WHOLEDROID",
	"NET_FEATURES",
	"NET_PLAYERRESPONDING",
	"NET_OPTIONS",
	"NET_KICK",
	"NET_SECONDARY",
	"NET_FIREUP",
	"NET_ALLIANCE",
	"NET_GIFT",
	"NET_DEMOLISH",
	"NET_COLOURREQUEST",
	"NET_ARTIFACTS",
	"NET_DMATCHWIN",
	"NET_SCORESUBMIT",
	"NET_DESTROYXTRA",
	"NET_VTOL",
	"NET_UNUSED_39",
	"NET_WHITEBOARD",
	"NET_SECONDARY_ALL",
	"NET_DROIDEMBARK",
	"NET_DROIDDISEMBARK",
	"NET_RESEARCHSTATUS",
	"NET_LASSAT",
	"NET_REQUESTMAP",
	"NET_AITEXTMSG",
	"NET_TEAMS_ON",
	"NET_BEACONMSG",
	"NET_SET_TEAMS",
	"NET_TEAMREQUEST",
	"NET_JOIN",
	"NET_ACCEPTED",
	"NET_PLAYER_INFO",
	"NET_PLAYER_JOINED",
	"NET_PLAYER_LEAVING",
	"NET_PLAYER_DROPPED",
	"NET_GAME_FLAGS",
	"NET_READY_REQUEST",
	"UNUSED",
	"NET_VERSION_CHECK",
	"NET_REQUEST_VERSION",
	"NET_REQUEST_PASSWORD",
	"NET_PASSWORD_CHECK",
	"NET_POSITIONREQUEST",
};

static PHYSFS_file	*pFileHandle = NULL;
static uint32_t		packetcount[2][NUM_GAME_PACKETS];
static uint32_t		packetsize[2][NUM_GAME_PACKETS];

BOOL NETstartLogging(void)
{
	time_t aclock;
	struct tm *newtime;
	char buf[256];
	static const char filename[] = "netplay.log";
	int i;

	for (i = 0; i < NUM_GAME_PACKETS; i++)
	{
		packetcount[0][i] = 0;
		packetsize[0][i] = 0;
		packetcount[1][i] = 0;
		packetsize[1][i] = 0;
	}

	time( &aclock );                 /* Get time in seconds */
	newtime = localtime( &aclock );  /* Convert time to struct */

	pFileHandle = PHYSFS_openWrite( filename ); // open the file
	if (!pFileHandle)
	{
		debug(LOG_ERROR, "Could not create net log %s: %s", filename,
		      PHYSFS_getLastError());
		return false;
	}
	snprintf(buf, sizeof(buf), "NETPLAY log: %s\n", asctime(newtime));
	PHYSFS_write( pFileHandle, buf, strlen( buf ), 1 );
	return true;
}

BOOL NETstopLogging(void)
{
	char buf[256];
	int i;

	if (!pFileHandle)
	{
		return false;
	}

	/* Output stats */
	for (i = 0; i < NUM_GAME_PACKETS; i++)
	{
		snprintf(buf, sizeof(buf), "%s: received %u times, %u bytes; sent %u times, %u bytes\n", packetname[i],
			packetcount[0][i], packetsize[0][i], packetcount[1][i], packetsize[1][i]);
		PHYSFS_write(pFileHandle, buf, strlen(buf), 1);
	}

	if (!PHYSFS_close(pFileHandle))
	{
		debug(LOG_ERROR, "Could not close net log: %s", PHYSFS_getLastError());
		return false;
	}
	pFileHandle = NULL;

	return true;
}

void NETlogPacket(NETMSG *msg, BOOL received)
{
	if (msg->type >= NUM_GAME_PACKETS)
	{
		return;
	}
	packetcount[received][msg->type]++;
	packetsize[received][msg->type] += msg->size;
}

BOOL NETlogEntry(const char *str,UDWORD a,UDWORD b)
{
	static const char star_line[] = "************************************************************\n";
	static UDWORD lastframe = 0;
	UDWORD frame= frameGetFrameNumber();
	time_t aclock;
	struct tm *newtime;
	char buf[256];

	if (!pFileHandle)
	{
		return false;
	}

#ifndef MASSIVELOGS
	if(a ==9 || a==10)
	{
		return true;
	}
#endif

	time( &aclock );                 /* Get time in seconds */
	newtime = localtime( &aclock );  /* Convert time to struct */

	if (!newtime || a >= NET_GAME_FLAGS || !str || !pFileHandle)
	{
		debug(LOG_ERROR, "Fatal error averted in NETlog");
		return false;
	}

	// check to see if a new frame.
	if(frame != lastframe)
	{
		static const char dash_line[] = "-----------------------------------------------------------\n";

		lastframe = frame;

		PHYSFS_write(pFileHandle, dash_line, strlen(dash_line), 1);
	}

	if (a <= 51)
		// replace common msgs with txt descriptions
		snprintf(buf, sizeof(buf), "%s \t: %s \t:%d\t\t%s", str, packetname[a], b, asctime(newtime));
	else
		snprintf(buf, sizeof(buf), "%s \t:%d \t\t\t:%d\t\t%s", str, a, b, asctime(newtime));

	if (a == 56 || a==57 ) // NET_PLAYER_LEAVING || NET_PLAYER_DROPPED
		// Write a starry line above NET_LEAVING messages
		PHYSFS_write(pFileHandle, star_line, strlen(star_line), 1);

	PHYSFS_write(pFileHandle, buf, strlen( buf ), 1);

	if (a == 56 || a== 57 ) // NET_PLAYER_LEAVING ||NET_PLAYER_DROPPED
		// Write a starry line below NET_LEAVING messages
		PHYSFS_write(pFileHandle, star_line, strlen(star_line), 1);

	PHYSFS_flush(pFileHandle);
	return true;
}
