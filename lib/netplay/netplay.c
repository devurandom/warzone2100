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
 * Netplay.c
 *
 */

// ////////////////////////////////////////////////////////////////////////
// includes

#include "lib/framework/frame.h"

#include <time.h>			// for stats
#include <SDL/SDL_thread.h>
#ifdef WZ_OS_MAC
#include <SDL_net/SDL_net.h>
#else
#include <SDL/SDL_net.h>
#endif
#include <physfs.h>
#include <string.h>

#include "netplay.h"
#include "netlog.h"

// WARNING !!! This is initialised via configuration.c !!!
char masterserver_name[255] = {'\0'};
unsigned int masterserver_port = 0, gameserver_port = 0;

#define MAX_CONNECTED_PLAYERS	8
#define MAX_TMP_SOCKETS		16

#define NET_READ_TIMEOUT	0
#define NET_BUFFER_SIZE		1024

#define MSG_JOIN		90
#define MSG_ACCEPTED		91
#define MSG_PLAYER_INFO		92
#define MSG_PLAYER_DATA		93
#define MSG_PLAYER_JOINED	94
#define MSG_PLAYER_LEFT		95
#define MSG_GAME_FLAGS		96

// ////////////////////////////////////////////////////////////////////////
// Function prototypes

static void NETallowJoining(void);
extern BOOL MultiPlayerJoin(UDWORD dpid);  /* from src/multijoin.c ! */
extern BOOL MultiPlayerLeave(UDWORD dpid); /* from src/multijoin.c ! */

/*
 * Network globals, these are part of the new network API
 */
NETMSG NetMsg;
static PACKETDIR NetDir;

// ////////////////////////////////////////////////////////////////////////
// Types

typedef struct		// data regarding the last one second or so.
{
	UDWORD		bytesRecvd;
	UDWORD		bytesSent;	// number of bytes sent in about 1 sec.
	UDWORD		packetsSent;
	UDWORD		packetsRecvd;
} NETSTATS;

typedef struct
{
	unsigned int	size;
	void*		data;
	unsigned int	buffer_size;
} NET_PLAYER_DATA;

typedef struct
{
	BOOL		allocated;
	unsigned int	id;
	char		name[StringSize];
	unsigned int	flags;
} NET_PLAYER;

typedef struct
{
	TCPsocket	socket;
	char*		buffer;
	unsigned int	buffer_start;
	unsigned int	bytes;
} NETBUFSOCKET;

#define PLAYER_HOST		1
#define PLAYER_SPECTATOR	2

// ////////////////////////////////////////////////////////////////////////
// Variables

NETPLAY	NetPlay;

static BOOL		allow_joining = FALSE;
static GAMESTRUCT	game;
static NET_PLAYER_DATA	local_player_data[MAX_CONNECTED_PLAYERS] = { { 0, NULL, 0 } };
static NET_PLAYER_DATA	global_player_data[MAX_CONNECTED_PLAYERS] = { { 0, NULL, 0 } };
static TCPsocket	tcp_socket = NULL;
static NETBUFSOCKET*	bsocket = NULL;
static NETBUFSOCKET*	connected_bsocket[MAX_CONNECTED_PLAYERS] = { NULL };
static SDLNet_SocketSet	socket_set = NULL;
static BOOL		is_server = FALSE;
static TCPsocket	tmp_socket[MAX_TMP_SOCKETS] = { 0 };
static SDLNet_SocketSet	tmp_socket_set = NULL;
static NETMSG		message;
static char*		hostname;
static NETSTATS		nStats = { 0, 0, 0, 0 };
static NET_PLAYER	players[MAX_CONNECTED_PLAYERS];
static SDWORD		NetGameFlags[4];

// *********** Socket with buffer that read NETMSGs ******************

static NETBUFSOCKET* NET_createBufferedSocket(void)
{
	NETBUFSOCKET* bs = (NETBUFSOCKET*)malloc(sizeof(NETBUFSOCKET));

	bs->socket = NULL;
	bs->buffer = NULL;
	bs->buffer_start = 0;
	bs->bytes = 0;

	return bs;
}

static void NET_destroyBufferedSocket(NETBUFSOCKET* bs)
{
	free(bs->buffer);
	free(bs);
}

static void NET_initBufferedSocket(NETBUFSOCKET* bs, TCPsocket s)
{
	bs->socket = s;
	if (bs->buffer == NULL) {
		bs->buffer = (char*)malloc(NET_BUFFER_SIZE);
	}
	bs->buffer_start = 0;
	bs->bytes = 0;
}

static BOOL NET_fillBuffer(NETBUFSOCKET* bs, SDLNet_SocketSet socket_set)
{
	int size;
	char* bufstart = bs->buffer + bs->buffer_start + bs->bytes;
	const int bufsize = NET_BUFFER_SIZE - bs->buffer_start - bs->bytes;


	if (bs->buffer_start != 0)
	{
		return FALSE;
	}

	if (SDLNet_SocketReady(bs->socket) <= 0)
	{
		return FALSE;
	}

	size = SDLNet_TCP_Recv(bs->socket, bufstart, bufsize);

	if (size > 0)
	{
		bs->bytes += size;
		return TRUE;
	} else {
		if (socket_set != NULL)
		{
			SDLNet_TCP_DelSocket(socket_set, bs->socket);
		}
		SDLNet_TCP_Close(bs->socket);
		bs->socket = NULL;
	}

	return FALSE;
}

// Check if we have a full message waiting for us. If not, return FALSE and wait for more data.
// If there is a data remnant somewhere in the buffer except at its beginning, move it to the
// beginning.
static BOOL NET_recvMessage(NETBUFSOCKET* bs, NETMSG* pMsg)
{
	unsigned int size;
	const NETMSG* message = (NETMSG*)(bs->buffer + bs->buffer_start);
	const unsigned int headersize =   sizeof(message->size)
					+ sizeof(message->type)
					+ sizeof(message->destination);

	if (headersize > bs->bytes)
	{
		goto error;
	}

	size = message->size + headersize;

	if (size > bs->bytes)
	{
		goto error;
	}

	debug(LOG_NET, "NETrecvMessage: received message of type %i and size %i.", message->type, message->size);

	memcpy(pMsg, message, size);
	bs->buffer_start += size;
	bs->bytes -= size;

	return TRUE;

error:
	if (bs->buffer_start != 0)
	{
		static char* tmp_buffer = NULL;
		char* buffer_start = bs->buffer + bs->buffer_start;
		char* tmp;

		// Create tmp buffer if necessary
		if (tmp_buffer == NULL)
		{
			tmp_buffer = (char*)malloc(NET_BUFFER_SIZE);
		}

		// Move remaining contents into tmp buffer
		memcpy(tmp_buffer, buffer_start, bs->bytes);

		// swap tmp buffer with buffer
		tmp = bs->buffer;
		bs->buffer = tmp_buffer;
		tmp_buffer = tmp;

		// Now data is in the beginning of the buffer
		bs->buffer_start = 0;
	}

	return FALSE;
}

static void NET_InitPlayers(void)
{
	unsigned int i;

	for (i = 0; i < MAX_CONNECTED_PLAYERS; ++i)
	{
		players[i].allocated = FALSE;
		players[i].id = i;
	}
}

static void NETBroadcastPlayerInfo(int dpid)
{
	message.type = MSG_PLAYER_INFO;
	message.size = sizeof(NET_PLAYER);
	memcpy(message.body, &players[dpid], sizeof(NET_PLAYER));
	NETbcast(&message, TRUE);
}

static unsigned int NET_CreatePlayer(const char* name, unsigned int flags)
{
	unsigned int i;

	for (i = 1; i < MAX_CONNECTED_PLAYERS; ++i)
	{
		if (players[i].allocated == FALSE)
		{
			players[i].allocated = TRUE;
			strncpy(players[i].name, name, sizeof(players[i].name));
			// Guarantee to nul-terminate
			players[i].name[sizeof(players[i].name) - 1] = '\0';
			players[i].flags = flags;
			NETBroadcastPlayerInfo(i);
			return i;
		}
	}

	return 0;
}

static void NET_DestroyPlayer(unsigned int id)
{
	players[id].allocated = FALSE;
}

// ////////////////////////////////////////////////////////////////////////
// count players. call with null to enumerate the game already joined.
UDWORD NETplayerInfo(void)
{
	unsigned int i;

	NetPlay.playercount = 0;		// reset player counter

	if(!NetPlay.bComms)
	{
		NetPlay.playercount		= 1;
		NetPlay.players[0].bHost	= TRUE;
		NetPlay.players[0].bSpectator	= FALSE;
		NetPlay.players[0].dpid		= 1;
		return 1;
	}

	memset(NetPlay.players, 0, (sizeof(PLAYER)*MaxNumberOfPlayers));	// reset player info

	for (i = 0; i < MAX_CONNECTED_PLAYERS; ++i)
	{
		if (players[i].allocated == TRUE)
		{
			NetPlay.players[NetPlay.playercount].dpid = i;
			strncpy(NetPlay.players[NetPlay.playercount].name, players[i].name, sizeof(NetPlay.players[NetPlay.playercount].name));
			// Guarantee to nul-terminate
			NetPlay.players[NetPlay.playercount].name[sizeof(NetPlay.players[NetPlay.playercount].name) - 1] = '\0';

			if (players[i].flags & PLAYER_HOST)
			{
				NetPlay.players[NetPlay.playercount].bHost = TRUE;
			}
			else
			{
				NetPlay.players[NetPlay.playercount].bHost = FALSE;
			}

			if (players[i].flags & PLAYER_SPECTATOR)
			{
				NetPlay.players[NetPlay.playercount].bSpectator = TRUE;
			}
			else
			{
				NetPlay.players[NetPlay.playercount].bSpectator = FALSE;
			}

			NetPlay.playercount++;
		}
	}

	return NetPlay.playercount;
}

// ////////////////////////////////////////////////////////////////////////
// rename the local player
// dont call this a lot, since it uses a guaranteed msg.
BOOL NETchangePlayerName(UDWORD dpid, char *newName)
{
	if(!NetPlay.bComms)
	{
		strncpy(NetPlay.players[0].name, newName, sizeof(NetPlay.players[0].name));
		// Guarantee to nul-terminate
		NetPlay.players[0].name[sizeof(NetPlay.players[0].name) - 1] = '\0';
		return TRUE;
	}

	strncpy(players[dpid].name, newName, sizeof(players[dpid].name));
	// Guarantee to nul-terminate
	players[dpid].name[sizeof(players[dpid].name) - 1] = '\0';

	NETBroadcastPlayerInfo(dpid);

	return TRUE;
}

static void resize_local_player_data(unsigned int i, unsigned int size)
{
	if (local_player_data[i].buffer_size < size)
	{
		if (local_player_data[i].data != NULL)
		{
			free(local_player_data[i].data);
		}
		local_player_data[i].data = malloc(size);
		local_player_data[i].buffer_size = size;
	}
}

static void resize_global_player_data(unsigned int i, unsigned int size)
{
	if (global_player_data[i].buffer_size < size)
	{
		if (global_player_data[i].data != NULL)
		{
			free(global_player_data[i].data);
		}
		global_player_data[i].data = malloc(size);
		global_player_data[i].buffer_size = size;
	}
}

// ////////////////////////////////////////////////////////////////////////
BOOL NETgetLocalPlayerData(UDWORD dpid, void *pData, SDWORD *pSize)
{
	memcpy(pData, local_player_data[dpid].data, local_player_data[dpid].size);
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
BOOL NETgetGlobalPlayerData(UDWORD dpid, void *pData, SDWORD *pSize)
{
	if(!NetPlay.bComms)
	{
		memcpy(pData, local_player_data[dpid].data, local_player_data[dpid].size);
		return TRUE;
	}

	memcpy(pData, global_player_data[dpid].data, global_player_data[dpid].size);

	return TRUE;
}
// ////////////////////////////////////////////////////////////////////////
BOOL NETsetLocalPlayerData(UDWORD dpid,void *pData, SDWORD size)
{
	debug(LOG_NET, "NETsetLocalPlayerData(%u, %p, %d)", dpid, pData, size);
	local_player_data[dpid].size = size;
	resize_local_player_data(dpid, size);
	memcpy(local_player_data[dpid].data, pData, size);
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
BOOL NETsetGlobalPlayerData(UDWORD dpid, void *pData, SDWORD size)
{
	debug(LOG_NET, "NETsetGlobalPlayerData(%u, %p, %d)", dpid, pData, size);
	if(!NetPlay.bComms)
	{
		local_player_data[dpid].size = size;
		resize_local_player_data(dpid, size);
		memcpy(local_player_data[dpid].data, pData, size);
		return TRUE;
	}

	global_player_data[dpid].size = size;
	resize_global_player_data(dpid, size);
	memcpy(global_player_data[dpid].data, pData, size);

	{ // broadcast player data
		unsigned int* p_dpid = (unsigned int*)(message.body);

		message.type = MSG_PLAYER_DATA;
		message.size = sizeof(unsigned int) + size;
		*p_dpid = dpid;
		memcpy(message.body+sizeof(unsigned int), pData, size);
	}

	NETBroadcastPlayerInfo(dpid);

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// return one of the four user flags in the current sessiondescription.
SDWORD NETgetGameFlags(UDWORD flag)
{
	if (flag < 1 || flag > 4)
	{
		return 0;
	}
	else
	{
		return NetGameFlags[flag-1];
	}
}

// ////////////////////////////////////////////////////////////////////////
// Set a game flag
BOOL NETsetGameFlags(UDWORD flag, SDWORD value)
{
	if(!NetPlay.bComms)
	{
		return TRUE;
	}

	if (flag < 1 || flag > 4)
	{
		return NetGameFlags[flag-1] = value;
	}

	message.type = MSG_GAME_FLAGS;
	message.size = sizeof(NetGameFlags);
	memcpy(message.body, NetGameFlags, sizeof(NetGameFlags));
	NETbcast(&message, TRUE);

	return TRUE;
}


// ////////////////////////////////////////////////////////////////////////
// setup stuff
BOOL NETinit(BOOL bFirstCall)
{
	UDWORD i;
	debug( LOG_NET, "NETinit" );

	if(bFirstCall)
	{
		debug(LOG_NET, "NETPLAY: Init called, MORNIN'");

		NetPlay.bLobbyLaunched		= FALSE;				// clean up
		NetPlay.dpidPlayer		= 0;
		NetPlay.bHost			= 0;
		NetPlay.bComms			= TRUE;

		NetPlay.bAllowCaptureRecord	= FALSE;
		NetPlay.bAllowCapturePlay	= FALSE;
		NetPlay.bCaptureInUse		= FALSE;

		for(i=0;i<MaxNumberOfPlayers;i++)
		{
			memset(&NetPlay.players[i], 0, sizeof(PLAYER));
			memset(&NetPlay.games[i], 0, sizeof(GAMESTRUCT));
		}
		//GAME_GUID = g;

		NetPlay.bComms = TRUE;
		NETstartLogging();
	}

	if (SDLNet_Init() == -1)
	{
		debug(LOG_ERROR, "SDLNet_Init: %s", SDLNet_GetError());
		return FALSE;
	}

	return TRUE;
}


// ////////////////////////////////////////////////////////////////////////
// SHUTDOWN THE CONNECTION.
BOOL NETshutdown(void)
{
	unsigned int i;
	debug( LOG_NET, "NETshutdown" );

	for( i = 0; i < MAX_CONNECTED_PLAYERS; i++ )
	{
		if( local_player_data[i].data != NULL )
		{
			free( local_player_data[i].data );
		}
	}

	NETstopLogging();
	SDLNet_Quit();
	return 0;
}

// ////////////////////////////////////////////////////////////////////////
//close the open game..
BOOL NETclose(void)
{
	unsigned int i;

	debug(LOG_NET, "NETclose");

	NEThaltJoining();
	is_server=FALSE;

	if(bsocket)
	{
		NET_destroyBufferedSocket(bsocket);
		bsocket=NULL;
	}

	for(i=0;i<MAX_CONNECTED_PLAYERS;i++) {
		if(connected_bsocket[i]) {
			NET_destroyBufferedSocket(connected_bsocket[i]);
			connected_bsocket[i]=NULL;
		}
		NET_DestroyPlayer(i);
	}

	if(tmp_socket_set)
	{
		SDLNet_FreeSocketSet(tmp_socket_set);
		tmp_socket_set=NULL;
	}

	for (i = 0; i < MAX_TMP_SOCKETS; i++)
	{
		if (tmp_socket[i])
		{
			SDLNet_TCP_Close(tmp_socket[i]);
			tmp_socket[i]=NULL;
		}
	}

	if (socket_set)
	{
		SDLNet_FreeSocketSet(socket_set);
		socket_set=NULL;
	}

	if (tcp_socket)
	{
		SDLNet_TCP_Close(tcp_socket);
		tcp_socket=NULL;
	}
	return FALSE;
}


// ////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////
// Send and Recv functions

// ////////////////////////////////////////////////////////////////////////
// return bytes of data sent recently.
UDWORD NETgetBytesSent(void)
{
	static UDWORD	lastsec=0;
	static UDWORD	timy=0;

	if(  (UDWORD)clock() > (timy+CLOCKS_PER_SEC) )
	{
		timy = clock();
		lastsec = nStats.bytesSent;
		nStats.bytesSent = 0;
	}

	return lastsec;
}

UDWORD NETgetRecentBytesSent(void)
{
	return nStats.bytesSent;
}


UDWORD NETgetBytesRecvd(void)
{
	static UDWORD	lastsec=0;
	static UDWORD	timy=0;
	if(  (UDWORD)clock() > (timy+CLOCKS_PER_SEC) )
	{
		timy = clock();
		lastsec = nStats.bytesRecvd;
		nStats.bytesRecvd = 0;
	}
	return lastsec;
}

UDWORD NETgetRecentBytesRecvd(void)
{
	return nStats.bytesRecvd;
}


//return number of packets sent last sec.
UDWORD NETgetPacketsSent(void)
{
	static UDWORD	lastsec=0;
	static UDWORD	timy=0;

	if(  (UDWORD)clock() > (timy+CLOCKS_PER_SEC) )
	{
		timy = clock();
		lastsec = nStats.packetsSent;
		nStats.packetsSent = 0;
	}

	return lastsec;
}


UDWORD NETgetRecentPacketsSent(void)
{
	return nStats.packetsSent;
}


UDWORD NETgetPacketsRecvd(void)
{
	static UDWORD	lastsec=0;
	static UDWORD	timy=0;
	if(  (UDWORD)clock() > (timy+CLOCKS_PER_SEC) )
	{
		timy = clock();
		lastsec = nStats.packetsRecvd;
		nStats.packetsRecvd = 0;
	}
	return lastsec;
}


// ////////////////////////////////////////////////////////////////////////
// Send a message to a player, option to guarantee message
BOOL NETsend(NETMSG *msg, UDWORD player, BOOL guarantee)
{
	int size;

	if(!NetPlay.bComms)
	{
		return TRUE;
	}

	if (player >= MAX_CONNECTED_PLAYERS) return FALSE;
	msg->destination = player;

	size = msg->size + sizeof(msg->size) + sizeof(msg->type) + sizeof(msg->destination);

	NETlogPacket(msg, FALSE);

	if (is_server)
	{
		if (   player < MAX_CONNECTED_PLAYERS
		    && connected_bsocket[player] != NULL
		    && connected_bsocket[player]->socket != NULL
		    && SDLNet_TCP_Send(connected_bsocket[player]->socket,
				       msg, size) == size)
		{
			nStats.bytesSent   += size;
			nStats.packetsSent += 1;
			return TRUE;
		}
	} else {
		if (   tcp_socket
		    && SDLNet_TCP_Send(tcp_socket, msg, size) == size)
		{
			return TRUE;
		}
	}

	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////
// broadcast a message to all players.
BOOL NETbcast(NETMSG *msg, BOOL guarantee)
{
	int size;

	if(!NetPlay.bComms)
	{
		return TRUE;
	}

	debug(LOG_NET, "NETbcast");

	msg->destination = NET_ALL_PLAYERS;

	size = msg->size + sizeof(msg->size) + sizeof(msg->type) + sizeof(msg->destination);

	NETlogPacket(msg, FALSE);

	if (is_server)
	{
		unsigned int i;

		for (i = 0; i < MAX_CONNECTED_PLAYERS; ++i)
		{
			if (   connected_bsocket[i] != NULL
			    && connected_bsocket[i]->socket != NULL)
			{
				SDLNet_TCP_Send(connected_bsocket[i]->socket, msg, size);
			}
		}

	} else {
		if (   tcp_socket == NULL
		    || SDLNet_TCP_Send(tcp_socket, msg, size) < size)
		{
			return FALSE;
		}
	}

	nStats.bytesSent   += size;
	nStats.packetsSent += 1;

	return TRUE;
}

///////////////////////////////////////////////////////////////////////////
// Check if a message is a system message
BOOL NETprocessSystemMessage(NETMSG * pMsg)
{
	debug(LOG_NET, "NETprocessSystemMessage with packet of type %hhu", pMsg->type);

	switch (pMsg->type)
	{
		case MSG_PLAYER_INFO: {
			NET_PLAYER* pi = (NET_PLAYER*)(pMsg->body);
			int dpid = pi->id;

			debug(LOG_NET, "NETprocessSystemMessage: Receiving MSG_PLAYER_INFO for player %d", dpid);

			memcpy(&players[dpid], pi, sizeof(NET_PLAYER));
			NETplayerInfo();

			if (is_server)
			{
				NETBroadcastPlayerInfo(dpid);
			}
		}	break;

		case MSG_PLAYER_DATA: {
			unsigned int* pdpid = (unsigned int*)(pMsg->body);
			int dpid = *pdpid;
			unsigned int size = pMsg->size - sizeof(unsigned int);

			debug(LOG_NET, "NETprocessSystemMessage: Receiving MSG_PLAYER_DATA for player %d", dpid);

			global_player_data[dpid].size = size;
			resize_global_player_data(dpid, size);
			memcpy(global_player_data[dpid].data, pMsg->body+sizeof(unsigned int), size);

			if (is_server)
			{
				NETbcast(pMsg, TRUE);
			}
		}	break;

		case MSG_PLAYER_JOINED: {
			int dpid = pMsg->body[0];

			debug(LOG_NET, "NETprocessSystemMessage: Receiving MSG_PLAYER_JOINED for player %d", dpid);

			MultiPlayerJoin(dpid);
		}	break;

		case MSG_PLAYER_LEFT: {
			unsigned int* pdpid = (unsigned int*)(pMsg->body);
			int dpid = *pdpid;

			debug(LOG_NET, "NETprocessSystemMessage: Receiving MSG_PLAYER_LEFT for player %d", dpid);

			NET_DestroyPlayer(dpid);
			MultiPlayerLeave(dpid);
		}	break;

		case MSG_GAME_FLAGS: {
			debug(LOG_NET, "NETprocessSystemMessage: Receiving game flags");

			memcpy(NetGameFlags, pMsg->body, sizeof(NetGameFlags));

			if (is_server)
			{
				NETbcast(pMsg, TRUE);
			}
		}	break;
		default:
			return FALSE;
	}

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// Receive a message over the current connection. We return TRUE if there
// is a message for the higher level code to process, and FALSE otherwise.
// We should not block here.
BOOL NETrecv(NETMSG * pMsg)
{
	static unsigned int current = 0;
	BOOL received;
	int size;

	if (!NetPlay.bComms)
	{
		return FALSE;
	}

	if (is_server)
	{
		NETallowJoining();
	}

	do {
receive_message:
		received = FALSE;

		if (is_server)
		{
			if (connected_bsocket[current] == NULL)
			{
				return FALSE;
			}

			received = NET_recvMessage(connected_bsocket[current], pMsg);

			if (received == FALSE)
			{
				unsigned int i = current + 1;

				if (socket_set == NULL
				    || SDLNet_CheckSockets(socket_set, NET_READ_TIMEOUT) <= 0)
				{
					return FALSE;
				}
				for (;;)
				{
					if (connected_bsocket[i]->socket == NULL)
					{
						// do nothing
					}
					else if (NET_fillBuffer(connected_bsocket[i], socket_set))
					{
						// we received some data, add to buffer
						received = NET_recvMessage(connected_bsocket[i], pMsg);
						current = i;
						break;
					}
					else if (connected_bsocket[i]->socket == NULL)
					{
						// check if we droped any players in the check above
						unsigned int* message_dpid = (unsigned int*)(message.body);

						game.desc.dwCurrentPlayers--;

						message.type = MSG_PLAYER_LEFT;
						message.size = 4;
						*message_dpid = i;
						debug(LOG_NET, "NETrecv: dpid to send set to %d", i);
						NETbcast(&message, TRUE);

						NET_DestroyPlayer(i);
						MultiPlayerLeave(i);
					}

					if (++i == MAX_CONNECTED_PLAYERS)
					{
						i = 0;
					}

					if (i == current+1)
					{
						return FALSE;
					}
				}
			}
		} else {
			// we are a client
			if (bsocket == NULL)
			{
				return FALSE;
			} else {
				received = NET_recvMessage(bsocket, pMsg);

				if (received == FALSE)
				{
					if (   socket_set != NULL
					    && SDLNet_CheckSockets(socket_set, NET_READ_TIMEOUT) > 0
					    && NET_fillBuffer(bsocket, socket_set))
					{
						received = NET_recvMessage(bsocket, pMsg);
					}
				}
			}
		}

		if (received == FALSE)
		{
			return FALSE;
		}
		else
		{
			size =	  pMsg->size + sizeof(pMsg->size) + sizeof(pMsg->type)
				+ sizeof(pMsg->destination);
			if (is_server == FALSE)
			{
				// do nothing
			}
			else if (pMsg->destination == NET_ALL_PLAYERS)
			{
				unsigned int j;

				// we are the host, and have received a broadcast packet; distribute it
				for (j = 0; j < MAX_CONNECTED_PLAYERS; ++j)
				{
					if (   j != current
					    && connected_bsocket[j] != NULL
					    && connected_bsocket[j]->socket != NULL)
					{
						SDLNet_TCP_Send(connected_bsocket[j]->socket,
								pMsg, size);
					}
				}
			}
			else if (pMsg->destination != NetPlay.dpidPlayer)
			{
				// message was not meant for us; send it further
				if (   pMsg->destination < MAX_CONNECTED_PLAYERS
				    && connected_bsocket[pMsg->destination] != NULL
				    && connected_bsocket[pMsg->destination]->socket != NULL)
				{
					debug(LOG_NET, "Reflecting message type %hhu to UDWORD %hhu", pMsg->type, pMsg->destination);
					SDLNet_TCP_Send(connected_bsocket[pMsg->destination]->socket,
							pMsg, size);
				} else {
					debug(LOG_NET, "Cannot reflect message type %hhu to %hhu", pMsg->type, pMsg->destination);
				}

				goto receive_message;
			}

			nStats.bytesRecvd   += size;
			nStats.packetsRecvd += 1;
		}

	} while (NETprocessSystemMessage(pMsg) == TRUE);

	NETlogPacket(pMsg, TRUE);

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////
// Protocol functions

BOOL NETsetupTCPIP(void ** addr, char * machine)
{
	debug(LOG_NET, "NETsetupTCPIP(,%s)", machine ? machine : "NULL");

	if (   hostname != NULL
	    && hostname != masterserver_name)
	{
		free(hostname);
	}
	if (   machine != NULL
	    && machine[0] != '\0')
	{
		hostname = strdup(machine);
	} else {
		hostname = masterserver_name;
	}

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// File Transfer programs.
// uses guaranteed messages to send files between clients.
// send file. it returns % of file sent. when 100 it's complete. call until it returns 100.
UBYTE NETsendFile(BOOL newFile, const char *fileName, UDWORD player)
{
	static PHYSFS_sint64	fileSize,currPos;
	static PHYSFS_file	*pFileHandle;
	PHYSFS_sint64		bytesRead;
	UBYTE		inBuff[2048];
	NETMSG		msg;

	if(newFile)		//force it true for now -Q
	{
		// open the file.
		pFileHandle = PHYSFS_openRead(fileName);			// check file exists
		if (pFileHandle == NULL)
		{
			debug(LOG_ERROR, "NETsendFile: Failed");
			return 0; // failed
		}
		// get the file's size.
		fileSize = 0;
		currPos =0;
		do
		{
			bytesRead = PHYSFS_read( pFileHandle, &inBuff, 1, sizeof(inBuff) );
			fileSize += bytesRead;
		} while(bytesRead != 0);

		PHYSFS_seek(pFileHandle, 0 );
	}
	// read some bytes.
	bytesRead = PHYSFS_read(pFileHandle, &inBuff, sizeof(inBuff), 1 );

	// form a message
	NetAdd(msg,0,fileSize);		// total bytes in this file.
	NetAdd(msg,4,bytesRead);	// bytes in this packet
	NetAdd(msg,8,currPos);		// start byte
	msg.body[12]=strlen(fileName);
	msg.size	= 13;
	NetAddSt(msg,msg.size,fileName);
	msg.size	+= strlen(fileName);
	memcpy(&(msg.body[msg.size]),&inBuff,bytesRead);
	msg.size	+= bytesRead;
	msg.type	=  FILEMSG;
	if(player==0)
	{
		NETbcast(&msg,TRUE);		// send it.
	}
	else
	{
		NETsend(&msg,player,TRUE);
	}

	currPos += bytesRead;		// update position!
	if(currPos == fileSize)
	{
		PHYSFS_close(pFileHandle);
	}

	return (currPos*100)/fileSize;
}


// recv file. it returns % of the file so far recvd.
UBYTE NETrecvFile(NETMSG *pMsg)
{
	UDWORD			pos, fileSize, currPos, bytesRead;
	char			fileName[256];
	unsigned int		len;
	static PHYSFS_file	*pFileHandle;

	//read incoming bytes.
	NetGet(pMsg,0,fileSize);
	NetGet(pMsg,4,bytesRead);
	NetGet(pMsg,8,currPos);

	// read filename
	len = (unsigned int)(pMsg->body[12]);
	memcpy(fileName,&(pMsg->body[13]),len);
	fileName[len] = '\0';	// terminate string.
	pos = 13+len;
	debug(LOG_NET, "NETrecvFile: Creating new file %s", fileName);

	if(currPos == 0)	// first packet!
	{
		pFileHandle = PHYSFS_openWrite(fileName);	// create a new file.
	}

	//write packet to the file.
	PHYSFS_write( pFileHandle, &(pMsg->body[pos]), bytesRead, 1 );

	if(currPos+bytesRead == fileSize)	// last packet
	{
		PHYSFS_close(pFileHandle);
	}

	//return the percent count.
	return ((currPos+bytesRead)*100) / fileSize;
}

void NETregisterServer(int state)
{
	static TCPsocket rs_socket = NULL;
	static int registered = 0;
	static int server_not_there = 0;
	IPaddress ip;

	if (server_not_there)
	{
		return;
	}

	if (state != registered)
	{
		switch(state)
		{
			case 1: {
				if(SDLNet_ResolveHost(&ip, masterserver_name, masterserver_port) == -1)
				{
					debug(LOG_ERROR, "NETregisterServer: Cannot resolve masterserver \"%s\": %s", masterserver_name, SDLNet_GetError());
					server_not_there = 1;
					return;
				}

				if(!rs_socket) rs_socket = SDLNet_TCP_Open(&ip);
				if(rs_socket == NULL)
				{
					debug(LOG_ERROR, "NETregisterServer: Cannot connect to masterserver \"%s:%d\": %s", masterserver_name, masterserver_port, SDLNet_GetError());
					server_not_there = 1;
					return;
				}

				SDLNet_TCP_Send(rs_socket, "addg", 5);
				SDLNet_TCP_Send(rs_socket, &game, sizeof(GAMESTRUCT));
			}
			break;

			case 0:
				SDLNet_TCP_Close(rs_socket);
				rs_socket=NULL;
			break;
		}
		registered=state;
	}
}


// ////////////////////////////////////////////////////////////////////////
// Host a game with a given name and player name. & 4 user game flags

static void NETallowJoining(void)
{
	unsigned int i;
	UDWORD numgames = SDL_SwapBE32(1);	// always 1 on normal server
	char buffer[5];

	if (allow_joining == FALSE) return;

	NETregisterServer(1);

	if (tmp_socket_set == NULL)
	{
		// initialize server socket set
		// FIXME: why is this not done in NETinit()?? - Per
		tmp_socket_set = SDLNet_AllocSocketSet(MAX_TMP_SOCKETS+1);
		if (tmp_socket_set == NULL)
		{
			debug(LOG_ERROR, "NETallowJoining: Cannot create socket set: %s", SDLNet_GetError());
			return;
		}
		SDLNet_TCP_AddSocket(tmp_socket_set, tcp_socket);
	}

	if (SDLNet_CheckSockets(tmp_socket_set, NET_READ_TIMEOUT) > 0)
	{
		if (SDLNet_SocketReady(tcp_socket))
		{
			for (i = 0; i < MAX_TMP_SOCKETS; ++i)
			{
				if (tmp_socket[i] == NULL)
				{
					break;
				}
			}
			tmp_socket[i] = SDLNet_TCP_Accept(tcp_socket);
			SDLNet_TCP_AddSocket(tmp_socket_set, tmp_socket[i]);
			if (SDLNet_CheckSockets(tmp_socket_set, 1000) > 0
			    && SDLNet_SocketReady(tmp_socket)
			    && SDLNet_TCP_Recv(tmp_socket[i], buffer, 5))
			{
				if(strcmp(buffer, "list")==0)
				{
					SDLNet_TCP_Send(tmp_socket[i], &numgames, sizeof(UDWORD));
					SDLNet_TCP_Send(tmp_socket[i], &game, sizeof(GAMESTRUCT));
				}
				else if (strcmp(buffer, "join") == 0)
				{
					SDLNet_TCP_Send(tmp_socket[i], &game, sizeof(GAMESTRUCT));
				}

			} else {
				return;
			}
		}
		for(i = 0; i < MAX_TMP_SOCKETS; ++i)
		{
			if (   tmp_socket[i] != NULL
			    && SDLNet_SocketReady(tmp_socket[i]) > 0)
			{
				int size = SDLNet_TCP_Recv(tmp_socket[i], &message, sizeof(NETMSG));

				if (size <= 0)
				{
					// socket probably disconnected.
					SDLNet_TCP_DelSocket(tmp_socket_set, tmp_socket[i]);
					SDLNet_TCP_Close(tmp_socket[i]);
					tmp_socket[i] = NULL;
				}
				else if (message.type == MSG_JOIN)
				{
					int j;

					char* name = message.body;
					unsigned int dpid = NET_CreatePlayer(name, 0);

					debug(LOG_NET, "NETallowJoining, MSG_JOIN: dpid set to %d", dpid);
					SDLNet_TCP_DelSocket(tmp_socket_set, tmp_socket[i]);
					NET_initBufferedSocket(connected_bsocket[dpid], tmp_socket[i]);
					SDLNet_TCP_AddSocket(socket_set, connected_bsocket[dpid]->socket);
					tmp_socket[i] = NULL;

					game.desc.dwCurrentPlayers++;

					message.type = MSG_ACCEPTED;
					message.size = 1;
					message.body[0] = dpid;
					NETsend(&message, dpid, TRUE);

					MultiPlayerJoin(dpid);
					message.type = MSG_PLAYER_JOINED;
					message.size = 1;

					// Send info about players to newcomer.
					for (j = 0; j < MAX_CONNECTED_PLAYERS; ++j)
					{
						if (   players[j].allocated
						    && dpid != players[j].id)
						{
							message.body[0] = players[j].id;
							NETsend(&message, dpid, TRUE);
						}
					}

					// Send info about newcomer to all players.
					message.body[0] = dpid;
					NETbcast(&message, TRUE);

					for (j = 0; j < MAX_CONNECTED_PLAYERS; ++j)
					{
						NETBroadcastPlayerInfo(j);
					}

					// Make sure the master server gets updated by disconnecting from it
					// NETallowJoining will reconnect
					NETregisterServer(0);
				}
			}
		}
	}
}

BOOL NEThostGame(const char* SessionName, const char* PlayerName,
		 SDWORD one, SDWORD two, SDWORD three, SDWORD four,
		 UDWORD plyrs)	// # of players.
{
	IPaddress ip;
	unsigned int i;

	debug(LOG_NET, "NEThostGame(%s, %s, %d, %d, %d, %d, %u)", SessionName, PlayerName,
	      one, two, three, four, plyrs);

	if(!NetPlay.bComms)
	{
		NetPlay.dpidPlayer		= 1;
		NetPlay.bHost			= TRUE;

		return TRUE;
	}

	if(SDLNet_ResolveHost(&ip, NULL, gameserver_port) == -1)
	{
		debug(LOG_ERROR, "NEThostGame: Cannot resolve master self: %s", SDLNet_GetError());
		return FALSE;
	}

	if(!tcp_socket) tcp_socket = SDLNet_TCP_Open(&ip);
	if(tcp_socket == NULL)
	{
		printf("NEThostGame: Cannot connect to master self: %s", SDLNet_GetError());
		return FALSE;
	}

	if(!socket_set) socket_set = SDLNet_AllocSocketSet(MAX_CONNECTED_PLAYERS);
	if (socket_set == NULL)
	{
		debug(LOG_ERROR, "NEThostGame: Cannot create socket set: %s", SDLNet_GetError());
		return FALSE;
	}
	for (i = 0; i < MAX_CONNECTED_PLAYERS; ++i)
	{
		connected_bsocket[i] = NET_createBufferedSocket();
	}

	is_server = TRUE;

	strncpy(game.name, SessionName, sizeof(game.name));
	// Guarantee to nul-terminate
	game.name[sizeof(game.name) - 1] = '\0';
	memset(&game.desc, 0, sizeof(SESSIONDESC));
	game.desc.dwSize = sizeof(SESSIONDESC);
	//game.desc.guidApplication = GAME_GUID;
	game.desc.host[0] = '\0';
	game.desc.dwCurrentPlayers = 1;
	game.desc.dwMaxPlayers = plyrs;
	game.desc.dwFlags = 0;
	game.desc.dwUser1 = one;
	game.desc.dwUser2 = two;
	game.desc.dwUser3 = three;
	game.desc.dwUser4 = four;

	NET_InitPlayers();
	NetPlay.dpidPlayer	= NET_CreatePlayer(PlayerName, PLAYER_HOST);
	NetPlay.bHost		= TRUE;
	NetPlay.bSpectator	= FALSE;

	MultiPlayerJoin(NetPlay.dpidPlayer);

	allow_joining = TRUE;

	NETregisterServer(0);

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// Stop the dplay interface from accepting more players.
BOOL NEThaltJoining(void)
{
	debug(LOG_NET, "NEThaltJoining");

	allow_joining = FALSE;
	// disconnect from the master server
	NETregisterServer(0);
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// find games on open connection
BOOL NETfindGame(void)
{
	static UDWORD gamecount = 0, gamesavailable;
	IPaddress ip;
	char buffer[sizeof(GAMESTRUCT)*2];
	GAMESTRUCT* tmpgame = (GAMESTRUCT*)buffer;
	unsigned int port = (hostname == masterserver_name) ? masterserver_port : gameserver_port;

	debug(LOG_NET, "NETfindGame");

	gamecount = 0;
	NetPlay.games[0].desc.dwSize = 0;
	NetPlay.games[0].desc.dwCurrentPlayers = 0;
	NetPlay.games[0].desc.dwMaxPlayers = 0;

	if(!NetPlay.bComms)
	{
		NetPlay.dpidPlayer		= 1;
		NetPlay.bHost			= TRUE;
		return TRUE;
	}

	if (SDLNet_ResolveHost(&ip, hostname, port) == -1)
	{
		debug(LOG_ERROR, "NETfindGame: Cannot resolve hostname \"%s\": %s", hostname, SDLNet_GetError());
		return FALSE;
	}

	if (tcp_socket != NULL)
	{
		SDLNet_TCP_Close(tcp_socket);
	}

	tcp_socket = SDLNet_TCP_Open(&ip);
	if (tcp_socket == NULL)
	{
		debug(LOG_ERROR, "NETfindGame: Cannot connect to \"%s:%d\": %s", hostname, port, SDLNet_GetError());
		return FALSE;
	}

	socket_set = SDLNet_AllocSocketSet(1);
	if (socket_set == NULL)
	{
		debug(LOG_ERROR, "NETfindGame: Cannot create socket set: %s", SDLNet_GetError());
		return FALSE;
	}
	SDLNet_TCP_AddSocket(socket_set, tcp_socket);

	SDLNet_TCP_Send(tcp_socket, "list", 5);

	if (   SDLNet_CheckSockets(socket_set, 1000) > 0
	    && SDLNet_SocketReady(tcp_socket)
	    && SDLNet_TCP_Recv(tcp_socket, &gamesavailable, sizeof(gamesavailable)))
	{
		gamesavailable = SDL_SwapBE32(gamesavailable);
	}

	debug(LOG_NET, "receiving info of %u game(s)", gamesavailable);

	do {
		if (   SDLNet_CheckSockets(socket_set, 1000) > 0
		    && SDLNet_SocketReady(tcp_socket)
		    && SDLNet_TCP_Recv(tcp_socket, buffer, sizeof(GAMESTRUCT)) == sizeof(GAMESTRUCT)
		    && tmpgame->desc.dwSize == sizeof(SESSIONDESC))
		{
			strncpy(NetPlay.games[gamecount].name, tmpgame->name, sizeof(NetPlay.games[gamecount].name));
			// Guarantee to nul-terminate
			NetPlay.games[gamecount].name[sizeof(NetPlay.games[gamecount].name) - 1] = '\0';
			NetPlay.games[gamecount].desc.dwSize = tmpgame->desc.dwSize;
			NetPlay.games[gamecount].desc.dwCurrentPlayers = tmpgame->desc.dwCurrentPlayers;
			NetPlay.games[gamecount].desc.dwMaxPlayers = tmpgame->desc.dwMaxPlayers;
			if (tmpgame->desc.host[0] == '\0')
			{
				unsigned char* address = (unsigned char*)(&(ip.host));

				snprintf(NetPlay.games[gamecount].desc.host, sizeof(NetPlay.games[gamecount].desc.host),
					"%i.%i.%i.%i",
 					(int)(address[0]),
 					(int)(address[1]),
 					(int)(address[2]),
 					(int)(address[3]));
			}
			else
			{
				strncpy(NetPlay.games[gamecount].desc.host, tmpgame->desc.host, sizeof(NetPlay.games[gamecount].desc.host));
			}

			// Guarantee to nul-terminate
			NetPlay.games[gamecount].desc.host[sizeof(NetPlay.games[gamecount].desc.host) - 1] = '\0';

			gamecount++;
		}
	} while (gamecount<gamesavailable);

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////
// Functions used to setup and join games.
BOOL NETjoinGame(UDWORD gameNumber, const char* playername)
{
	char* name;
	IPaddress ip;
	char buffer[sizeof(GAMESTRUCT)*2];
	GAMESTRUCT* tmpgame = (GAMESTRUCT*)buffer;

	debug(LOG_NET, "NETjoinGame gameNumber=%d", gameNumber);

	NETclose();	// just to be sure :)

	if (hostname != masterserver_name)
	{
		free(hostname);
	}
	hostname = strdup(NetPlay.games[gameNumber].desc.host);

	if(SDLNet_ResolveHost(&ip, hostname, gameserver_port) == -1)
	{
		debug(LOG_ERROR, "NETjoinGame: Cannot resolve hostname \"%s\": %s", hostname, SDLNet_GetError());
		return FALSE;
	}

	if (tcp_socket != NULL)
	{
		SDLNet_TCP_Close(tcp_socket);
	}

	tcp_socket = SDLNet_TCP_Open(&ip);
 	if (tcp_socket == NULL)
	{
		debug(LOG_ERROR, "NETjoinGame: Cannot connect to \"%s:%d\": %s", hostname, gameserver_port, SDLNet_GetError());
		return FALSE;
	}

	socket_set = SDLNet_AllocSocketSet(1);
	if (socket_set == NULL)
	{
		debug(LOG_ERROR, "NETjoinGame: Cannot create socket set: %s", SDLNet_GetError());
 		return FALSE;
 	}
	SDLNet_TCP_AddSocket(socket_set, tcp_socket);

	SDLNet_TCP_Send(tcp_socket, "join", 5);

	if (   SDLNet_CheckSockets(socket_set, 1000) > 0
	    && SDLNet_SocketReady(tcp_socket)
	    && SDLNet_TCP_Recv(tcp_socket, buffer, sizeof(GAMESTRUCT)*2) == sizeof(GAMESTRUCT)
	    && tmpgame->desc.dwSize == sizeof(SESSIONDESC))
	{
		strncpy(NetPlay.games[gameNumber].name, tmpgame->name, sizeof(NetPlay.games[gameNumber].name));
		// Guarantee to nul-terminate
		NetPlay.games[gameNumber].name[sizeof(NetPlay.games[gameNumber].name) - 1] = '\0';

		NetPlay.games[gameNumber].desc.dwSize = tmpgame->desc.dwSize;
		NetPlay.games[gameNumber].desc.dwCurrentPlayers = tmpgame->desc.dwCurrentPlayers;
		NetPlay.games[gameNumber].desc.dwMaxPlayers = tmpgame->desc.dwMaxPlayers;
		strncpy(NetPlay.games[gameNumber].desc.host, tmpgame->desc.host, sizeof(NetPlay.games[gameNumber].desc.host));
		// Guarantee to nul-terminate
		NetPlay.games[gameNumber].desc.host[sizeof(NetPlay.games[gameNumber].desc.host) - 1] = '\0';
		if (tmpgame->desc.host[0] == '\0')
		{
			unsigned char* address = (unsigned char*)(&(ip.host));

			snprintf(NetPlay.games[gameNumber].desc.host, sizeof(NetPlay.games[gameNumber].desc.host),
				"%i.%i.%i.%i",
				(int)(address[0]),
				(int)(address[1]),
				(int)(address[2]),
				(int)(address[3]));
		}
		else
		{
			strncpy(NetPlay.games[gameNumber].desc.host, tmpgame->desc.host, sizeof(NetPlay.games[gameNumber].desc.host));
		}

		// Guarantee to nul-terminate
		NetPlay.games[gameNumber].desc.host[sizeof(NetPlay.games[gameNumber].desc.host) - 1] = '\0';
	}

	bsocket = NET_createBufferedSocket();
	NET_initBufferedSocket(bsocket, tcp_socket);

	message.type = MSG_JOIN;
	message.size = 64;
	name = message.body;
	strcpy(name, playername);
	NETsend(&message, 1, TRUE);

	for (;;)
	{
		NETrecv(&message);

		if (message.type == MSG_ACCEPTED)
		{
			NetPlay.dpidPlayer = message.body[0];
			debug(LOG_NET, "NETjoinGame: I'm player %u", NetPlay.dpidPlayer);
			NetPlay.bHost = FALSE;
			NetPlay.bSpectator = FALSE;

			if (NetPlay.dpidPlayer >= MAX_CONNECTED_PLAYERS)
			{
				debug(LOG_ERROR, "Bad player number (%u) received from host!",
				      NetPlay.dpidPlayer);
				return FALSE;
			}
			players[NetPlay.dpidPlayer].allocated = TRUE;
			players[NetPlay.dpidPlayer].id = NetPlay.dpidPlayer;
			strncpy(players[NetPlay.dpidPlayer].name, playername, sizeof(players[NetPlay.dpidPlayer].name));
			// Guarantee to nul-terminate
			players[NetPlay.dpidPlayer].name[sizeof(players[NetPlay.dpidPlayer].name) - 1] = '\0';
			players[NetPlay.dpidPlayer].flags = 0;

			return TRUE;
		}
	}

	return FALSE;
}

void NETsetPacketDir(PACKETDIR dir)
{
    NetDir = dir;
}

PACKETDIR NETgetPacketDir()
{
    return NetDir;
}

/*!
 * Set the masterserver name
 * \param hostname The hostname of the masterserver to connect to
 */
void NETsetMasterserverName(const char* hostname)
{
	size_t name_size = strlen(hostname);

	if ( name_size > 255 )
	{
		name_size = 255;
	}
	strncpy(masterserver_name, hostname, name_size);
}


/*!
 * Set the masterserver port
 * \param port The port of the masterserver to connect to
 */
void NETsetMasterserverPort(unsigned int port)
{
	masterserver_port = port;
}


/*!
 * Set the port we shall host games on
 * \param port The port to listen to
 */
void NETsetGameserverPort(unsigned int port)
{
	gameserver_port = port;
}
