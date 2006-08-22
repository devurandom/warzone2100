/*
 * Net join.
 * join related stuff
 */

#include "lib/framework/frame.h"
#include "netplay.h"

DWORD NETgetGameFlagsUnjoined(UDWORD gameid, UDWORD flag)
{
	switch(flag)
	{
	case 1:
		return NetPlay.games[gameid].desc.dwUser1;
		break;
	case 2:
		return NetPlay.games[gameid].desc.dwUser2;
		break;
	case 3:
		return NetPlay.games[gameid].desc.dwUser3;
		break;
	case 4:
		return NetPlay.games[gameid].desc.dwUser4;
		break;
	default:
		debug( LOG_ERROR, "Invalid flag for NETgetGameFlagsUnjoined in netplay lib" );
		abort();
		break;
	}
	return 0;
}

