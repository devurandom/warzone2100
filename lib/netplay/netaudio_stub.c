// FIXME Stubfile!

/*
 * NetAudio.c
 *
 * intra/internet audio
 */

#include "lib/framework/frame.h"
#include "netplay.h"

// ////////////////////////////////////////////////////////////////////////
BOOL NETinitAudioCapture(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETinitAudioCapture");
	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////
BOOL NETshutdownAudioCapture(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETshutdownAudioCapture");
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////
WZ_DEPRECATED BOOL NETstartAudioCapture(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETstartAudioCapture");
	return FALSE;
}
// ////////////////////////////////////////////////////////////////////////
BOOL NETstopAudioCapture(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETstopAudioCapture");
	return FALSE;
}


// ////////////////////////////////////////////////////////////////////////
// update the pointers and process the buffer accordingly.
BOOL NETprocessAudioCapture(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETprocessAudioCapture");
	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////

BOOL NETinitPlaybackBuffer(LPDIRECTSOUND pDs) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETinitPlaybackBuffer");
	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////
// handle the playback buffer.
BOOL NETqueueIncomingAudio( LPBYTE lpbSoundData, DWORD dwSoundBytes,BOOL bStream) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETqueueIncomingAudio");
	return FALSE;
}

// ////////////////////////////////////////////////////////////////////////
// Handle a incoming message that needs to be played
void NETplayIncomingAudio(NETMSG *pMsg) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETplayIncomingAudio");
}

// ////////////////////////////////////////////////////////////////////////
// close it all down
BOOL NETshutdownAudioPlayback(VOID) // FIXME Remove if unused
{
	debug(LOG_SOUND, "NETshutdownAudioPlayback");
	return TRUE;
}
