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
/***************************************************************************/

#ifndef _TRACK_H_
#define _TRACK_H_

/***************************************************************************/
/* defines */

#include "lib/framework/frame.h"

#ifndef WZ_NOSOUND
# ifdef WZ_OS_MAC
#  include <OpenAL/al.h>
# else
#  include <AL/al.h>
# endif
#endif

#ifndef MAX_STR
	#define	MAX_STR			255
#endif

#define	MAX_PITCH				127

#define	SAMPLE_NOT_ALLOCATED	-1
#define	SAMPLE_NOT_FOUND		-3
#define	SAMPLE_COORD_INVALID	-5

#define	AUDIO_VOL_MIN			0L
#define	AUDIO_VOL_MAX			100L
#define	AUDIO_VOL_RANGE			(AUDIO_VOL_MAX-AUDIO_VOL_MIN)

/***************************************************************************/

/***************************************************************************/
/* enums */


/***************************************************************************/
/* typedefs
 */

typedef BOOL (* AUDIO_CALLBACK)  ( void *psObj );

/***************************************************************************/
/* structs */

typedef struct AUDIO_SAMPLE
{
	SDWORD                  iTrack;         // ID number identifying a specific sound; currently (r1182) mapped in audio_id.c
#ifndef WZ_NOSOUND
	ALuint                  iSample;        // OpenAL name of the sound source
#endif
	SDWORD                  x, y, z;
	BOOL                    bFinishedPlaying;
	AUDIO_CALLBACK          pCallback;
	void                    *psObj;
	struct AUDIO_SAMPLE     *psPrev;
	struct AUDIO_SAMPLE     *psNext;
} AUDIO_SAMPLE;

typedef struct TRACK
{
	BOOL            bLoop;
	SDWORD          iVol;
	SDWORD          iAudibleRadius;
	SDWORD          iTime;                  // duration in milliseconds
	UDWORD          iTimeLastFinished;      // time last finished in ms
	UDWORD          iNumPlaying;
#ifndef WZ_NOSOUND
	ALuint          iBufferName;            // OpenAL name of the buffer
#endif
#if defined(WZ_C99)
	const char      fileName[];
#else
	const char*     fileName;
#endif /* WZ_C99 */
} TRACK;

/***************************************************************************/
/* functions
 */

BOOL	sound_Init(void);
BOOL	sound_Shutdown(void);

TRACK *	sound_LoadTrackFromFile(const char *fileName);
unsigned int sound_SetTrackVals(const char* fileName, BOOL loop, unsigned int volume, unsigned int audibleRadius);
void	sound_ReleaseTrack( TRACK * psTrack );

void	sound_StopTrack( AUDIO_SAMPLE *psSample );
void	sound_PauseTrack( AUDIO_SAMPLE *psSample );
void	sound_UpdateSample( AUDIO_SAMPLE *psSample );
void	sound_CheckAllUnloaded( void );

BOOL	sound_CheckTrack( SDWORD iTrack );

SDWORD	sound_GetTrackTime( SDWORD iTrack );
SDWORD	sound_GetTrackAudibleRadius( SDWORD iTrack );
SDWORD	sound_GetTrackVolume( SDWORD iTrack );
const char *	sound_GetTrackName( SDWORD iTrack );

BOOL	sound_TrackLooped( SDWORD iTrack );
void	sound_SetCallbackFunction( void * fn );

BOOL	sound_Play2DTrack( AUDIO_SAMPLE *psSample, BOOL bQueued );
BOOL	sound_Play3DTrack( AUDIO_SAMPLE *psSample );
void	sound_PlayWithCallback( AUDIO_SAMPLE *psSample, SDWORD iCurTime, AUDIO_CALLBACK pDoneFunc );
void	sound_FinishedCallback( AUDIO_SAMPLE *psSample );

BOOL	sound_GetSystemActive( void );
SDWORD	sound_GetTrackID( TRACK *psTrack );
SDWORD	sound_GetAvailableID( void );
SDWORD	sound_GetNumPlaying( SDWORD iTrack );

SDWORD	sound_GetGlobalVolume( void );
void	sound_SetGlobalVolume( SDWORD iVol );

void	sound_SetStoppedCallback( AUDIO_CALLBACK pStopTrackCallback );

UDWORD	sound_GetTrackTimeLastFinished( SDWORD iTrack );
void	sound_SetTrackTimeLastFinished( SDWORD iTrack, UDWORD iTime );

/***************************************************************************/

#endif	// _TRACK_H_

/***************************************************************************/
