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
/*
 * Library-specific sound library functions;
 * these need to be re-written for each library.
 */
/***************************************************************************/

#ifndef _TRACKLIB_H_
#define _TRACKLIB_H_

/***************************************************************************/



#include "track.h"

/***************************************************************************/

#define	KHZ22					(22050L)
#define	KHZ11					(11025L)
#define	MAX_AUDIO_SAMPLES		20

/***************************************************************************/

BOOL	sound_InitLibrary( void );
void	sound_ShutdownLibrary( void );

void	sound_FreeTrack( TRACK * psTrack );

BOOL	sound_Play2DSample( TRACK * psTrack, AUDIO_SAMPLE * psSample,
							BOOL bQueued );
BOOL	sound_Play3DSample( TRACK * psTrack, AUDIO_SAMPLE * psSample );
void	sound_StopSample( UDWORD iSample );
void	sound_PauseSample( AUDIO_SAMPLE * psSample );
void	sound_ResumeSample( AUDIO_SAMPLE * psSample );

BOOL	sound_PlayStream( AUDIO_SAMPLE *psSample, const char szFileName[],
							SDWORD iVol );

void	sound_SetSampleFreq( AUDIO_SAMPLE * psSample, SDWORD iFreq );
void	sound_SetSampleVol( AUDIO_SAMPLE * psSample, SDWORD iVol,
							BOOL bScale3D );

int		sound_GetNumSamples( void );
BOOL	sound_SampleIsFinished( AUDIO_SAMPLE * psSample );
BOOL	sound_QueueSamplePlaying( void );

void	sound_SetPlayerPos( SDWORD iX, SDWORD iY, SDWORD iZ );
void	sound_SetPlayerOrientation( SDWORD iX, SDWORD iY, SDWORD iZ );
void	sound_SetObjectPosition( SDWORD iSample,
									SDWORD iX, SDWORD iY, SDWORD iZ );

void *	sound_GetObject( SDWORD iSample );
void	sound_SetObject( SDWORD iSample, void *pObj );

void	sound_SetCallback( SDWORD iSample, AUDIO_CALLBACK pCallBack );

void	sound_PauseAll( void );
void	sound_ResumeAll( void );
void	sound_StopAll( void );
void	sound_Update( void );

UDWORD	sound_GetGameTime( void );

/***************************************************************************/

#endif	// _TRACKLIB_H_

/***************************************************************************/
