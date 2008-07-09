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

#ifndef __INCLUDED_LIB_SOUND_AUDIO_H__
#define __INCLUDED_LIB_SOUND_AUDIO_H__

#include "track.h"

#if defined(__cplusplus)
extern "C"
{
#endif

extern BOOL		audio_Init( AUDIO_CALLBACK pStopTrackCallback );
extern void		audio_Update(void);
extern BOOL		audio_Shutdown(void);
extern BOOL		audio_Disabled( void );

extern BOOL		audio_LoadTrackFromFile( char szFileName[] );
extern unsigned int audio_SetTrackVals(const char* fileName, BOOL loop, unsigned int volume, unsigned int audibleRadius);

extern BOOL		audio_PlayStaticTrack( SDWORD iX, SDWORD iY, int iTrack );
extern BOOL		audio_PlayObjStaticTrack( void * psObj, int iTrack );
extern BOOL		audio_PlayObjStaticTrackCallback( void * psObj, int iTrack,
									AUDIO_CALLBACK pUserCallback );
extern BOOL		audio_PlayObjDynamicTrack( void * psObj, int iTrack,
											AUDIO_CALLBACK pUserCallback );
extern BOOL		audio_PlayClusterDynamicTrack( void * psClusterObj,
								int iTrack, AUDIO_CALLBACK pUserCallback );
extern void		audio_StopObjTrack( void * psObj, int iTrack );
extern void		audio_PlayTrack( int iTrack );
extern void		audio_PlayCallbackTrack( int iTrack,
											AUDIO_CALLBACK pUserCallback );
extern AUDIO_STREAM* audio_PlayStream(const char* fileName, float volume, void (*onFinished)(void*), void* user_data);
extern void		audio_QueueTrack( SDWORD iTrack );
extern void		audio_QueueTrackMinDelay( SDWORD iTrack, UDWORD iMinDelay );
extern void		audio_QueueTrackMinDelayPos( SDWORD iTrack, UDWORD iMinDelay,
											SDWORD iX, SDWORD iY, SDWORD iZ);
extern void		audio_QueueTrackGroup( SDWORD iTrack, SDWORD iGroup );
extern void		audio_QueueTrackPos( SDWORD iTrack, SDWORD iX, SDWORD iY,
										SDWORD iZ );
extern void		audio_QueueTrackGroupPos( SDWORD iTrack, SDWORD iGroup,
										SDWORD iX, SDWORD iY, SDWORD iZ );
extern BOOL		audio_GetPreviousQueueTrackPos( SDWORD *iX, SDWORD *iY,
											SDWORD *iZ );
extern void		audio_PauseAll( void );
extern void		audio_ResumeAll( void );
extern void		audio_StopAll( void );

extern SDWORD	audio_GetTrackID( const char *fileName );
extern void audio_RemoveObj(const void* psObj);
extern unsigned int audio_GetSampleQueueCount(void);
extern unsigned int audio_GetSampleListCount(void);
extern unsigned int sound_GetActiveSamplesCount(void);

#if defined(__cplusplus)
}
#endif

extern void 		audioTest(void);

#endif // __INCLUDED_LIB_SOUND_AUDIO_H__
