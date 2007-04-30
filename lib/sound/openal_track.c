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
//*
//
// Sound library-specific functions
//*
//

// this has to be first
#include "lib/framework/frame.h"
#include "lib/framework/frameresource.h"

#ifdef WZ_OS_MAC
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#else
#include <AL/al.h>
#include <AL/alc.h>
#endif

#include <physfs.h>
#include <string.h>

#include "tracklib.h"
#include "audio.h"
#include "oggvorbis.h"

#define ATTENUATION_FACTOR	0.0003f

ALuint current_queue_sample = -1;

typedef struct	SAMPLE_LIST
{
	struct AUDIO_SAMPLE *curr;
	struct SAMPLE_LIST	*next;
} SAMPLE_LIST;

static SAMPLE_LIST *active_samples = NULL;

static ALfloat		sfx_volume = 1.0;
static ALfloat		sfx3d_volume = 1.0;

static ALCdevice* device = 0;
static ALCcontext* context = 0;

static char* DataBuffer = NULL; // Needed for sound_DecodeOggVorbisTrack, must be global, so it can be free'd on shutdown
static size_t DataBuffer_size = 16 * 1024;

BOOL openal_initialized = FALSE;

BOOL		cdAudio_Update( void );

static void PrintOpenALVersion(void)
{
	debug(LOG_ERROR, "OpenAL Vendor: %s\n"
		   "OpenAL Version: %s\n"
		   "OpenAL Renderer: %s\n"
		   "OpenAL Extensions: %s\n",
		   alGetString(AL_VENDOR), alGetString(AL_VERSION),
		   alGetString(AL_RENDERER), alGetString(AL_EXTENSIONS));
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_InitLibrary( void )
{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	int err=0;
	ALfloat listenerVel[3] = { 0.0, 0.0, 0.0 };
	ALfloat listenerOri[6] = { 0.0, 0.0, 1.0, 0.0, 1.0, 0.0 };
//	int contextAttributes[] = { 0 };
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	device = alcOpenDevice(0);
	if(device == 0) {
		PrintOpenALVersion();
		debug(LOG_ERROR, "Couldn't open audio device.");
		return FALSE;
	}

	context = alcCreateContext(device, NULL);		//NULL was contextAttributes
	alcMakeContextCurrent(context);

	err = alcGetError(device);
	if (err != ALC_NO_ERROR) {
		PrintOpenALVersion();
		debug(LOG_ERROR, "Couldn't initialize audio context: %s",
				alcGetString(device, err));
		return FALSE;
	}

	openal_initialized = TRUE;

	// Clear Error Codes
	alGetError();
	alcGetError(device);

	// Check what version of Open AL we are using
	debug(LOG_SOUND, "OpenAL Version : %s",alGetString(AL_VERSION));
	debug(LOG_SOUND, "OpenAL Renderer : %s",alGetString(AL_RENDERER));
	debug(LOG_SOUND, "OpenAL Extensions : %s",alGetString(AL_EXTENSIONS));


	alListener3f( AL_POSITION, 0, 0, 0 );
	alListenerfv( AL_VELOCITY, listenerVel );
	alListenerfv( AL_ORIENTATION, listenerOri );
	alDistanceModel( AL_NONE );
	return TRUE;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_ShutdownLibrary( void )
{
	SAMPLE_LIST * aSample = active_samples, * tmpSample = NULL;

	debug(LOG_SOUND, "sound_ShutdownLibrary: starting shutdown");
	if(context != 0) {
#ifdef WIN32
		/* Ifdef'ed out the two lines below on Linux since this caused some versions
		 * of OpenAL to hang on exit. - Per */
		debug(LOG_SOUND, "sound_ShutdownLibrary: make default context NULL");
		alcMakeContextCurrent(NULL);		//this should work now -Q
#endif
		debug(LOG_SOUND, "sound_ShutdownLibrary: destroy previous context");
		alcDestroyContext(context); // this gives a long delay on some impl.
		context = 0;
	}
	debug(LOG_SOUND, "sound_ShutdownLibrary: close device");
	if(device != 0) {
		alcCloseDevice(device);
		device = 0;
	}

	free(DataBuffer);
	DataBuffer = NULL;

	while( aSample )
	{
		tmpSample = aSample->next;
		free( aSample );
		aSample = tmpSample;
	}
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_Update( void )
{
	int err=0;
//	{			//  <=== whats up with this ??
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		SAMPLE_LIST **tmp = &active_samples;
		SAMPLE_LIST *i;
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		for ( tmp = &active_samples, i = *tmp; i != NULL; i = *tmp )
		{
			//~~~~~~~~~~
			ALenum	state = AL_STOPPED;
			//~~~~~~~~~~

			alGetSourcei( i->curr->iSample, AL_SOURCE_STATE, &state );
			switch ( state )
			{
			case AL_PLAYING:
			case AL_PAUSED:
				//
				// * sound_SetObjectPosition(i->curr->iSample, i->curr->x, i->curr->y, i->curr->z);
				//
				tmp = &( i->next );
				break;

			default:
				sound_FinishedCallback( i->curr );
				if (i->curr->iSample != (ALuint)AL_INVALID) {
					alDeleteSources( 1, &(i->curr->iSample) );
					i->curr->iSample = AL_INVALID;
				}
				*tmp = i->next;
				free( i );
				break;
			}
		}
//	}//  <=== whats up with this You trying to make those local only ??

	cdAudio_Update();
	alcProcessContext(context);
	err = alcGetError(device);
	if(err != ALC_NO_ERROR) {
		debug(LOG_ERROR, "Error while processing audio context: %s",
				alGetString(err));
	}
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_QueueSamplePlaying( void )
{
	if ( current_queue_sample == (ALuint)AL_INVALID )
	{
		return FALSE;
	}
	else
	{
		//~~~~~~~~~~
		ALenum	state;
		//~~~~~~~~~~

		alGetSourcei( current_queue_sample, AL_SOURCE_STATE, &state );
		if ( state == AL_PLAYING )
		{
			return TRUE;
		}
		else
		{
			if (current_queue_sample != (ALuint)AL_INVALID) {
				alDeleteSources( 1, &current_queue_sample );
				current_queue_sample = AL_INVALID;
			}
			current_queue_sample = -1;
			return FALSE;
		}
	}
}

/** Decodes an opened OggVorbis file into an OpenAL buffer
 *  \param psTrack pointer to object which will contain the final buffer
 *  \param PHYSFS_fileHandle file handle given by PhysicsFS to the opened file
 *  \return on success the psTrack pointer, otherwise it will be free'd and a NULL pointer is returned instead
 */
static inline TRACK* sound_DecodeOggVorbisTrack(TRACK *psTrack, PHYSFS_file* PHYSFS_fileHandle)
{
	ALenum		format;
	ALuint		buffer;

	OggVorbisDecoderState* decoder = sound_CreateOggVorbisDecoder(PHYSFS_fileHandle, TRUE);
	soundDataBuffer* soundBuffer;

	// Allocate an initial buffer to contain the decoded PCM data
	if (DataBuffer == NULL)
	{
		DataBuffer = malloc(DataBuffer_size);
	}

	soundBuffer = sound_DecodeOggVorbis(decoder, DataBuffer_size, DataBuffer);
	sound_DestroyOggVorbisDecoder(decoder);

	DataBuffer = (char*)soundBuffer;

	if (soundBuffer == NULL)
	{
		free(psTrack);
		return NULL;
	}

	DataBuffer_size = soundBuffer->bufferSize;

	// Determine PCM data format
	format = (soundBuffer->channelCount == 1) ? AL_FORMAT_MONO16 : AL_FORMAT_STEREO16;

	// Create an OpenAL buffer and fill it with the decoded data
	alGenBuffers(1, &buffer);
	alBufferData(buffer, format, soundBuffer->data, soundBuffer->size, soundBuffer->frequency);

	// save buffer name in track
	psTrack->iBufferName = buffer;

	return psTrack;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
TRACK* sound_LoadTrackFromFile(const char *fileName)
{
	TRACK* pTrack;
	PHYSFS_file* fileHandle;

	// Use PhysicsFS to open the file
	fileHandle = PHYSFS_openRead(fileName);
	if (fileHandle == NULL)
	{
		debug(LOG_ERROR, "sound_LoadTrackFromFile: PHYSFS_openRead(\"%s\") failed with error: %s\n", fileName, PHYSFS_getLastError());
		return NULL;
	}

	// allocate track, plus the memory required to contain the filename
	// one malloc call ensures only one free call is required
	pTrack = (TRACK*)malloc(sizeof(TRACK) + strlen(GetLastResourceFilename()) + 1);
	if (pTrack == NULL)
	{
		debug( LOG_ERROR, "sound_ConstructTrack: couldn't allocate memory\n" );
		abort();
		return NULL;
	}

	// Initialize everyting (except for the filename) to zero
	memset(pTrack, 0, sizeof(TRACK));

	// Set filename pointer and copy the filename into struct
	pTrack->pName = (char*)pTrack + sizeof(TRACK);
	strcpy(pTrack->pName, GetLastResourceFilename());

	// Now use sound_ReadTrackFromBuffer to decode the file's contents
	pTrack = sound_DecodeOggVorbisTrack(pTrack, fileHandle);

	PHYSFS_close(fileHandle);
	return pTrack;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_FreeTrack( TRACK *psTrack )
{
	alDeleteBuffers( 1, &psTrack->iBufferName );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
int sound_GetMaxVolume( void )
{
	return 32767;		// Why this value? -Q
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
static void sound_AddActiveSample( AUDIO_SAMPLE *psSample )
{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	SAMPLE_LIST *tmp = (SAMPLE_LIST *) malloc( sizeof(SAMPLE_LIST) );
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	tmp->curr = psSample;
	tmp->next = active_samples;
	active_samples = tmp;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
static BOOL sound_SetupChannel( AUDIO_SAMPLE *psSample )
{
	sound_AddActiveSample( psSample );

	if (sound_TrackLooped(psSample->iTrack))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_Play2DSample( TRACK *psTrack, AUDIO_SAMPLE *psSample, BOOL bQueued )
{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	ALfloat zero[3] = { 0.0, 0.0, 0.0 };
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	if (sfx_volume == 0.0) {
		return FALSE;
	}

	alGenSources( 1, &(psSample->iSample) );
	alSourcef( psSample->iSample, AL_PITCH, 1.0f );
	alSourcef( psSample->iSample, AL_GAIN, sfx_volume );
	alSourcefv( psSample->iSample, AL_POSITION, zero );
	alSourcefv( psSample->iSample, AL_VELOCITY, zero );
	alSourcei( psSample->iSample, AL_BUFFER, psTrack->iBufferName );
	alSourcei( psSample->iSample, AL_SOURCE_RELATIVE, AL_TRUE );
	alSourcei( psSample->iSample, AL_LOOPING, (sound_SetupChannel(psSample)) ? AL_TRUE : AL_FALSE );
	alSourcePlay( psSample->iSample );
	if ( bQueued )
	{
		current_queue_sample = psSample->iSample;
	}
	else if ( current_queue_sample == psSample->iSample )
	{
		current_queue_sample = -1;
	}

	return TRUE;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_Play3DSample( TRACK *psTrack, AUDIO_SAMPLE *psSample )
{
	ALfloat zero[3] = { 0.0, 0.0, 0.0 };

	if (sfx3d_volume == 0.0) {
		return FALSE;
	}

	alGenSources( 1, &(psSample->iSample) );
	alSourcef( psSample->iSample, AL_PITCH, 1.0 );
	alSourcef( psSample->iSample, AL_GAIN, sfx3d_volume );
	sound_SetObjectPosition( psSample->iSample, psSample->x, psSample->y, psSample->z );
	alSourcefv( psSample->iSample, AL_VELOCITY, zero );
	alSourcei( psSample->iSample, AL_BUFFER, psTrack->iBufferName );
	alSourcei( psSample->iSample, AL_LOOPING, (sound_SetupChannel(psSample)) ? AL_TRUE : AL_FALSE );
	alSourcePlay( psSample->iSample );
	return TRUE;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_PlayStream( AUDIO_SAMPLE *psSample, const char szFileName[], SDWORD iVol )
{
	return TRUE;
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_StopSample( UDWORD iSample )
{
	alSourceStop( iSample );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_SetSamplePan( AUDIO_SAMPLE *psSample, SDWORD iPan )
{
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_SetSampleVolAll( int iVol )
{
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_SetPlayerPos( SDWORD iX, SDWORD iY, SDWORD iZ )
{
	alListener3f( AL_POSITION, iX, iY, iZ );
}

//
// =======================================================================================================================
// =======================================================================================================================
//
/** sets player's sound orientation
 * \param iX pitch in degree (current function implementation ignores this)
 * \param iY roll in degree (current function implementation ignores this)
 * \param iZ yaw in degree
 */
void sound_SetPlayerOrientation( SDWORD iX, SDWORD iY, SDWORD iZ )
{
	//~~~~~~~~~~~
	float	ori[6];
	//~~~~~~~~~~~

	// convert params to rad
	// float pitch = (float)iX * M_PI / 180;
	// float roll = (float)iY * M_PI / 180;
	float yaw = (float)iZ * M_PI / 180;

	ori[0] = -sin( yaw );
	ori[1] = cos( yaw );
	ori[2] = 0;
	ori[3] = 0;
	ori[4] = 0;
	ori[5] = 1;
	alListenerfv( AL_ORIENTATION, ori );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_SetObjectPosition( SDWORD iSample, SDWORD iX, SDWORD iY, SDWORD iZ )
{
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// coordinates
	float	listenerX, listenerY, listenerZ, dX, dY, dZ;

	// calculation results
	float	distance, gain;
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	// compute distance
	alGetListener3f( AL_POSITION, &listenerX, &listenerY, &listenerZ );
	dX = iX - listenerX; // distances on all axis
	dY = iY - listenerY;
	dZ = iZ - listenerZ;
	distance = sqrt(dX * dX + dY * dY + dZ * dZ); // Pythagorean theorem

	// compute gain
	gain = 1 - distance * ATTENUATION_FACTOR;

	if ( gain < 0.0 )
	{
		gain = 0.0;
	}

	alSourcef( iSample, AL_GAIN, gain * sfx3d_volume );

	// the alSource3i variant would be better, if it wouldn't provide linker errors however
	alSource3f( iSample, AL_POSITION, iX, iY, iZ );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_PauseSample( AUDIO_SAMPLE *psSample )
{
	alSourcePause( psSample->iSample );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_ResumeSample( AUDIO_SAMPLE *psSample )
{
	alSourcePlay( psSample->iSample );
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_PauseAll( void )
{
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_ResumeAll( void )
{
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
void sound_StopAll( void )
{
}

//*
// =======================================================================================================================
// =======================================================================================================================
//
BOOL sound_SampleIsFinished( AUDIO_SAMPLE *psSample )
{
	//~~~~~~~~~~
	ALenum	state;
	//~~~~~~~~~~

	alGetSourcei( psSample->iSample, AL_SOURCE_STATE, &state );
	if ( state == AL_PLAYING )
	{
		return FALSE;
	}
	else
	{
		if (psSample->iSample != (ALuint)AL_INVALID)
		{
			alDeleteSources( 1, &(psSample->iSample) );
			psSample->iSample = AL_INVALID;
		}
		return TRUE;
	}
}

//*
// =======================================================================================================================
// =======================================================================================================================
//

SDWORD mixer_GetWavVolume( void )
{
	return (SDWORD)(100*sfx_volume);
}

void mixer_SetWavVolume( SDWORD iVol )
{
	sfx_volume = iVol * 0.01;

	if (sfx_volume < 0.0) {
		sfx_volume = 0.0;
	} else if (sfx_volume > 1.0) {
		sfx_volume = 1.0;
	}
}

SDWORD mixer_Get3dWavVolume( void )
{
	return (SDWORD)(100*sfx3d_volume);
}

void mixer_Set3dWavVolume( SDWORD iVol )
{
	sfx3d_volume = iVol * 0.01;

	if (sfx3d_volume < 0.0) {
		sfx3d_volume = 0.0;
	} else if (sfx3d_volume > 1.0) {
		sfx3d_volume = 1.0;
	}
}
