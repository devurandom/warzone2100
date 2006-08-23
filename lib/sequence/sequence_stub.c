/***************************************************************************/
/*
 * Sequence.c
 *
 * Sequence setup and video control
 *
 * based on eidos example code
 *
 *
 */
/***************************************************************************/

// Standard include file
#include "lib/framework/frame.h"
#include "rpl_reader.h"
#include "sequence.h"

#ifdef WZ_OPENAL_MAC_H
#include <openal/al.h>
#else
#include <AL/al.h>
#endif

#define DUMMY_VIDEO

RPL* current_sequence = NULL;
unsigned int current_frame = 0;

#define SOUND_BUFFER_SIZE 1024*1024
short sound_buffer[SOUND_BUFFER_SIZE];
BOOL seq_sound = FALSE;

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

ALuint 		seq_buffer;
ALuint		seq_source;

void seq_start_sound(RPL* s) {
	unsigned int buffer_size = rpl_decode_sound(s, sound_buffer, SOUND_BUFFER_SIZE);

	if (buffer_size != 0) {
		seq_sound = TRUE;
		alGenBuffers(1, &seq_buffer);
		alBufferData(seq_buffer, AL_FORMAT_MONO16, sound_buffer, buffer_size*sizeof(short), 22050);

		alGenSources(1, &seq_source);

		alSourcef (seq_source, AL_GAIN,  	   1            );
		alSource3f(seq_source, AL_POSITION,        0.0, 0.0, 0.0);
		alSource3f(seq_source, AL_VELOCITY,        0.0, 0.0, 0.0);
		alSourcef (seq_source, AL_ROLLOFF_FACTOR,  0.0          );
		alSourcei (seq_source, AL_SOURCE_RELATIVE, AL_TRUE      );

		alSourceQueueBuffers(seq_source, 1, &seq_buffer);
		alSourcePlay(seq_source);
	}
}


BOOL seq_SetSequenceForBuffer(char* filename, int startTime, PERF_MODE perfMode)
{
#ifdef DUMMY_VIDEO
	debug( LOG_VIDEO, "seq_SetSequenceForBuffer %s -> novideo.rpl\n", filename );
	filename = "novideo.rpl";
#endif
	if (current_sequence != NULL) {
		rpl_close(current_sequence);
	}
	current_sequence = rpl_open(filename);
	if (current_sequence == NULL) {
		return FALSE;
	}
	current_frame = 0;
	seq_start_sound(current_sequence);
	return TRUE;
}

/*
 * directX fullscreeen render uses local buffer to store previous frame data
 * directX 640 * 480 16bit rgb mode render through local buffer to back buffer
 */
BOOL seq_SetSequence(char* filename, int startTime, char* lpBF, PERF_MODE perfMode)
{
#ifdef DUMMY_VIDEO
	debug( LOG_VIDEO, "seq_SetSequence %s -> novideo.rpl\n", filename );
	filename = "novideo.rpl";
#endif
	if (current_sequence != NULL) {
		rpl_close(current_sequence);
	}
	current_sequence = rpl_open(filename);
	if (current_sequence == NULL) {
		return FALSE;
	}
	current_frame = 0;
	seq_start_sound(current_sequence);
	return TRUE;

}

int seq_ClearMovie(void)
{
	debug( LOG_VIDEO, "seq_ClearMovie\n" );
	if (current_sequence != NULL) {
		rpl_close(current_sequence);
		current_sequence = NULL;
	}
	return TRUE;
}


int	seq_RenderOneFrameToBuffer(char *lpSF, int skip, SDWORD subMin, SDWORD subMax)
{
	//printf("seq_RenderOneFrameToBuffer %i\n", skip);
	if (current_sequence == NULL) {
		return VIDEO_FRAME_ERROR;
	} else {
		rpl_decode_next_image(current_sequence, lpSF);
		if (++current_frame >= current_sequence->nb_chunks)
		{
			return VIDEO_FINISHED;
		}
	}
	return current_frame;
}


/*
 * render one frame to a direct draw surface (normally the back buffer)
 * directX 640 * 480 16bit rgb mode render through local buffer to back buffer
 */
int	seq_RenderOneFrame(int skip, SDWORD subMin, SDWORD subMax)
{
	//printf("seq_RenderOneFrame %i\n", skip);
	if (current_sequence == NULL)
	{
		return VIDEO_FRAME_ERROR;
	}
#ifdef DUMMY_VIDEO
	  if (++current_frame >= 1000)
	    return VIDEO_FINISHED;
#else
	else if (++current_frame >= current_sequence->nb_chunks)
	{
//		seq_AddTextForVideo("<<hit ESC to continue>>", 0, 0, 399, 299);
//		return VIDEO_FINISHED;	//For now, user must hit ESC to continue during mission briefings!
		// temporary "fix".
	}
#endif
	return current_frame;
}

BOOL	seq_RefreshVideoBuffers(void)
{
	//printf("seq_RefreshVideoBuffers\n");
	return TRUE;
}

BOOL	seq_ShutDown(void)
{
	debug( LOG_VIDEO, "seq_ShutDown\n" );
	if (current_sequence != NULL) {
		if (seq_sound == TRUE) {
			alSourceStop(seq_source);
			alDeleteSources(1, &seq_source);
			alDeleteBuffers(1, &seq_buffer);
		}
		rpl_close(current_sequence);
		current_sequence = NULL;
	}
	return TRUE;
}

BOOL	seq_GetFrameSize(SDWORD *pWidth, SDWORD* pHeight)
{
	//printf("seq_GetFrameSize\n");
	if (current_sequence != NULL) {
		*pWidth = current_sequence->width;
		*pHeight = current_sequence->height;
		return TRUE;
	} else {
		*pWidth = 0;
		*pHeight = 0;
		return FALSE;
	}
}

int seq_GetCurrentFrame(void)
{
	//printf("seq_GetCurrentFrame\n");
	if (current_sequence != NULL) {
		return current_frame;
	} else {
		return -1;
	}
}

int seq_GetFrameTimeInClicks(void)
{
	//printf("seq_GetFrameTimeInClicks\n");
	if (current_sequence != NULL) {
		return (1000.0 / current_sequence->fps);
	} else {
		return 0;
	}
}

int seq_GetTotalFrames(void)
{
	//printf("seq_GetTotalFrames\n");
	if (current_sequence != NULL) {
		return current_sequence->nb_chunks;
	} else {
		return -1;
	}
}

