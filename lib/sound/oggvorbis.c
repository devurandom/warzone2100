/*
	This file is part of Warzone 2100.
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

#include "lib/framework/frame.h"
#include <physfs.h>

#ifndef WZ_NOOGG
#include <vorbis/vorbisfile.h>
#endif

#ifdef __BIG_ENDIAN__
#define OGG_ENDIAN 1
#else
#define OGG_ENDIAN 0
#endif

#include "oggvorbis.h"

struct OggVorbisDecoderState
{
	// Internal identifier towards PhysicsFS
	PHYSFS_file* fileHandle;

	// Wether to allow seeking or not
	BOOL         allowSeeking;

	// Internal identifier towards libVorbisFile
	OggVorbis_File oggVorbis_stream;

	// Internal meta data
	vorbis_info* VorbisInfo;
};

static size_t wz_oggVorbis_read(void *ptr, size_t size, size_t nmemb, void *datasource)
{
	PHYSFS_file* fileHandle = ((struct OggVorbisDecoderState*)datasource)->fileHandle;
	return PHYSFS_read(fileHandle, ptr, 1, size*nmemb);
}

static int wz_oggVorbis_seek(void *datasource, ogg_int64_t offset, int whence)
{
	PHYSFS_file* fileHandle = ((struct OggVorbisDecoderState*)datasource)->fileHandle;
	BOOL allowSeeking = ((struct OggVorbisDecoderState*)datasource)->allowSeeking;

	int newPos;

	if (!allowSeeking)
		return -1;

	switch (whence)
	{
		// Seek to absolute position
		case SEEK_SET:
			newPos = offset;
			break;

		// Seek `offset` ahead
		case SEEK_CUR:
		{
			int curPos = PHYSFS_tell(fileHandle);
			if (curPos == -1)
				return -1;

			newPos = curPos + offset;
			break;
		}

		// Seek backwards from the end of the file
		case SEEK_END:
		{
			int fileSize = PHYSFS_fileLength(fileHandle);
			if (fileSize == -1)
				return -1;

			newPos = fileSize - 1 - offset;
			break;
		}

		// unrecognized seek instruction
		default:
			// indicate failure
			return -1;
	}

	// PHYSFS_seek return value of non-zero means success
	if (PHYSFS_seek(fileHandle, newPos) != 0)
		return newPos;   // success
	else
		return -1;  // failure
}

static int wz_oggVorbis_close(void *datasource)
{
	return 0;
}

static long wz_oggVorbis_tell(void *datasource)
{
	PHYSFS_file* fileHandle = ((struct OggVorbisDecoderState*)datasource)->fileHandle;
	return PHYSFS_tell(fileHandle);
}

static const ov_callbacks wz_oggVorbis_callbacks =
{
	wz_oggVorbis_read,
	wz_oggVorbis_seek,
	wz_oggVorbis_close,
	wz_oggVorbis_tell
};

struct OggVorbisDecoderState* sound_CreateOggVorbisDecoder(PHYSFS_file* PHYSFS_fileHandle, BOOL allowSeeking)
{
	int error;

	struct OggVorbisDecoderState* decoder = malloc(sizeof(struct OggVorbisDecoderState));
	if (decoder == NULL)
	{
		debug(LOG_ERROR, "sound_CreateOggVorbisDecoder: couldn't allocate memory\n");
		return NULL;
	}

	decoder->fileHandle = PHYSFS_fileHandle;
	decoder->allowSeeking = allowSeeking;

	error = ov_open_callbacks(decoder, &decoder->oggVorbis_stream, NULL, 0, wz_oggVorbis_callbacks);
	if (error < 0)
	{
		debug(LOG_ERROR, "sound_CreateOggVorbisDecoder: ov_open_callbacks failed with errorcode %d\n", error);
		free(decoder);
		return NULL;
	}

	// Aquire some info about the sound data
	decoder->VorbisInfo = ov_info(&decoder->oggVorbis_stream, -1);

	return decoder;
}

void sound_DestroyOggVorbisDecoder(struct OggVorbisDecoderState* decoder)
{
	// Close the OggVorbis decoding stream
	ov_clear(&decoder->oggVorbis_stream);

	free(decoder);
}

static inline unsigned int getSampleCount(struct OggVorbisDecoderState* decoder)
{
	int numSamples = ov_pcm_total(&decoder->oggVorbis_stream, -1);

	if (numSamples == OV_EINVAL)
		return 0;

	return numSamples;
}

static inline unsigned int getCurrentSample(struct OggVorbisDecoderState* decoder)
{
	int samplePos = ov_pcm_tell(&decoder->oggVorbis_stream);

	if (samplePos == OV_EINVAL)
		return 0;

	return samplePos;
}

soundDataBuffer* sound_DecodeOggVorbis(struct OggVorbisDecoderState* decoder, size_t bufferSize)
{
	size_t		size = 0;
	int		result;

	soundDataBuffer* buffer;

	if (decoder->allowSeeking)
	{
		unsigned int sampleCount = getSampleCount(decoder);

		unsigned int sizeEstimate = sampleCount * decoder->VorbisInfo->channels * 2;

		if (((bufferSize == 0) || (bufferSize > sizeEstimate)) && (sizeEstimate != 0))
		{
			bufferSize = (sampleCount - getCurrentSample(decoder)) * decoder->VorbisInfo->channels * 2;
		}
	}

	// If we can't seek nor receive any suggested size for our buffer, just quit
	if (bufferSize == 0)
	{
		debug(LOG_ERROR, "sound_DecodeOggVorbis: can't find a proper buffer size\n");
		return NULL;
	}

	buffer = malloc(bufferSize + sizeof(soundDataBuffer));
	if (buffer == NULL)
	{
		debug(LOG_ERROR, "sound_DecodeOggVorbis: couldn't allocate memory (%u bytes requested)\n", bufferSize + sizeof(soundDataBuffer));
		return NULL;
	}

#if !(__STDC_VERSION__ >= 199901L)
	buffer->data = (char*)(buffer + 1);
#endif
	buffer->bufferSize = bufferSize + sizeof(soundDataBuffer);
	buffer->bitsPerSample = 16;
	buffer->channelCount = decoder->VorbisInfo->channels;
	buffer->frequency = decoder->VorbisInfo->rate;

	// Decode PCM data into the buffer until there is nothing to decode left
	do
	{
		// Decode
		int section;
		result = ov_read(&decoder->oggVorbis_stream, &buffer->data[size], bufferSize - size, OGG_ENDIAN, 2, 1, &section);

		if (result < 0)
		{
			debug(LOG_ERROR, "sound_DecodeOggVorbis: error decoding from OggVorbis file; errorcode from ov_read: %d\n", result);
			free(buffer);
			return NULL;
		}
		else
		{
			size += result;
		}

	}
	while ((result != 0 && size < bufferSize));

	buffer->size = size;

	return buffer;
}
