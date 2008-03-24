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

#include "lib/framework/frame.h"

#include "png_util.h"
#include <png.h>
#include <physfs.h>

#define PNG_BYTES_TO_CHECK 8
static const unsigned int channelBitdepth = 8;
static const unsigned int channelsPerPixel = 3;

// PNG callbacks
static void wzpng_read_data(png_structp ctx, png_bytep area, png_size_t size)
{
	PHYSFS_file* fileHandle = (PHYSFS_file*)png_get_io_ptr(ctx);

	PHYSFS_read(fileHandle, area, 1, size);
}

static void wzpng_write_data(png_structp png_ptr, png_bytep data, png_size_t length)
{
	PHYSFS_file* fileHandle = (PHYSFS_file*)png_get_io_ptr(png_ptr);

	PHYSFS_write(fileHandle, data, length, 1);
}

static void wzpng_flush_data(png_structp png_ptr)
{
	PHYSFS_file* fileHandle = (PHYSFS_file*)png_get_io_ptr(png_ptr);

	PHYSFS_flush(fileHandle);
}
// End of PNG callbacks

static inline void PNGReadCleanup(png_infop *info_ptr, png_structp *png_ptr, PHYSFS_file* fileHandle)
{
	if (*info_ptr != NULL)
		png_destroy_info_struct(*png_ptr, info_ptr);
	if (*png_ptr != NULL)
		png_destroy_read_struct(png_ptr, NULL, NULL);
	if (fileHandle != NULL)
		PHYSFS_close(fileHandle);
}

static inline void PNGWriteCleanup(png_infop *info_ptr, png_structp *png_ptr, PHYSFS_file* fileHandle)
{
	if (*info_ptr != NULL)
		png_destroy_info_struct(*png_ptr, info_ptr);
	if (*png_ptr != NULL)
		png_destroy_write_struct(png_ptr, NULL);
	if (fileHandle != NULL)
		PHYSFS_close(fileHandle);
}

BOOL iV_loadImage_PNG(const char *fileName, iV_Image *image)
{
	unsigned char PNGheader[PNG_BYTES_TO_CHECK];
	PHYSFS_sint64 readSize;

	png_structp png_ptr = NULL;
	png_infop info_ptr = NULL;

	// Open file
	PHYSFS_file* fileHandle = PHYSFS_openRead(fileName);
	if (fileHandle == NULL)
	{
		debug(LOG_ERROR, "pie_PNGLoadFile: PHYSFS_openRead(%s) failed with error: %s\n", fileName, PHYSFS_getLastError());
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	// Read PNG header from file
	readSize = PHYSFS_read(fileHandle, PNGheader, 1, PNG_BYTES_TO_CHECK);
	if (readSize < PNG_BYTES_TO_CHECK)
	{
		debug(LOG_ERROR, "pie_PNGLoadFile: PHYSFS_read(%s) failed with error: %s\n", fileName, PHYSFS_getLastError());
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	// Verify the PNG header to be correct
	if (png_sig_cmp(PNGheader, 0, PNG_BYTES_TO_CHECK))
	{
		debug(LOG_3D, "pie_PNGLoadMem: Did not recognize PNG header in %s", fileName);
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL) {
		debug(LOG_3D, "pie_PNGLoadMem: Unable to create png struct");
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL) {
		debug(LOG_3D, "pie_PNGLoadMem: Unable to create png info struct");
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	// Set libpng's failure jump position to the if branch,
	// setjmp evaluates to false so the else branch will be executed at first
	if (setjmp(png_jmpbuf(png_ptr))) {
		debug(LOG_3D, "pie_PNGLoadMem: Error decoding PNG data in %s", fileName);
		PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
		return false;
	}

	// Tell libpng how many byte we already read
	png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK);

	/* Set up the input control */
	png_set_read_fn(png_ptr, fileHandle, wzpng_read_data);

	// Most of the following transformations are seemingly not needed
	// Filler is, however, for an unknown reason required for tertilesc[23]

	/* tell libpng to strip 16 bit/color files down to 8 bits/color */
 	png_set_strip_16(png_ptr);

	/* Extract multiple pixels with bit depths of 1, 2, and 4 from a single
	 * byte into separate bytes (useful for paletted and grayscale images).
	 */
// 	png_set_packing(png_ptr);

	/* More transformations to ensure we end up with 32bpp, 4 channel RGBA */
// 	png_set_gray_to_rgb(png_ptr);
 	png_set_palette_to_rgb(png_ptr);
 	png_set_tRNS_to_alpha(png_ptr);
	png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
// 	png_set_gray_1_2_4_to_8(png_ptr);

	png_read_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

	image->width = info_ptr->width;
	image->height = info_ptr->height;
	image->depth = info_ptr->channels;
	image->bmp = malloc(info_ptr->height * info_ptr->rowbytes);

	{
		unsigned int i = 0;
		png_bytepp row_pointers = png_get_rows(png_ptr, info_ptr);
		for ( i = 0; i < info_ptr->height; i++ )
			memcpy( image->bmp + (info_ptr->rowbytes * i), row_pointers[i], info_ptr->rowbytes );
	}

	PNGReadCleanup(&info_ptr, &png_ptr, fileHandle);
	return true;
}

void iV_saveImage_PNG(const char *fileName, const iV_Image *image)
{
	const unsigned char** scanlines = NULL;
	png_infop info_ptr = NULL;
	png_structp png_ptr = NULL;

	PHYSFS_file* fileHandle = PHYSFS_openWrite(fileName);
	if (fileHandle == NULL)
	{
		debug(LOG_ERROR, "pie_PNGSaveFile: PHYSFS_openWrite failed (while openening file %s) with error: %s\n", fileName, PHYSFS_getLastError());
		return;
	}

	png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
	if (png_ptr == NULL)
	{
		debug(LOG_ERROR, "pie_PNGSaveFile: Unable to create png struct\n");
		return PNGWriteCleanup(&info_ptr, &png_ptr, fileHandle);
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == NULL)
	{
		debug(LOG_ERROR, "pie_PNGSaveFile: Unable to create png info struct\n");
		return PNGWriteCleanup(&info_ptr, &png_ptr, fileHandle);
	}

	// If libpng encounters an error, it will jump into this if-branch
	if (setjmp(png_jmpbuf(png_ptr)))
	{
		debug(LOG_ERROR, "pie_PNGSaveFile: Error encoding PNG data\n");
	}
	else
	{
		unsigned int currentRow;
		unsigned int row_stride = image->width * channelsPerPixel;

		scanlines = malloc(sizeof(const char*) * image->height);
		if (scanlines == NULL)
		{
			debug(LOG_ERROR, "pie_PNGSaveFile: Couldn't allocate memory\n");
			return PNGWriteCleanup(&info_ptr, &png_ptr, fileHandle);
		}

		png_set_write_fn(png_ptr, fileHandle, wzpng_write_data, wzpng_flush_data);

		// Set the compression level of ZLIB
		// Right now we stick with the default, since that one is the
		// fastest which still produces acceptable filesizes.
		// The highest compression level hardly produces smaller files than default.
		//
		// Below are some benchmarks done while taking screenshots at 1280x1024
		// Z_NO_COMPRESSION:
		// black (except for GUI): 398 msec
		// 381, 391, 404, 360 msec
		//
		// Z_BEST_SPEED:
		// black (except for GUI): 325 msec
		// 611, 406, 461, 608 msec
		//
		// Z_DEFAULT_COMPRESSION:
		// black (except for GUI): 374 msec
		// 1154, 1121, 627, 790 msec
		//
		// Z_BEST_COMPRESSION:
		// black (except for GUI): 439 msec
		// 1600, 1078, 1613, 1700 msec

		// Not calling this function is equal to using the default
		// so to spare some CPU cycles we comment this out.
		// png_set_compression_level(png_ptr, Z_DEFAULT_COMPRESSION);

		png_set_IHDR(png_ptr, info_ptr, image->width, image->height, channelBitdepth,
			             PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

		// Create an array of scanlines
		for (currentRow = 0; currentRow < image->height; ++currentRow)
		{
			// We're filling the scanline from the bottom up here,
			// otherwise we'd have a vertically mirrored image.
			scanlines[currentRow] = &image->bmp[row_stride * (image->height - currentRow - 1)];
		}

		png_set_rows(png_ptr, info_ptr, (const png_bytepp)scanlines);

		png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);
	}

	free(scanlines);
	return PNGWriteCleanup(&info_ptr, &png_ptr, fileHandle);
}
