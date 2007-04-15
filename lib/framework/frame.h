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
/*! \file frame.h
 * \brief The framework library initialisation and shutdown routines.
 */
#ifndef _frame_h
#define _frame_h

#include "platform.h"

/* This one must be invoked *after* platform.h to get _GNU_SOURCE! */
#include <string.h>
#include <locale.h>
#include <stdlib.h>
#include <ctype.h>

#include "gettext.h"
#define _(String) gettext(String)
#define N_(String) gettext_noop(String)

// Context sensitive strings
#define P_(Context, String) pgettext(Context, String)
// Non literal context sensitive strings
#define PE_(Context, String) pgettext_expr(Context, String)
// Make xgettext recognize the context
#define NP_(Context, String) gettext_noop(String)


#include "macros.h"
#include "types.h"
#include "debug.h"

#include "heap.h"
#include "treap.h"

#include "fractions.h"
#include "trig.h"


/* Initialise the frame work library */
extern BOOL frameInitialise(
					const char *pWindowName,// The text to appear in the window title bar
					UDWORD width,			// The display width
					UDWORD height,			// The display height
					UDWORD bitDepth,		// The display bit depth
					BOOL fullScreen		// Whether to start full screen or windowed
					);

/* Shut down the framework library.
 * This clears up all the Direct Draw stuff and ensures
 * that Windows gets restored properly after Full screen mode.
 */
extern void frameShutDown(void);

/* The current status of the framework */
typedef enum _frame_status
{
	FRAME_OK,			// Everything normal
	FRAME_KILLFOCUS,	// The main app window has lost focus (might well want to pause)
	FRAME_SETFOCUS,		// The main app window has focus back
	FRAME_QUIT,			// The main app window has been told to quit
} FRAME_STATUS;

/* Call this each cycle to allow the framework to deal with
 * windows messages, and do general house keeping.
 *
 * Returns FRAME_STATUS.
 */
extern FRAME_STATUS frameUpdate(void);

/* Set the current cursor from a Resource ID
 * This is the same as calling:
 *       frameSetCursor(LoadCursor(MAKEINTRESOURCE(resID)));
 * but with a bit of extra error checking.
 */
extern void frameSetCursorFromRes(SWORD resID);

/* Returns the current frame we're on - used to establish whats on screen */
extern UDWORD	frameGetFrameNumber(void);

/**
 * Average framerate of the last seconds
 *
 * \return Average framerate
 */
extern UDWORD frameGetAverageRate(void);


/**
 * Set the framerate limit
 *
 * \param fpsLimit Desired framerate
 */
extern void setFramerateLimit(Uint32 fpsLimit);

/**
 * Get the framerate limit
 *
 * \return Desired framerate
 */
extern Uint32 getFramerateLimit(void);

/* Load the file with name pointed to by pFileName into a memory buffer. */
extern BOOL loadFile(const char *pFileName,		// The filename
              char **ppFileData,	// A buffer containing the file contents
              UDWORD *pFileSize);	// The size of this buffer

/* Save the data in the buffer into the given file */
extern BOOL saveFile(const char *pFileName, const char *pFileData, UDWORD fileSize);

// load a file from disk into a fixed memory buffer
BOOL loadFileToBuffer(const char *pFileName, char *pFileBuffer, UDWORD bufferSize, UDWORD *pSize);

// as above but returns quietly if no file found
BOOL loadFileToBufferNoError(const char *pFileName, char *pFileBuffer, UDWORD bufferSize,
                             UDWORD *pSize);

extern SDWORD ftol(float f);

UDWORD HashString( const char *String );
UDWORD HashStringIgnoreCase( const char *String );


/* Endianness hacks */

#ifdef __BIG_ENDIAN__
static inline void endian_uword(UWORD *uword) {
  UBYTE tmp, *ptr;

  ptr = (UBYTE *) uword;
  tmp = ptr[0];
  ptr[0] = ptr[1];
  ptr[1] = tmp;
}
#else
# define endian_uword(x)
#endif

#ifdef __BIG_ENDIAN__
static inline void endian_sword(SWORD *sword) {
  UBYTE tmp, *ptr;

  ptr = (UBYTE *) sword;
  tmp = ptr[0];
  ptr[0] = ptr[1];
  ptr[1] = tmp;
}
#else
# define endian_sword(x)
#endif

#ifdef __BIG_ENDIAN__
static inline void endian_udword(UDWORD *udword) {
  UBYTE tmp, *ptr;

  ptr = (UBYTE *) udword;
  tmp = ptr[0];
  ptr[0] = ptr[3];
  ptr[3] = tmp;
  tmp = ptr[1];
  ptr[1] = ptr[2];
  ptr[2] = tmp;
}
#else
# define endian_udword(x)
#endif

#ifdef __BIG_ENDIAN__
static inline void endian_sdword(SDWORD *sdword) {
  UBYTE tmp, *ptr;

  ptr = (UBYTE *) sdword;
  tmp = ptr[0];
  ptr[0] = ptr[3];
  ptr[3] = tmp;
  tmp = ptr[1];
  ptr[1] = ptr[2];
  ptr[2] = tmp;
}
#else
# define endian_sdword(x)
#endif

#ifdef __BIG_ENDIAN__
static inline void endian_fract(FRACT *fract) {
  UBYTE tmp, *ptr;

  ptr = (UBYTE *) fract;
  tmp = ptr[0];
  ptr[0] = ptr[3];
  ptr[3] = tmp;
  tmp = ptr[1];
  ptr[1] = ptr[2];
  ptr[2] = ptr[1];
}
#else
# define endian_fract(x)
#endif

void setupExceptionHandler(const char * programCommand);

#endif
