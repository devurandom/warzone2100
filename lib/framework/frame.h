/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
	Copyright (C) 2005-2009  Warzone Resurrection Project

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

#include "wzglobal.h"

// Workaround X11 headers #defining Status
#ifdef Status
# undef Status
#endif

#include "types.h"
/**
* NOTE: the next two #include lines are needed by MSVC to override the default,
* non C99 compliant routines, and redefinition; different linkage errors
*/
#include "stdio_ext.h"
#include "string_ext.h"

#include "macros.h"
#include "debug.h"

#include "i18n.h"
#include "trig.h"
#include "cursors.h"

extern UDWORD selectedPlayer;
#define MAX_PLAYERS	8	/**< Maximum number of players in the game. */

/** Initialise the framework library
 *  @param pWindowName the text to appear in the window title bar
 *  @param width the display widget
 *  @param height the display height
 *  @param bitDepth the display bit depth
 *  @param fullScreen whether to start full screen or windowed
 *  @param vsync if to sync to the vertical blanking interval or not
 *
 *  @return true when the framework library is successfully initialised, false
 *          when a part of the initialisation failed.
 */
extern BOOL frameInitialise(const char* pWindowName, UDWORD width, UDWORD height, UDWORD bitDepth, unsigned int fsaa, BOOL fullScreen, BOOL vsync);

extern bool selfTest;

/** Shut down the framework library.
 * This clears up all the Direct Draw stuff and ensures
 * that Windows gets restored properly after Full screen mode.
 */
extern void frameShutDown(void);

typedef enum _focus_state
{
	FOCUS_OUT,		// Window does not have the focus
	FOCUS_IN,		// Window has got the focus
} FOCUS_STATE;

/*!
 * Set the framerate limit
 *
 * \param fpsLimit Desired framerate
 */
extern void setFramerateLimit(int fpsLimit);

/*!
 * Get the framerate limit
 *
 * \return Desired framerate
 */
extern int getFramerateLimit(void);

/** Call this each cycle to allow the framework to deal with
 * windows messages, and do general house keeping.
 */
extern void frameUpdate(void);

extern void frameSetCursor(CURSOR cur);

/** Returns the current frame we're on - used to establish whats on screen. */
extern UDWORD frameGetFrameNumber(void);

/** Return average framerate of the last seconds. */
extern UDWORD frameGetAverageRate(void);

extern UDWORD HashString( const char *String );
extern UDWORD HashStringIgnoreCase( const char *String );

#if defined(WZ_OS_WIN)
struct timeval
{
	long tv_sec;
	long tv_usec;
};

struct timezone;
extern int gettimeofday(struct timeval* tv, struct timezone* tz);
#endif

static inline WZ_DECL_CONST const char * bool2string(bool var)
{
	return (var ? "true" : "false");
}

#endif
