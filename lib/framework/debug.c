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
/*
 * Debug.c
 *
 * Various debugging output functions.
 *
 */

#include "frame.h"

#include <string.h>
#include <stdio.h>

#include "frameint.h"
#include "strnlen1.h"

#define MAX_LEN_LOG_LINE 512

char last_called_script_event[MAX_EVENT_NAME_LEN];
UDWORD traceID;

static debug_callback * callbackRegistry = NULL;
BOOL enabled_debug[LOG_LAST]; // global

/*
 * This list _must_ match the enum in debug.h!
 * Names must be 8 chars long at max!
 */
static const char *code_part_names[] = {
	"all",
	"main",
	"sound",
	"video",
	"wz",
	"3d",
	"texture",
	"net",
	"memory",
	"warning",
	"error",
	"never",
	"script",
	"movement",
	"attack",
	"fog",
	"sensor",
	"gui",
	"map",
	"save",
	"sync",
	"death",
	"gateway",
	"message",
	"last"
};

static char inputBuffer[2][MAX_LEN_LOG_LINE];
static BOOL useInputBuffer1 = false;

/**
 * Convert code_part names to enum. Case insensitive.
 *
 * \return	Codepart number or LOG_LAST if can't match.
 */
static code_part code_part_from_str(const char *str)
{
	unsigned int i;

	for (i = 0; i < LOG_LAST; i++) {
		if (strcasecmp(code_part_names[i], str) == 0) {
			return i;
		}
	}
	return LOG_LAST;
}


/**
 * Callback for outputing to stderr
 *
 * \param	data			Ignored. Use NULL.
 * \param	outputBuffer	Buffer containing the preprocessed text to output.
 */
void debug_callback_stderr( WZ_DECL_UNUSED void ** data, const char * outputBuffer )
{
	if ( !strchr( outputBuffer, '\n' ) ) {
		fprintf( stderr, "%s\n", outputBuffer );
	} else {
		fprintf( stderr, "%s", outputBuffer );
	}
}


/**
 * Callback for outputting to a win32 debugger
 *
 * \param	data			Ignored. Use NULL.
 * \param	outputBuffer	Buffer containing the preprocessed text to output.
 */
#if defined WIN32 && defined DEBUG
void debug_callback_win32debug( void ** data, const char * outputBuffer )
{
	char tmpStr[MAX_LEN_LOG_LINE];

	strlcpy(tmpStr, outputBuffer, sizeof(tmpStr));
	if (!strchr(tmpStr, '\n'))
	{
		strlcat(tmpStr, "\n", sizeof(tmpStr));
	}

	OutputDebugStringA( tmpStr );
}
#endif // WIN32


/**
 * Callback for outputing to a file
 *
 * \param	data			Filehandle to output to.
 * \param	outputBuffer	Buffer containing the preprocessed text to output.
 */
void debug_callback_file( void ** data, const char * outputBuffer )
{
	FILE * logfile = (FILE*)*data;

	if ( !strchr( outputBuffer, '\n' ) ) {
		fprintf( logfile, "%s\n", outputBuffer );
	} else {
		fprintf( logfile, "%s", outputBuffer );
	}
}


/**
 * Setup the file callback
 *
 * Sets data to the filehandle opened for the filename found in data.
 *
 * \param[in,out]	data	In: 	The filename to output to.
 * 							Out:	The filehandle.
 */
void debug_callback_file_init( void ** data )
{
	const char * filename = (const char *)*data;
	FILE * logfile = NULL;

	logfile = fopen( filename, "w" );
	if (!logfile) {
		fprintf( stderr, "Could not open %s for appending!\n", filename );
	} else {
		setbuf( logfile, NULL );
		fprintf( logfile, "\n--- Starting log ---\n" );
		*data = logfile;
	}
}


/**
 * Shutdown the file callback
 *
 * Closes the logfile.
 *
 * \param	data	The filehandle to close.
 */
void debug_callback_file_exit( void ** data )
{
	FILE * logfile = (FILE*)*data;
	fclose( logfile );
	*data = NULL;
}


void debug_init(void)
{
	/*** Initialize the debug subsystem ***/
#if defined(WZ_CC_MSVC) && defined(DEBUG)
	int tmpDbgFlag;
	_CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_DEBUG ); // Output CRT info to debugger

	tmpDbgFlag = _CrtSetDbgFlag( _CRTDBG_REPORT_FLAG ); // Grab current flags
# if defined(DEBUG_MEMORY)
	tmpDbgFlag |= _CRTDBG_CHECK_ALWAYS_DF; // Check every (de)allocation
# endif // DEBUG_MEMORY
	tmpDbgFlag |= _CRTDBG_ALLOC_MEM_DF; // Check allocations
	tmpDbgFlag |= _CRTDBG_LEAK_CHECK_DF; // Check for memleaks
	_CrtSetDbgFlag( tmpDbgFlag );
#endif // WZ_CC_MSVC && DEBUG

	STATIC_ASSERT(ARRAY_SIZE(code_part_names) - 1 == LOG_LAST); // enums start at 0

	memset( enabled_debug, false, sizeof(enabled_debug) );
	enabled_debug[LOG_ERROR] = true;
	inputBuffer[0][0] = '\0';
	inputBuffer[1][0] = '\0';
#ifdef DEBUG
	enabled_debug[LOG_WARNING] = true;
#endif
}


void debug_exit(void)
{
	debug_callback * curCallback = callbackRegistry, * tmpCallback = NULL;

	while ( curCallback )
	{
		if ( curCallback->exit )
			curCallback->exit( &curCallback->data );
		tmpCallback = curCallback->next;
		free( curCallback );
		curCallback = tmpCallback;
	}

	callbackRegistry = NULL;
}


void debug_register_callback( debug_callback_fn callback, debug_callback_init init, debug_callback_exit exit, void * data )
{
	debug_callback * curCallback = callbackRegistry, * tmpCallback = NULL;

	tmpCallback = (debug_callback*)malloc(sizeof(debug_callback));

	tmpCallback->next = NULL;
	tmpCallback->callback = callback;
	tmpCallback->init = init;
	tmpCallback->exit = exit;
	tmpCallback->data = data;

	if ( tmpCallback->init )
		tmpCallback->init( &tmpCallback->data );

	if ( !curCallback )
	{
		callbackRegistry = tmpCallback;
		return;
	}

	while ( curCallback->next )
		curCallback = curCallback->next;

	curCallback->next = tmpCallback;
}


BOOL debug_enable_switch(const char *str)
{
	code_part part = code_part_from_str(str);

	if (part != LOG_LAST) {
		enabled_debug[part] = !enabled_debug[part];
	}
	if (part == LOG_ALL) {
		memset(enabled_debug, true, sizeof(enabled_debug));
	}
	return (part != LOG_LAST);
}

/* Dump last two debug log calls into file descriptor. For exception handler. */
#if defined(WZ_OS_WIN)
static inline void dumpstr(HANDLE file, const char* str)
{
	DWORD lNumberOfBytesWritten;
	WriteFile(file, str, strnlen1(str, MAX_LEN_LOG_LINE) - 1, &lNumberOfBytesWritten, NULL);
}
static inline void dumpEOL(HANDLE file)
{
	DWORD lNumberOfBytesWritten;
	WriteFile(file, "\r\n", strlen("\r\n"), &lNumberOfBytesWritten, NULL);
}
#else
static inline void dumpstr(int file, const char* str)
{
	write(file, str, strnlen1(str, MAX_LEN_LOG_LINE) - 1);
}
static inline void dumpEOL(int file)
{
	write(file, "\n", strlen("\n"));
}
#endif

#if defined(WZ_OS_WIN)
void dumpLog(HANDLE file)
#else
void dumpLog(int file)
#endif
{
	dumpstr(file, "Log message 1: ");
	dumpstr(file, inputBuffer[0]);
	dumpEOL(file);
	dumpstr(file, "Log message 2: ");
	dumpstr(file, inputBuffer[1]);
	dumpEOL(file);
	dumpEOL(file);
}

void _debug( code_part part, const char *function, const char *str, ... )
{
	va_list ap;
	static char outputBuffer[MAX_LEN_LOG_LINE];

	debug_callback * curCallback = callbackRegistry;

	static unsigned int repeated = 0; /* times current message repeated */
	static unsigned int next = 2;     /* next total to print update */
	static unsigned int prev = 0;     /* total on last update */

	va_start(ap, str);
	vsnprintf(outputBuffer, MAX_LEN_LOG_LINE, str, ap);
	va_end(ap);

	snprintf(inputBuffer[useInputBuffer1 ? 1 : 0], MAX_LEN_LOG_LINE, "[%s] %s", function, outputBuffer);

	if ( strncmp( inputBuffer[0], inputBuffer[1], MAX_LEN_LOG_LINE - 1 ) == 0 ) {
		// Received again the same line
		repeated++;
		if (repeated == next) {
			if (repeated > 2) {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %u times (total %u repeats)", repeated - prev, repeated );
			} else {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %u times", repeated - prev );
			}
			while (curCallback) {
				curCallback->callback( &curCallback->data, outputBuffer );
				curCallback = curCallback->next;
			}
			curCallback = callbackRegistry;
			prev = repeated;
			next *= 2;
		}
	} else {
		// Received another line, cleanup the old
		if (repeated > 0 && repeated != prev && repeated != 1) {
			/* just repeat the previous message when only one repeat occurred */
			if (repeated > 2) {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %u times (total %u repeats)", repeated - prev, repeated );
			} else {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %u times", repeated - prev );
			}
			while (curCallback)
			{
				curCallback->callback( &curCallback->data, outputBuffer );
				curCallback = curCallback->next;
			}
			curCallback = callbackRegistry;
		}
		repeated = 0;
		next = 2;
		prev = 0;
	}

	if (!repeated)
	{
		// Assemble the outputBuffer:
		snprintf( outputBuffer, MAX_LEN_LOG_LINE, "%-8s: %s", code_part_names[part], useInputBuffer1 ? inputBuffer[1] : inputBuffer[0] );

		while (curCallback) {
			curCallback->callback( &curCallback->data, outputBuffer );
			curCallback = curCallback->next;
		}
	}
	useInputBuffer1 = !useInputBuffer1; // Swap buffers
}

bool debugPartEnabled(code_part codePart)
{
	return enabled_debug[codePart];
}
