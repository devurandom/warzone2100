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

#define MAX_LEN_LOG_LINE 512

char last_called_script_event[MAX_EVENT_NAME_LEN];

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
	"last"
};


/**
 * Convert code_part names to enum. Case insensitive.
 *
 * \return	Codepart number or LOG_LAST if can't match.
 */
static int code_part_from_str(const char *str)
{
	int i;

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

	strcpy( tmpStr, outputBuffer );
	if ( !strchr( tmpStr, '\n' ) ) {
		strcat( tmpStr, "\n" );
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
	int count = 0;

	while (strcmp(code_part_names[count], "last") != 0) {
		count++;
	}
	if (count != LOG_LAST) {
		fprintf( stderr, "LOG_LAST != last; whoever edited the debug code last "
		        "did a mistake.\n" );
		fprintf( stderr, "Always edit both the enum in debug.h and the string "
		        "list in debug.c!\n" );
		exit(1);
	}
	memset( enabled_debug, FALSE, sizeof(enabled_debug) );
	enabled_debug[LOG_ERROR] = TRUE;
#ifdef DEBUG
	enabled_debug[LOG_WARNING] = TRUE;
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
	int part = code_part_from_str(str);

	if (part != LOG_LAST) {
		enabled_debug[part] = !enabled_debug[part];
	}
	if (part == LOG_ALL) {
		memset(enabled_debug, TRUE, sizeof(enabled_debug));
	}
	return (part != LOG_LAST);
}


void _debug( code_part part, const char *str, ... )
{
	va_list ap;
	static char inputBuffer[2][MAX_LEN_LOG_LINE];
	static char outputBuffer[MAX_LEN_LOG_LINE];
	static BOOL useInputBuffer1 = FALSE;

	debug_callback * curCallback = callbackRegistry;

	static unsigned int repeated = 0; /* times current message repeated */
	static unsigned int next = 2;     /* next total to print update */
	static unsigned int prev = 0;     /* total on last update */

	va_start(ap, str);
	vsnprintf( useInputBuffer1 ? inputBuffer[1] : inputBuffer[0], MAX_LEN_LOG_LINE, str, ap );
	va_end(ap);

	if ( strncmp( inputBuffer[0], inputBuffer[1], MAX_LEN_LOG_LINE - 1 ) == 0 ) {
		// Received again the same line
		repeated++;
		if (repeated == next) {
			if (repeated > 2) {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %d times (total %d repeats)", repeated - prev, repeated );
			} else {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %d times", repeated - prev );
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
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %d times (total %d repeats)", repeated - prev, repeated );
			} else {
				snprintf( outputBuffer, sizeof(outputBuffer), "last message repeated %d times", repeated - prev );
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
