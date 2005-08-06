/*
 * Debug.c
 *
 * Various debugging output functions.
 *
 */

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#include <windows.h>
#include <stdio.h>
#endif

#include "frame.h"
#include "frameint.h"

#define MAX_FILENAME_SIZE 200
#define MAX_LEN_LOG_LINE 512
FILE *logfile = NULL;
static log_callback_fn callback;
static BOOL enabled_debug_parts[LOG_LAST];

/* This list _must_ match the enum in debug.h! */
static const char *code_part_names[] = {
  "all", "sound", "video", "3d", "error", "never"
};

#ifdef _MSC_VER
#define vsnprintf _vsnprintf
#define snprintf  _snprintf
#define strcasecmp stricmp 
#endif

/**********************************************************************
 cat_snprintf is like a combination of snprintf and strlcat;
 it does snprintf to the end of an existing string.

 Like mystrlcat, n is the total length available for str, including
 existing contents and trailing nul.  If there is no extra room
 available in str, does not change the string.

 Also like mystrlcat, returns the final length that str would have
 had without truncation.  I.e., if return is >= n, truncation occurred.
**********************************************************************/
static int cat_snprintf(char *str, size_t n, const char *format, ...)
{
	size_t len;
	int ret;
	va_list ap;

	assert(format != NULL);
	assert(str != NULL);
	assert(n > 0);

	len = strlen(str);
	assert(len < n);

	va_start(ap, format);
	ret = vsnprintf(str + len, n - len, format, ap);
	va_end(ap);
	return (int) (ret + len);
}

/**************************************************************************
  Convert code_part names to enum; case insensitive; returns LOG_LAST 
  if can't match.
**************************************************************************/
static enum code_part code_part_from_str(const char *str)
{
	enum code_part i;

	for (i = 0; i < LOG_LAST; i++) {
		if (strcasecmp(code_part_names[i], str) == 0) {
			return i;
		}
	}
	return LOG_LAST;
}

/**************************************************************************
  Basic output callback.  Just dump stuff to stderr.  Add newline if none.
**************************************************************************/
static void callback_debug_stderr(const char *buf)
{
	if (!strchr(buf, '\n')) {
		fprintf(stderr, "%s\n", buf);
	} else {
		fprintf(stderr, "%s", buf);
	}
}

/**************************************************************************
  Call once to initialize the debug logging system.
**************************************************************************/
void debug_init(void)
{
	memset(enabled_debug_parts, 0, sizeof(enabled_debug_parts));
	enabled_debug_parts[LOG_ERROR] = TRUE;
	callback = callback_debug_stderr;
}

/**************************************************************************
  Send debug output to given file, in addition to any other places.
  Only enabled output is sent to the file.
**************************************************************************/
void debug_to_file(char *file)
{
	assert(strlen(file) < MAX_FILENAME_SIZE);
	if (logfile) {
		fclose(logfile);
	}
	logfile = fopen(file, "a");
	if (!logfile) {
		fprintf(stderr, "Could not open %s for appending!\n",
		        file);
	} else {
		fprintf(logfile, "\nStarting log\n");
	}
}

/**************************************************************************
  Use the given callback for outputting debug logging to screen.  This 
  is in addition to any logging to file we do.
**************************************************************************/
void debug_use_callback(log_callback_fn use_callback)
{
	callback = use_callback;
}

/**************************************************************************
  Use the given callback for outputting debug logging to screen.  This 
  is in addition to any logging to file we do.
**************************************************************************/
void debug_enable_switch(const char *str)
{
	enum code_part part = code_part_from_str(str);

	if (part != LOG_LAST) {
		enabled_debug_parts[part] = !enabled_debug_parts[part];
	}
	if (part == LOG_ALL) {
		memset(enabled_debug_parts, TRUE, sizeof(enabled_debug_parts));
	}
}

/**************************************************************************
  Output to relevant places.
**************************************************************************/
static void debug_out(const char *buf)
{
	callback(buf);
	if (logfile) {
		if (!strchr(buf, '\n')) {
			fprintf(logfile, "%s\n", buf);
		} else {
			fprintf(logfile, "%s", buf);
		}
	}
}

/**************************************************************************
  Maybe output some information, depending on user settings.
**************************************************************************/
void debug(enum code_part part, const char *str, ...)
{
	va_list ap;
	static char bufbuf[2][MAX_LEN_LOG_LINE];
	char buf[MAX_LEN_LOG_LINE];
	static BOOL bufbuf1 = FALSE;
	static unsigned int repeated = 0; /* times current message repeated */
	static unsigned int next = 2;     /* next total to print update */
	static unsigned int prev = 0;     /* total on last update */

	/* Not enabled debugging for this part? Punt! */
	if (!enabled_debug_parts[part]) {
		return;
	}

	va_start(ap, str);

	vsnprintf(bufbuf1 ? bufbuf[1] : bufbuf[0], MAX_LEN_LOG_LINE, str, ap);

	if (0 == strncmp(bufbuf[0], bufbuf[1], MAX_LEN_LOG_LINE - 1)) {
		repeated++;
		if (repeated == next) {
			snprintf(buf, sizeof(buf), "last message repeated %d times",
			         repeated - prev);
			if (repeated > 2) {
				cat_snprintf(buf, sizeof(buf), " (total %d repeats)", 
				             repeated);
			}
			debug_out(buf);
			prev = repeated;
			next *= 2;
			}
		} else {
		if (repeated > 0 && repeated != prev) {
			if (repeated == 1) {
				/* just repeat the previous message: */
				debug_out(bufbuf1 ? bufbuf[0] : bufbuf[1]);
			} else {
				snprintf(buf, sizeof(buf),
					 "last message repeated %d times", 
				         repeated - prev);
				if (repeated > 2) {
					cat_snprintf(buf, sizeof(buf),
					" (total %d repeats)", repeated);
				}
				debug_out(buf);
			}
		}
		repeated = 0;
		next = 2;
		prev = 0;
		debug_out(bufbuf1 ? bufbuf[1] : bufbuf[0]);
	}
	bufbuf1 = !bufbuf1;
	fflush(logfile);
	va_end(ap);
}
