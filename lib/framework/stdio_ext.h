/*
	This file is part of Warzone 2100.
	Copyright (C) 2007  Giel van Schijndel
	Copyright (C) 2007-2009  Warzone Resurrection Project

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
#ifndef STDIO_EXT_H
#define STDIO_EXT_H

#include "wzglobal.h"

#include <stdarg.h>

#if defined(__cplusplus)
extern "C" {
#endif

/** A variant on snprintf which appends its output string to the given string
 *  buffer, rather than to replace it.
 *  \param str the string to append to
 *  \param size the size of the buffer \c str expressed in bytes
 *  \param format the formatting string
 *  \param ap a variable arguments list of variables to use in the formatting
 *            string
 *  \return the amount of characters appended to the string
 */
extern int vslcatprintf(char* str, size_t size, const char* format, va_list ap);


/** A variant on snprintf which appends its output string to the given string
 *  The function's interface is similar to vslcatprintf(), so look at that
 *  function's description.
 */
extern int slcatprintf(char* str, size_t size, const char* format, ...) WZ_DECL_FORMAT(printf, 3, 4);


// A stack-allocating variant of sprintf
#define sasprintf(strp, format, ...) \
do { \
	/* Make sure to evaluate "format" just once */ \
	const char* fmt = format; \
	/* Determine the size of the string we're going to produce */ \
	size_t size = snprintf(NULL, 0, fmt, __VA_ARGS__); \
	\
	/* Let the compiler perform some static type-checking */ \
	char** var = strp; \
	\
	/* Allocate a buffer large enough to hold our string on the stack*/ \
	*var = (char*)alloca(size + 1); \
	/* Print into our newly created string-buffer */ \
	sprintf(*var, fmt,  __VA_ARGS__); \
} while(0)

#if defined(__cplusplus)
}
#endif

#endif // STDIO_EXT_H
