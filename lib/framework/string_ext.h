/*
	This file is part of Warzone 2100.
	Copyright (C) 2005-2006  Free Software Foundation, Inc.
	Copyright (C) 2009 Warzone Resurrection Project

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
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
	MA 02110-1301, USA.
*/
#ifndef STRING_EXT_H
#define STRING_EXT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "wzglobal.h"

#include "lib/platform/string_ext.h"

#include <string.h>
#include <stddef.h>
#include <assert.h>


/*!
 * Safe variant of strlen.
 * Finds the length of the required buffer to store string.
 * \param string holds the string to scan the required buffer size for
 * \param maxlen the maximum amount of bytes to scan in string
 * \return the required size for a buffer to hold \c string or \c maxlen if
 *         no terminating '\\0' is found.
 *
 * \note This is the same as strnlen(string, maxlen - 1) + 1 when using the
 *       GNU C library.
 */
static inline size_t strnlen1(const char* string, size_t maxlen)
{
	// Find the first NUL char
	const char* end = (const char*)memchr(string, '\0', maxlen); // Cast required for C++

	if (end != NULL)
		return end - string + 1;
	else
		return maxlen;
}


/*
 * Static array versions of common string functions. Safer because one less parameter to screw up.
 * Can only be used on strings longer than the length of a pointer, because we use this for debugging.
 */
#ifndef DEBUG
#define sstrcpy(dest, src) strlcpy((dest), (src), sizeof(dest))
#define sstrcat(dest, src) strlcat((dest), (src), sizeof(dest))
#define ssprintf(dest, ...) snprintf((dest), sizeof(dest), __VA_ARGS__)
#define vssprintf(dest, format, ap) vsnprintf((dest), sizeof(dest), format, ap)
#define sstrcmp(str1, str2) strncmp((str1), (str2), sizeof(str1) > sizeof(str2) ? sizeof(str2) : sizeof(str1))
#else
#define sstrcpy(dest, src) (WZ_ASSERT_STATIC_STRING(dest), strlcpy((dest), (src), sizeof(dest)))
#define sstrcat(dest, src) (WZ_ASSERT_STATIC_STRING(dest), strlcat((dest), (src), sizeof(dest)))
#define ssprintf(dest, ...) (WZ_ASSERT_STATIC_STRING(dest), snprintf((dest), sizeof(dest), __VA_ARGS__))
#define vssprintf(dest, format, ap) (WZ_ASSERT_STATIC_STRING(dest), vsnprintf((dest), sizeof(dest), format, ap))
#define sstrcmp(str1, str2) (WZ_ASSERT_STATIC_STRING(str1), WZ_ASSERT_STATIC_STRING(str2), strncmp((str1), (str2), sizeof(str1) > sizeof(str2) ? sizeof(str2) : sizeof(str1)))
#endif


#ifdef __cplusplus
}
#endif

#endif // STRING_EXT_H
