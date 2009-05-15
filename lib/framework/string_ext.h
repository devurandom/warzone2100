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


#ifndef HAVE_VALID_STRLCPY
# ifdef HAVE_SYSTEM_STRLCPY
   // If the system provides a non-conformant strlcpy we use our own
#  ifdef strlcpy
#   undef strlcpy
#  endif
#  define strlcpy wz_strlcpy
# endif // HAVE_SYSTEM_STRLCPY

/*!
 * A safer variant of \c strncpy and its completely unsafe variant \c strcpy.
 * Copy src to string dest of size "size". At most size-1 characters will be copied.
 * Always nul-terminates, unless size = 0. Returned value is the entire length of string src.
 * \param dest a pointer to the destination buffer
 * \param src the source string to copy into the \c dest buffer
 * \param size the buffer size (in bytes) of buffer \c dest
 * \return Length to string src, if >= size truncation occured
 */
static inline size_t strlcpy(char *dest, const char *src, size_t size)
{
	assert(src != NULL);

	if (size > 0)
	{
		assert(dest != NULL);

		strncpy(dest, src, size - 1);

		// Guarantee to nul-terminate
		dest[size - 1] = '\0';
	}

	return strlen(src);
}
#endif // HAVE_VALID_STRLCPY

#ifndef HAVE_VALID_STRLCAT
# ifdef HAVE_SYSTEM_STRLCAT
   // If the system provides a non-conformant strlcat we use our own
#  ifdef strlcat
#   undef strlcat
#  endif
#  define strlcat wz_strlcat
# endif // HAVE_SYSTEM_STRLCAT
/**
 * A safer variant of \c strncat and its completely unsafe variant \c strcat.
 * Append src to string dest of size "size" (unlike strncat, size is the
 * full size of dest, not space left). At most size-1 characters will be copied.
 * Always nul-terminates, unless size < strlen(dest).
 * Returned value is the entire length of string src + min(size, strlen(dest)).
 * \param dest a pointer to the destination buffer
 * \param src the source string to copy into the \c dest buffer
 * \param size the buffer size (in bytes) of buffer \c dest
 * \return Length to string src + dest, if >= size truncation occured.
 */
static inline size_t strlcat(char *dest, const char *src, size_t size)
{
	size_t len;

	assert(src != NULL);
	len = strlen(src);

	if (size > 0)
	{
		size_t dlen;

		assert(dest != NULL);
		dlen = strnlen1(dest, size);
		len += dlen;

		assert(dlen > 0);

		strlcpy(&dest[dlen-1], src, size - dlen);
	}

	return len;
}
#endif // HAVE_VALID_STRLCAT

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
