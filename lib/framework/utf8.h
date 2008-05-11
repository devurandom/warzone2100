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
#ifndef WZ_UTF8_H
#define WZ_UTF8_H


/** Initialise the utf8 module. */
extern void utf8Initialise(void);

/** Shutdown the utf8 module. */
extern void utf8ShutDown(void);

/*! Convert an UCS-2 unicode to an UTF-8 sequence */
extern size_t ucs2ToUtf8(uint16_t unicode, char* outbuffer, size_t outbytes);

/*! Convert an UTF-8 sequence to an UCS-4 unicode */
extern uint32_t utf8ToUcs4(const char* inbuffer, size_t inbytes);

/*! Detect whether code is out of the mid of a UTF-8 sequence */
static inline bool utf8IsMid(char code)
{
	return (((1<<7) & code) && !((1<<6) & code)); // Test for b10xxxxxx
}


/*! Detect whether code is the start of a UTF-8 sequence */
static inline bool utf8IsStart(char code)
{
	return (
		(((1<<7) & code) && ((1<<6) & code)) // Test for b11xxxxxx
		|| !((1<<7) & code) // or b0xxxxxxx
	);
}


/*! Finds previous start of an utf8 sequence *at or before* pos */
static inline const char * utf8FindPrevSign(const char* min, const char* pos)
{
	while (pos > min && utf8IsMid(*pos))
	{
		pos--;
	}

	assert(utf8IsStart(*pos));

	return pos;
}


/*! Finds next start of an utf8 sequence *at or after* pos */
static inline const char * utf8FindNextSign(const char* max, const char* pos)
{
	while (pos < max && utf8IsMid(*pos))
	{
		pos++;
	}

	assert(utf8IsStart(*pos));

	return pos;
}


static inline size_t utf8SignSize(const char* max, const char* pos)
{
	return utf8FindNextSign(max, pos + 1) - pos;
}

#endif /* WZ_UTF8_H */
