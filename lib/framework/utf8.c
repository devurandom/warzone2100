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
#include "utf8.h"


static iconv_t ucs2ToUtf8Conv;
static iconv_t utf8ToUcs4Conv;


void utf8Initialise(void)
{
	ucs2ToUtf8Conv = iconv_open("UTF-8", "UCS-2");
	if (ucs2ToUtf8Conv == (iconv_t)-1)
	{
		debug(LOG_ERROR, "Unable to get a converter: UCS-2 to UTF-8!");
		abort();
	}

	utf8ToUcs4Conv = iconv_open("UCS-4", "UTF-8");
	if (utf8ToUcs4Conv == (iconv_t)-1)
	{
		debug(LOG_ERROR, "Unable to get a converter: UTF-8 to UCS-4!");
		abort();
	}
}


void utf8ShutDown(void)
{
	if (iconv_close(ucs2ToUtf8Conv) == -1)
	{
		debug(LOG_ERROR, "Unable to destroy the converter: UCS-2 to UTF-8!");
		abort();
	}
	if (iconv_close(utf8ToUcs4Conv) == -1)
	{
		debug(LOG_ERROR, "Unable to destroy the converter: UCS-2 to UTF-8!");
		abort();
	}
}


uint32_t utf8ToUcs4(const char* inbuffer, size_t inbytes)
{
	uint32_t unicode = 0;
	char * outbuffer = (char*)&unicode;
	size_t outbytes = sizeof(unicode);

	if (iconv(utf8ToUcs4Conv,
		(char**)&inbuffer, &inbytes,
		(char**)&outbuffer, &outbytes) == (size_t)-1)
	{
		debug(LOG_ERROR, "Unable to convert: UTF-8 to UCS-4!");
		abort();
	}

	return unicode;
}


size_t ucs2ToUtf8(uint16_t unicode, char* outbuffer, size_t outbytes)
{
	char * inbuffer = (char*)&unicode;
	size_t inbytes = sizeof(unicode), maxoutbytes = outbytes;

	if (iconv(ucs2ToUtf8Conv,
		&inbuffer, &inbytes,
		&outbuffer, &outbytes) == (size_t)-1)
	{
		debug(LOG_ERROR, "Unable to convert: UCS-2 to UTF-8!");
		abort();
	}

	return (maxoutbytes - outbytes);
}
