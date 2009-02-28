/*
	This file is part of Warzone 2100.
	Copyright (C) 1992-2007  Trolltech ASA.
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
#include "frame.h"
#include "stdio_ext.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>


int vslcatprintf(char* str, size_t size, const char* format, va_list ap)
{
	size_t str_len;

	if (str == NULL
	 || size == 0)
	{
		return vsnprintf(NULL, 0, format, ap);
	}

	str_len = strlen(str);

	assert(str_len < size);

	return vsnprintf(&str[str_len], size - str_len, format, ap);
}


int slcatprintf(char* str, size_t size, const char* format, ...)
{
	va_list ap;
	int count;

	va_start(ap, format);
		count = vslcatprintf(str, size, format, ap);
	va_end(ap);

	return count;
}
