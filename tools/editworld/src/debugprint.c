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

	$Revision$
	$Id$
	$HeadURL$
*/

#include "windows.h"
#include "windowsx.h"
#include "stdio.h"
#include "tchar.h"

#ifdef _DEBUG

extern void DebugWinPrint(char *String);

FILE* DebugStream=NULL;

void DebugOpen(char* LogName)
{
	if(DebugStream) {
		fclose(DebugStream);
		DebugStream = NULL;
	}

	DebugStream = fopen(LogName,"wb");
	
	fprintf(DebugStream,"This build : %s %s\n\n",__DATE__,__TIME__);
}

void DebugClose(void)
{
	if(DebugStream) {
		fclose(DebugStream);
	}
	DebugStream=NULL;
}

void DebugPrint(const TCHAR *format, ...)
{
	TCHAR buf[4096];

	va_list args;
	va_start(args,format);
	_vsntprintf(buf,4096,format,args);
	va_end(args);
	OutputDebugString(buf);
	if(DebugStream!=NULL) {
		fprintf(DebugStream,"%s",buf);
	}

//	DebugWinPrint(buf);
}

#else

void DebugOpen(char* LogName)
{
}

void DebugClose(void)
{
}


#endif
