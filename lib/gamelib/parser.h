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
/***************************************************************************/

#ifndef _PARSER_H_
#define _PARSER_H_

/***************************************************************************/

#include <stdio.h>

/***************************************************************************/

extern void		IncludeFile( char szFileName[] );
extern BOOL		ParseFile( char szFileName[] );
extern void		IncludeFile( char szFileName[] );
extern void		parserSetInputBuffer(char *pBuffer, UDWORD size);
extern BOOL		ParseResourceFile(char *pData, UDWORD fileSize);
extern BOOL		ParsingBuffer( void );
extern void		parseGetErrorData(int *pLine, char **ppText);

/***************************************************************************/

#endif	/* _PARSER_H_ */

/***************************************************************************/
