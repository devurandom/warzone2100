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
