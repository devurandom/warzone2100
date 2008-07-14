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
/*
 * StrRes.c
 *
 * String storage an manipulation functions
 *
 */

#include <string.h>
#include <stdlib.h>

/* Allow frame header files to be singly included */
#define FRAME_LIB_INCLUDE

#include "types.h"
#include "debug.h"
#include "treap.h"
#include "strres.h"
#include "strresly.h"

/* Static forward declarations */
static void strresReleaseIDStrings(STR_RES *psRes);
static BOOL strresAllocBlock(STR_BLOCK **ppsBlock, UDWORD size);

/* The string resource currently being loaded */
STR_RES	*psCurrRes;

/* The id number of ID strings allocated by the system is ORed with this */
#define ID_ALLOC	0x80000000

/* Allocate a string block */
static BOOL strresAllocBlock(STR_BLOCK **ppsBlock, UDWORD size)
{
	*ppsBlock = (STR_BLOCK*)malloc(sizeof(STR_BLOCK));
	if (!*ppsBlock)
	{
		debug( LOG_ERROR, "strresAllocBlock: Out of memory - 1" );
		abort();
		return false;
	}

	(*ppsBlock)->apStrings = (char**)malloc(sizeof(char *) * size);
	if (!(*ppsBlock)->apStrings)
	{
		debug( LOG_ERROR, "strresAllocBlock: Out of memory - 2" );
		abort();
		free(*ppsBlock);
		return false;
	}
	memset((*ppsBlock)->apStrings, 0, sizeof(char *) * size);

#ifdef DEBUG
	(*ppsBlock)->aUsage = (UDWORD*)malloc(sizeof(UDWORD) * size);
	memset((*ppsBlock)->aUsage, 0, sizeof(UDWORD) * size);
#endif

	return true;
}


/* Initialise the string system */
BOOL strresCreate(STR_RES **ppsRes, UDWORD init, UDWORD ext)
{
	STR_RES		*psRes;

	psRes = (STR_RES*)malloc(sizeof(STR_RES));
	if (!psRes)
	{
		debug( LOG_ERROR, "strresCreate: Out of memory" );
		abort();
		return false;
	}
	psRes->init = init;
	psRes->ext = ext;
	psRes->nextID = 0;

	if (!TREAP_CREATE(&psRes->psIDTreap, treapStringCmp))
	{
		debug( LOG_ERROR, "strresCreate: Out of memory" );
		abort();
		free(psRes);
		return false;
	}

	if (!strresAllocBlock(&psRes->psStrings, init))
	{
		treapDestroy(psRes->psIDTreap);
		free(psRes);
		return false;
	}
	psRes->psStrings->psNext = NULL;
	psRes->psStrings->idStart = 0;
	psRes->psStrings->idEnd = init-1;

	*ppsRes = psRes;

	return true;
}


/* Release the id strings, but not the strings themselves,
 * (they can be accessed only by id number).
 */
static void strresReleaseIDStrings(STR_RES *psRes)
{
	STR_ID		*psID;

	ASSERT( psRes != NULL,
		"strresReleaseIDStrings: Invalid string res pointer" );

	for(psID = (STR_ID*)treapGetSmallest(psRes->psIDTreap); psID;
		psID = (STR_ID*)treapGetSmallest(psRes->psIDTreap))
	{
		treapDel(psRes->psIDTreap, (void*)psID->pIDStr);
		if (psID->id & ID_ALLOC)
		{
			free(psID->pIDStr);
			free(psID);
		}
	}
}


/* Shutdown the string system */
void strresDestroy(STR_RES *psRes)
{
	STR_BLOCK	*psBlock, *psNext = NULL;
	UDWORD		i;

	ASSERT( psRes != NULL,
		"strresDestroy: Invalid string res pointer" );

	// Free the string id's
	strresReleaseIDStrings(psRes);

	// Free the strings themselves
	for(psBlock = psRes->psStrings; psBlock; psBlock=psNext)
	{
		for(i=psBlock->idStart; i<=psBlock->idEnd; i++)
		{
#ifdef DEBUG_GROUP0
			if (psBlock->aUsage[i - psBlock->idStart] == 0
				&& i != 0 && i < psRes->nextID)
			{
// 				debug( LOG_NEVER, "strresDestroy: String id %d not used:\n               \"%s\"\n", i, psBlock->apStrings[i - psBlock->idStart] );
			}
#endif
			if (psBlock->apStrings[i - psBlock->idStart])
			{
				free(psBlock->apStrings[i - psBlock->idStart]);
			}
#ifdef DEBUG
			else if (i < psRes->nextID)
			{
				debug( LOG_NEVER, "strresDestroy: No string loaded for id %d\n", i );
			}
#endif
		}
		psNext = psBlock->psNext;
		free(psBlock->apStrings);
#ifdef DEBUG
		free(psBlock->aUsage);
#endif
		free(psBlock);
	}

	// Release the treap and free the final memory
	treapDestroy(psRes->psIDTreap);
	free(psRes);
}


/* Return the ID number for an ID string */
BOOL strresGetIDNum(STR_RES *psRes, const char *pIDStr, UDWORD *pIDNum)
{
	STR_ID	*psID;

	ASSERT( psRes != NULL,
		"strresGetIDNum: Invalid string res pointer" );

	psID = (STR_ID*)treapFind(psRes->psIDTreap, pIDStr);
	if (!psID)
	{
		*pIDNum = 0;
		return false;
	}

	if (psID->id & ID_ALLOC)
	{
		*pIDNum = psID->id & ~ID_ALLOC;
	}
	else
	{
		*pIDNum = psID->id;
	}
	return true;
}


/* Return the ID stored ID string that matches the string passed in */
BOOL strresGetIDString(STR_RES *psRes, const char *pIDStr, char **ppStoredID)
{
	STR_ID *psID;

	ASSERT( psRes != NULL, "strresGetIDString: Invalid string res pointer" );

	psID = (STR_ID*)treapFind(psRes->psIDTreap, pIDStr);
	if (!psID)
	{
		*ppStoredID = NULL;
		return false;
	}

	*ppStoredID = psID->pIDStr;
	return true;
}


/* Store a string */
BOOL strresStoreString(STR_RES *psRes, char *pID, const char *pString)
{
	STR_ID		*psID;
	char		*pNew;
	STR_BLOCK	*psBlock;
	UDWORD		id;

	ASSERT( psRes != NULL,
		"strresStoreString: Invalid string res pointer" );

	// Find the id for the string
	psID = (STR_ID*)treapFind(psRes->psIDTreap, (void*)pID);
	if (!psID)
	{
		// No ID yet so generate a new one
		psID = (STR_ID*)malloc(sizeof(STR_ID));
		if (!psID)
		{
			debug( LOG_ERROR, "strresStoreString: Out of memory" );
			abort();
			return false;
		}
		psID->pIDStr = strdup(pID);
		if (!psID->pIDStr)
		{
			debug( LOG_ERROR, "strresStoreString: Out of memory" );
			abort();
			free(psID);
			return false;
		}
		psID->id = psRes->nextID | ID_ALLOC;
		psRes->nextID += 1;
		TREAP_ADD(psRes->psIDTreap, (void*)psID->pIDStr, psID);
	}

	// Remove the ID_ALLOC bit
	id = psID->id & ~ID_ALLOC;

	// Find the block to store the string in
	for(psBlock = psRes->psStrings; psBlock->idEnd < id;
		psBlock = psBlock->psNext)
	{
		if (!psBlock->psNext)
		{
			// Need to allocate a new string block
			if (!strresAllocBlock(&psBlock->psNext, psRes->ext))
			{
				return false;
			}
			psBlock->psNext->idStart = psBlock->idEnd+1;
			psBlock->psNext->idEnd = psBlock->idEnd + psRes->ext;
			psBlock->psNext->psNext = NULL;
		}
	}

	// Put the new string in the string block
	if (psBlock->apStrings[id - psBlock->idStart] != NULL)
	{
		debug( LOG_ERROR, "strresStoreString: Duplicate string for id: %s", psID->pIDStr );
		abort();
		return false;
	}

	// Allocate a copy of the string
	pNew = strdup(pString);
	if (!pNew)
	{
		debug( LOG_ERROR, "strresStoreString: Out of memory" );
		abort();
		return false;
	}
	psBlock->apStrings[id - psBlock->idStart] = pNew;

	return true;
}


/* Get the string from an ID number */
char *strresGetString(STR_RES *psRes, UDWORD id)
{
	STR_BLOCK	*psBlock;

	ASSERT( psRes != NULL,
		"strresGetString: Invalid string res pointer" );

	// find the block the string is in
	for(psBlock = psRes->psStrings; psBlock && psBlock->idEnd < id;
		psBlock = psBlock->psNext)
	{}

	if (!psBlock)
	{
		ASSERT( false, "strresGetString: String not found" );
		// Return the default string
		return psRes->psStrings->apStrings[0];
	}

	ASSERT( psBlock->apStrings[id - psBlock->idStart] != NULL,
		"strresGetString: String not found" );

#ifdef DEBUG
	psBlock->aUsage[id - psBlock->idStart] += 1;
#endif

	if (psBlock->apStrings[id - psBlock->idStart] == NULL)
	{
		// Return the default string
		return psRes->psStrings->apStrings[0];
	}

	return psBlock->apStrings[id - psBlock->idStart];
}


/* Load a string resource file */
BOOL strresLoad(STR_RES* psRes, const char* fileName)
{
	bool retval;
	lexerinput_t input;

	input.type = LEXINPUT_PHYSFS;
	input.input.physfsfile = PHYSFS_openRead(fileName);
	if (!input.input.physfsfile)
	{
		debug(LOG_ERROR, "strresLoadFile: PHYSFS_openRead(\"%s\") failed with error: %s\n", fileName, PHYSFS_getLastError());
		return false;
	}

	// Set string resource to operate on
	psCurrRes = psRes;

	strres_set_extra(&input);
	retval = (strres_parse() == 0);

	strres_lex_destroy();
	PHYSFS_close(input.input.physfsfile);

	return retval;
}

/* Get the ID number for a string*/
UDWORD strresGetIDfromString(STR_RES *psRes, const char *pString)
{
	STR_BLOCK *psBlock, *psNext = NULL;
	unsigned int i;

	ASSERT( psRes != NULL,
		"strresGetID: Invalid string res pointer" );

	// Search through all the blocks to find the string
	for(psBlock = psRes->psStrings; psBlock != NULL; psBlock=psNext)
	{
		for(i = psBlock->idStart; i <= psBlock->idEnd; i++)
		{
			if ( psBlock->apStrings[i - psBlock->idStart] &&
				!strcmp(psBlock->apStrings[i - psBlock->idStart], pString) )
			{
				return i;
			}
		}
		psNext = psBlock->psNext;
	}
	return 0;
}



