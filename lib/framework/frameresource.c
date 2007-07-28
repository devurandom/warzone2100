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
 * FrameResource.c
 *
 * Framework Resource file processing functions
 *
 */
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include "frame.h"
#include "frameresource.h"
#include "resly.h"
#include <physfs.h>

// Local prototypes
static RES_TYPE *psResTypes=NULL;

/* The initial resource directory and the current resource directory */
char aResDir[MAX_PATH];
char aCurrResDir[MAX_PATH];

// the current resource block ID
static SDWORD	resBlockID;

// buffer to load file data into
static char	*pFileBuffer = NULL;
static SDWORD	fileBufferSize = 0;

// prototypes
static void ResetResourceFile(void);

// callback to resload screen.
static RESLOAD_CALLBACK resLoadCallback=NULL;


/* set the callback function for the res loader*/
void resSetLoadCallback(RESLOAD_CALLBACK funcToCall)
{
	resLoadCallback = funcToCall;
}

/* do the callback for the resload display function */
static inline void resDoResLoadCallback(void)
{
	if(resLoadCallback)
	{
		resLoadCallback();
	}
}


/* Initialise the resource module */
BOOL resInitialise(void)
{
	ASSERT( psResTypes == NULL,
		"resInitialise: resource module hasn't been shut down??" );
	psResTypes = NULL;
	resBlockID = 0;
	resLoadCallback = NULL;

	ResetResourceFile();

	return TRUE;
}


/* Shutdown the resource module */
void resShutDown(void)
{
	if (psResTypes != NULL) {
		debug(LOG_WZ, "resShutDown: warning resources still allocated");
		resReleaseAll();
	}
}


// set the base resource directory
void resSetBaseDir(char *pResDir)
{
	strncpy(aResDir, pResDir, MAX_PATH - 1);
}

/* Parse the res file */
BOOL resLoad(const char *pResFile, SDWORD blockID,
             char *pLoadBuffer, SDWORD bufferSize)
{
	char *pBuffer;
	UDWORD	size;

	strcpy(aCurrResDir, aResDir);

	// Note the buffer for file data
	pFileBuffer = pLoadBuffer;
	fileBufferSize = bufferSize;

	// Note the block id number
	resBlockID = blockID;

	debug(LOG_WZ, "resLoad: loading %s", pResFile);

	// Load the RES file; allocate memory for a wrf, and load it
	if (!loadFile(pResFile, &pBuffer, &size))
	{
		debug(LOG_ERROR, "resLoad: failed to load %s", pResFile);
		return FALSE;
	}

	// and parse it
	resSetInputBuffer(pBuffer, size);
	if (res_parse() != 0)
	{
		debug(LOG_ERROR, "resLoad: failed to parse %s", pResFile);
		free(pBuffer);
		return FALSE;
	}

	free(pBuffer);

	return TRUE;
}


/* Allocate a RES_TYPE structure */
static RES_TYPE* resAlloc(const char *pType)
{
	RES_TYPE	*psT;

#ifdef DEBUG
	// Check for a duplicate type
	for(psT = psResTypes; psT; psT = psT->psNext)
	{
		ASSERT( strcmp(psT->aType, pType) != 0,
			"resAlloc: Duplicate function for type: %s", pType );
	}
#endif

	// Allocate the memory
	psT = (RES_TYPE *)malloc(sizeof(RES_TYPE));
	if (!psT)
	{
		debug( LOG_ERROR, "resAlloc: Out of memory" );
		abort();
		return NULL;
	}

	// setup the structure
	strncpy(psT->aType, pType, RESTYPE_MAXCHAR - 1);
	psT->aType[RESTYPE_MAXCHAR - 1] = 0;

	psT->HashedType=HashString(psT->aType);		// store a hased version for super speed !

	psT->psRes = NULL;

	return psT;
}



/* Add a buffer load function for a file type */
BOOL resAddBufferLoad(const char *pType, RES_BUFFERLOAD buffLoad,
					  RES_FREE release)
{
	RES_TYPE	*psT = resAlloc(pType);

	if (!psT)
	{
		return FALSE;
	}

	psT->buffLoad = buffLoad;
	psT->fileLoad = NULL;
	psT->release = release;

	psT->psNext = psResTypes;
	psResTypes = psT;

	return TRUE;
}


/* Add a file name load function for a file type */
BOOL resAddFileLoad(const char *pType, RES_FILELOAD fileLoad,
					RES_FREE release)
{
	RES_TYPE	*psT = resAlloc(pType);

	if (!psT)
	{
		return FALSE;
	}

	psT->buffLoad = NULL;
	psT->fileLoad = fileLoad;
	psT->release = release;

	psT->psNext = psResTypes;
	psResTypes = psT;

	return TRUE;
}

// Make a string lower case
void resToLower(char *pStr)
{
	while (*pStr != 0)
	{
		if (isupper(*pStr))
		{
			*pStr = (char)(*pStr - (char)('A' - 'a'));
		}
		pStr += 1;
	}
}


static char LastResourceFilename[MAX_PATH];

/*!
 * Returns the filename of the last resource file loaded
 * The filename is always null terminated
 */
const char *GetLastResourceFilename(void)
{
	return LastResourceFilename;
}

/*!
 * Set the resource name of the last resource file loaded
 */
void SetLastResourceFilename(const char *pName)
{
	strncpy(LastResourceFilename, pName, MAX_PATH-1);
	LastResourceFilename[MAX_PATH-1] = '\0';
}


// Structure for each file currently in use in the resource  ... probably only going to be one ... but we will handle upto MAXLOADEDRESOURCE
typedef struct
{
  	char *pBuffer;	// a pointer to the data
	UDWORD size;	// number of bytes
	UBYTE	type;	// what type of resource is it
} RESOURCEFILE;

#define RESFILETYPE_EMPTY (0)			// empty entry
#define RESFILETYPE_PC_SBL (1)			// Johns SBL stuff
#define RESFILETYPE_LOADED (2)			// Loaded from a file (!)
#define RESFILETYPE_WDGPTR (3)			// A pointer from the WDG cache


#define MAXLOADEDRESOURCES (6)
static RESOURCEFILE LoadedResourceFiles[MAXLOADEDRESOURCES];



// Clear out the resource list ... needs to be called during init.
static void ResetResourceFile(void)
{
	UWORD i;

	for (i=0;i<MAXLOADEDRESOURCES;i++)
	{
		LoadedResourceFiles[i].type=RESFILETYPE_EMPTY;
	}
}

// Returns an empty resource entry or -1 if none exsist
static SDWORD FindEmptyResourceFile(void)
{
	UWORD i;
	for (i=0;i<MAXLOADEDRESOURCES ;i++ )
	  {
		if (LoadedResourceFiles[i].type==RESFILETYPE_EMPTY)
			return(i);

	  }
	return(-1);			// ERROR
}


// Get a resource data file ... either loads it or just returns a pointer
static BOOL RetreiveResourceFile(char *ResourceName, RESOURCEFILE **NewResource)
{
	SDWORD ResID;
	RESOURCEFILE *ResData;
	UDWORD size;
	char *pBuffer;

	ResID=FindEmptyResourceFile();
	if (ResID==-1) return(FALSE);		// all resource files are full

	ResData= &LoadedResourceFiles[ResID];
	*NewResource=ResData;

#if 0
	if (pFileBuffer && loadFile(ResourceName, &pBuffer, &size)) {
		ResData->type=RESFILETYPE_PC_SBL;
		ResData->size=size;
		ResData->pBuffer=pBuffer;
		return(TRUE);
	}
#endif

	// This is needed for files that do not fit in the WDG cache ... (VAB file for example)
	if (!loadFile(ResourceName, &pBuffer, &size))
	{
		return FALSE;
	}

	ResData->type=RESFILETYPE_LOADED;
	ResData->size=size;
	ResData->pBuffer=pBuffer;
	return(TRUE);
}


// Free up the file depending on what type it is
static void FreeResourceFile(RESOURCEFILE *OldResource)
{
	switch (OldResource->type)
	{
		case RESFILETYPE_LOADED:
			free(OldResource->pBuffer);
			OldResource->pBuffer = NULL;
			break;

		default:
			debug(LOG_WARNING, "resource not freed");
	}


	// Remove from the list
	OldResource->type=RESFILETYPE_EMPTY;
}


static inline RES_DATA* resDataInit(const char *DebugName, UDWORD DataIDHash, void *pData, UDWORD BlockID)
{
	RES_DATA* psRes = malloc(sizeof(RES_DATA));
	if (!psRes)
	{
		debug(LOG_ERROR, "resDataInit: Out of memory");
		return NULL;
	}

	psRes->aID = malloc(strlen(DebugName) + 1);
	if (!psRes->aID)
	{
		debug(LOG_ERROR, "resDataInit: Out of memory");
		return NULL;
	}

	strcpy((char*)psRes->aID, DebugName);

	psRes->pData = pData;
	psRes->blockID = BlockID;
	psRes->HashedID = DataIDHash;

	psRes->usage = 0;

	return psRes;
}


/*!
 * Return the language part of the selected locale
 */
static WZ_DECL_CONST const char* getLanguage(void)
{
	static char language[4] = { '\0' }; // ISO639 language code has to fit in!

#ifdef ENABLE_NLS
	static BOOL haveLanguage = FALSE;

	if ( ! haveLanguage )  // only get language name once for speed optimization
	{
		char *localeName = setlocale(LC_MESSAGES, NULL);
		char *delim = NULL;

		haveLanguage = TRUE;

		if ( !localeName )
		{
			return language; // Return empty string on errors
		}

		strncpy(language, localeName, sizeof(language));
		language[sizeof(language) - 1] = '\0';  // be sure to have a 0-terminated string

		delim = strchr(language, '_');

		if ( !delim )
		{
			delim = strchr(language, '.');
		}

		if ( delim )
		{
			*delim = '\0';
		}
	}
#endif // ENABLE_NLS

	return language;
}


/*!
 * check if given file exists in a locale dependend subdir
 * if so, modify given fileName to hold the locale dep. file,
 * else do not change given fileName
 */
static void makeLocaleFile(char fileName[])  // given string must have MAX_PATH size
{
#ifdef ENABLE_NLS
	const char * language = getLanguage();
	char localeFile[MAX_PATH];

	if ( language[0] == '\0' || // could not get language
		 strlen(fileName) + strlen(language) + 1 >= MAX_PATH )
	{
		return;
	}

	snprintf(localeFile, sizeof(localeFile), "locale/%s/%s", language, fileName);

	if ( PHYSFS_exists(localeFile) )
	{
		strncpy(fileName, localeFile, sizeof(localeFile));
		debug(LOG_WZ, "Found translated file: %s", fileName);
	}
#endif // ENABLE_NLS

	return;
}


/*!
 * Call the load function (registered in data.c)
 * for this filetype
 */
BOOL resLoadFile(const char *pType, const char *pFile)
{
	RES_TYPE	*psT;
	void		*pData;
	RES_DATA	*psRes;
	char		aFileName[MAX_PATH];
	UDWORD HashedName, HashedType = HashString(pType);

	// Find the resource-type
	for(psT = psResTypes; psT != NULL; psT = psT->psNext )
	{
		if (psT->HashedType == HashedType)
		{
			break;
		}
	}

	if (psT == NULL)
	{
		debug(LOG_WZ, "resLoadFile: Unknown type: %s", pType);
		return FALSE;
	}

	// Check for duplicates
	HashedName = HashStringIgnoreCase(pFile);
	for (psRes = psT->psRes; psRes; psRes = psRes->psNext)
	{
		if(psRes->HashedID == HashedName)
		{
			debug(LOG_WZ, "resLoadFile: Duplicate file name: %s (hash %x) for type %s",
			      pFile, HashedName, psT->aType);
			// assume that they are actually both the same and silently fail
			// lovely little hack to allow some files to be loaded from disk (believe it or not!).
			return TRUE;
		}
	}

	// Create the file name
	if (strlen(aCurrResDir) + strlen(pFile) + 1 >= MAX_PATH)
	{
		debug(LOG_ERROR, "resLoadFile: Filename too long!! %s%s", aCurrResDir, pFile);
		return FALSE;
	}
	strcpy(aFileName, aCurrResDir);
	strcat(aFileName, pFile);

	makeLocaleFile(aFileName);  // check for translated file

	SetLastResourceFilename(pFile); // Save the filename in case any routines need it

	// load the resource
	if (psT->buffLoad)
	{
		RESOURCEFILE *Resource;

		// Load the file in a buffer
		if (!RetreiveResourceFile(aFileName, &Resource))
		{
			debug(LOG_ERROR, "resLoadFile: Unable to retreive resource - %s", aFileName);
			return(FALSE);
		}

		// Now process the buffer data
		if (!psT->buffLoad(Resource->pBuffer, Resource->size, &pData))
		{
			FreeResourceFile(Resource);
			psT->release( pData );
			return FALSE;
		}

		FreeResourceFile(Resource);
	}
	else if(psT->fileLoad)
	{
		// Process data directly from file
		if (!psT->fileLoad(aFileName, &pData))
		{
			psT->release( pData );
			return FALSE;
		}
	}
	else
	{
		debug(LOG_ERROR, "resLoadFile:  No load functions for this type (%s)", pType);
		return FALSE;
	}

	resDoResLoadCallback();		// do callback.

	// Set up the resource structure if there is something to store
	if (pData != NULL)
	{
		// LastResourceFilename may have been changed (e.g. by TEXPAGE loading)
		psRes = resDataInit( GetLastResourceFilename(), HashStringIgnoreCase(GetLastResourceFilename()), pData, resBlockID );
		if (!psRes)
		{
			psT->release(pData);
			return FALSE;
		}

		// Add the resource to the list
		psRes->psNext = psT->psRes;
		psT->psRes = psRes;
	}
	return TRUE;
}


/* Return the resource for a type and hashedname */
void *resGetDataFromHash(const char *pType, UDWORD HashedID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;
	UDWORD HashedType;

	// Find the correct type
	HashedType=HashString(pType);	// la da la

	for(psT = psResTypes; psT != NULL; psT = psT->psNext )
	{
		if (psT->HashedType==HashedType)
		{
			break;
		}
	}

	if (psT == NULL)
	{
		ASSERT( FALSE, "resGetData: Unknown type: %s", pType );
		return NULL;
	}

	for(psRes = psT->psRes; psRes; psRes = psRes->psNext)
	{
		if (psRes->HashedID == HashedID)
		{
			/* We found it */
			break;
		}
	}

	if (psRes == NULL)
	{
		ASSERT( psRes != NULL, "resGetDataFromHash: Unknown ID:" );
		return NULL;
	}

	psRes->usage += 1;

	return psRes->pData;
}


/* Return the resource for a type and ID */
void *resGetData(const char *pType, const char *pID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;
	// Find the correct type
	UDWORD HashedType = HashString(pType);
	UDWORD HashedID = HashStringIgnoreCase(pID);

	for(psT = psResTypes; psT != NULL; psT = psT->psNext )
	{
		if (psT->HashedType == HashedType)
		{
			break;
		}
	}

	if (psT == NULL)
	{
		ASSERT( FALSE, "resGetData: Unknown type: %s", pType );
		return NULL;
	}

	for(psRes = psT->psRes; psRes != NULL; psRes = psRes->psNext)
	{
		if (psRes->HashedID == HashedID)
		{
			/* We found it */
			break;
		}
	}

	if (psRes == NULL)
	{
		ASSERT( psRes != NULL, "resGetData: Unknown ID: %s of type %s", pID, pType );
		return NULL;
	}

	psRes->usage += 1;

	return psRes->pData;
}


BOOL resGetHashfromData(const char *pType, const void *pData, UDWORD *pHash)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;

	// Find the correct type
	UDWORD	HashedType=HashString(pType);

	for(psT = psResTypes; psT != NULL; psT = psT->psNext )
	{
		if (psT->HashedType==HashedType)
		{
			break;
		}
	}

	if (psT == NULL)
	{
		ASSERT( FALSE, "resGetHashfromData: Unknown type: %x", HashedType );
		return FALSE;
	}

	// Find the resource
	for(psRes = psT->psRes; psRes; psRes = psRes->psNext)
	{

		if (psRes->pData == pData)
		{
			break;
		}
	}

	if (psRes == NULL)
	{
		ASSERT( FALSE, "resGetHashfromData:: couldn't find data for type %x\n", HashedType );
		return FALSE;
	}

	*pHash = psRes->HashedID;

	return TRUE;
}


/* Simply returns true if a resource is present */
BOOL resPresent(const char *pType, const char *pID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;

	// Find the correct type
	UDWORD HashedType=HashString(pType);

	for(psT = psResTypes; psT != NULL; psT = psT->psNext )
	{
		if (psT->HashedType==HashedType)
		{
			break;
		}
	}

	/* Bow out if unrecognised type */
	if (psT == NULL)
	{
//		ASSERT( FALSE, "resPresent: Unknown type" );
		return FALSE;
	}

	{
		UDWORD HashedID=HashStringIgnoreCase(pID);
//		DBPRINTF(("%x - %d\n",HashedID,pID));
		for(psRes = psT->psRes; psRes; psRes = psRes->psNext)
		{
//	DBPRINTF(("!= %x\n",psRes->HashedID));
			if (psRes->HashedID==HashedID)
			{
				/* We found it */
				break;
			}
		}
	}

	/* Did we find it? */
	if (psRes != NULL)
	{
		return (TRUE);
	}

	return (FALSE);
}


/* Release all the resources currently loaded and the resource load functions */
void resReleaseAll(void)
{
	RES_TYPE *psT, *psNT;

	resReleaseAllData();

	for(psT = psResTypes; psT != NULL; psT = psNT)
	{
		psNT = psT->psNext;
		free(psT);
	}

	psResTypes = NULL;
}


/* Release all the resources currently loaded but keep the resource load functions */
void resReleaseAllData(void)
{
	RES_TYPE *psT;
	RES_DATA *psRes, *psNRes;

	for (psT = psResTypes; psT != NULL; psT = psT->psNext)
	{
		for(psRes = psT->psRes; psRes != NULL; psRes = psNRes)
		{
			if (psRes->usage == 0)
			{
				debug(LOG_WZ, "resReleaseAllData: %s resource: %s(%04x) not used", psT->aType, psRes->aID, psRes->HashedID);
			}

			if (psT->release != NULL)
			{
				psT->release(psRes->pData);
			}

			psNRes = psRes->psNext;
			free(psRes);
		}

		psT->psRes = NULL;
	}
}


// release the data for a particular block ID
void resReleaseBlockData(SDWORD blockID)
{
	RES_TYPE	*psT, *psNT;
	RES_DATA	*psPRes, *psRes, *psNRes;

	for(psT = psResTypes; psT != NULL; psT = psNT)
	{
		psPRes = NULL;
		for(psRes = psT->psRes; psRes; psRes = psNRes)
		{
			ASSERT(psRes != NULL, "resReleaseBlockData: null pointer passed into loop");

			if (psRes->blockID == blockID)
			{
				if (psRes->usage == 0)
				{
					debug(LOG_WZ, "resReleaseBlockData: %s resource: %s(%04x) not used", psT->aType, psRes->aID,
					      psRes->HashedID);
				}
				if(psT->release != NULL)
				{
					psT->release( psRes->pData );
				}
				else
				{
					ASSERT( FALSE,"resReleaseAllData: NULL release function" );
				}

				psNRes = psRes->psNext;
				free(psRes);

				if (psPRes == NULL)
				{
					psT->psRes = psNRes;
				}
				else
				{
					psPRes->psNext = psNRes;
				}
			}
			else
			{
				psPRes = psRes;
				psNRes = psRes->psNext;
			}
		}

		psNT = psT->psNext;
	}
}
