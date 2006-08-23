/*
 * FrameResource.c
 *
 * Framework Resource file processing functions
 *
 */
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "frame.h"
#include "frameresource.h"
#include "resly.h"

// Local prototypes
static RES_TYPE *psResTypes=NULL;

// check to see if RES_TYPE entry is valid
// this is a NULL check (ie linked list)
#define resValidType(Type) (Type)

// linked list macros
#define resNextType(Type)  (Type->psNext)
#define resGetResDataPointer(psRes) (psRes->pData)
#define resGetResBlockID(psRes) (psRes->blockID)

/* The initial resource directory and the current resource directory */
STRING	aResDir[FILE_MAXCHAR];
STRING	aCurrResDir[FILE_MAXCHAR];

// the current resource block ID
static SDWORD	resBlockID;

// buffer to load file data into
static char	*pFileBuffer = NULL;
static SDWORD	fileBufferSize = 0;

// prototypes
static void ResetResourceFile(void);

// callback to resload screen.
static RESLOAD_CALLBACK resLoadCallback=NULL;
static RESPRELOAD_CALLBACK resPreLoadCallback=NULL;


void resSetPreLoadCallback(RESPRELOAD_CALLBACK funcToCall)
{
	resPreLoadCallback=funcToCall;
}

/* set the callback function for the res loader*/
VOID resSetLoadCallback(RESLOAD_CALLBACK funcToCall)
{
	resLoadCallback = funcToCall;
}

/* do the callback for the resload display function */
VOID resDoResLoadCallback()
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
	resPreLoadCallback = NULL;

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
void resSetBaseDir(STRING *pResDir)
{
	strncpy(aResDir, pResDir, FILE_MAXCHAR - 1);
}

/* Parse the res file */
BOOL resLoad(STRING *pResFile, SDWORD blockID,
             char *pLoadBuffer, SDWORD bufferSize,
             BLOCK_HEAP *psMemHeap)
{
	char *pBuffer;
	UDWORD	size;
	BLOCK_HEAP *psOldHeap;

	strcpy(aCurrResDir, aResDir);

	// Note the buffer for file data
	pFileBuffer = pLoadBuffer;
	fileBufferSize = bufferSize;

	// Note the block id number
	resBlockID = blockID;

	debug(LOG_WZ, "resLoad: loading %s", pResFile);

	// make sure the WRF doesn't get loaded into a block heap
	psOldHeap = memGetBlockHeap();
	memSetBlockHeap(NULL);

	// Load the RES file; allocate memory for a wrf, and load it
	if (!loadFile(pResFile, &pBuffer, &size)) {
		debug(LOG_ERROR, "resLoad: failed to load %s", pResFile);
		return FALSE;
	}

	// now set the memory system to use the block heap
	memSetBlockHeap(psMemHeap);

	// and parse it
	resSetInputBuffer(pBuffer, size);
	if (res_parse() != 0) {
		debug(LOG_ERROR, "resLoad: failed to parse %s", pResFile);
		return FALSE;
	}

	// reset the memory system
	memSetBlockHeap(psOldHeap);

	FREE(pBuffer);

	return TRUE;
}


/* Allocate a RES_TYPE structure */
static BOOL resAlloc(STRING *pType, RES_TYPE **ppsFunc)
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
	psT = (RES_TYPE *)MALLOC(sizeof(RES_TYPE));
	if (!psT)
	{
		debug( LOG_ERROR, "resAlloc: Out of memory" );
		abort();
		return FALSE;
	}

	// setup the structure
	strncpy(psT->aType, pType, RESTYPE_MAXCHAR - 1);
	psT->aType[RESTYPE_MAXCHAR - 1] = 0;

	psT->HashedType=HashString(psT->aType);		// store a hased version for super speed !

	psT->psRes = NULL;

	*ppsFunc = psT;

	return TRUE;
}



/* Add a buffer load function for a file type */
BOOL resAddBufferLoad(STRING *pType, RES_BUFFERLOAD buffLoad,
					  RES_FREE release)
{
	RES_TYPE	*psT;

	if (!resAlloc(pType, &psT))
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
BOOL resAddFileLoad(STRING *pType, RES_FILELOAD fileLoad,
					RES_FREE release)
{
	RES_TYPE	*psT;

	if (!resAlloc(pType, &psT))
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
void resToLower(STRING *pStr)
{
	while (*pStr != 0)
	{
		if (isupper(*pStr))
		{
			*pStr = (STRING)(*pStr - (STRING)('A' - 'a'));
		}
		pStr += 1;
	}
}


static char LastResourceFilename[FILE_MAXCHAR];

// Returns the filename of the last resource file loaded
char *GetLastResourceFilename(void)
{
	return(LastResourceFilename);
}

// Set the resource name of the last resource file loaded
void SetLastResourceFilename(char *pName)
{
	strncpy(LastResourceFilename, pName, FILE_MAXCHAR-1);
	LastResourceFilename[FILE_MAXCHAR-1] = 0;
}


static UDWORD LastHashName;

// Returns the filename of the last resource file loaded
UDWORD GetLastHashName(void)
{
	return (LastHashName);
}

// Set the resource name of the last resource file loaded
void SetLastHashName(UDWORD HashName)
{
	LastHashName = HashName;
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
SDWORD FindEmptyResourceFile(void)
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
BOOL RetreiveResourceFile(char *ResourceName, RESOURCEFILE **NewResource)
{
	SDWORD ResID;
	RESOURCEFILE *ResData;
	UDWORD size;
	char *pBuffer;

	ResID=FindEmptyResourceFile();
	if (ResID==-1) return(FALSE);		// all resource files are full

	ResData= &LoadedResourceFiles[ResID];
	*NewResource=ResData;

	if (pFileBuffer && loadFile(ResourceName, &pBuffer, &size)) {
		ResData->type=RESFILETYPE_PC_SBL;
		ResData->size=size;
		ResData->pBuffer=pBuffer;
		return(TRUE);
	}

	blockSuspendUsage();

	// This is needed for files that do not fit in the WDG cache ... (VAB file for example)
	if (!loadFile(ResourceName, &pBuffer, &size))
	{
		return FALSE;
	}

	blockUnsuspendUsage();

	ResData->type=RESFILETYPE_LOADED;
	ResData->size=size;
	ResData->pBuffer=pBuffer;
	return(TRUE);
}


// Free up the file depending on what type it is
void FreeResourceFile(RESOURCEFILE *OldResource)
{
	switch (OldResource->type)
	  {
		case RESFILETYPE_LOADED:
			FREE(OldResource->pBuffer);
			break;
	  }


	// Remove from the list
	OldResource->type=RESFILETYPE_EMPTY;
}


void resDataInit(RES_DATA* psRes, STRING *DebugName, UDWORD DataIDHash, void *pData, UDWORD BlockID)
{
	psRes->pData = pData;
	psRes->blockID = resBlockID;
	psRes->HashedID=DataIDHash;

	strcpy(psRes->aID, DebugName);
	psRes->usage = 0;
}


/* Call the load function for a file */
BOOL resLoadFile(STRING *pType, STRING *pFile)
{
	RES_TYPE	*psT;
	void		*pData;
	RES_DATA	*psRes;
	STRING		aFileName[FILE_MAXCHAR];
	BOOL loadresource;
	UDWORD HashedName;

	loadresource=TRUE;
	if(resPreLoadCallback)
	{
		loadresource=resPreLoadCallback(pType,pFile,aCurrResDir);
	}

	if (loadresource==TRUE)
	{
		UDWORD HashedType=HashString(pType);

		for(psT = psResTypes; resValidType(psT); psT = resNextType(psT) )
		{
			if (psT->HashedType==HashedType)
			{
				break;
			}
		}

		if (psT == NULL) {
			debug(LOG_WZ, "resLoadFile: Unknown type: %s", pType);
			return FALSE;
		}

		HashedName = HashStringIgnoreCase(pFile);
		for (psRes = psT->psRes; psRes; psRes = psRes->psNext) {
			if(psRes->HashedID == HashedName) {
				debug(LOG_WZ, "resLoadFile: Duplicate file name: %s (hash %x) for type %s",
				      pFile, HashedName, psT->aType);
				// assume that they are actually both the same and silently fail
				// lovely little hack to allow some files to be loaded from disk (believe it or not!).
				return TRUE;
			}
		}

		// Create the file name
		if (strlen(aCurrResDir) + strlen(pFile) + 1 >= FILE_MAXCHAR) {
			debug(LOG_ERROR, "resLoadFile: Filename too long!! %s%s", aCurrResDir, pFile);
			return FALSE;
		}
		strcpy(aFileName, aCurrResDir);
		strcat(aFileName, pFile);

		strcpy(LastResourceFilename,pFile);	// Save the filename in case any routines need it
		resToLower(LastResourceFilename);
		SetLastHashName(HashStringIgnoreCase(LastResourceFilename));

		// load the resource
		if (psT->buffLoad) {
			RESOURCEFILE *Resource;
			BOOL Result;

			Result=RetreiveResourceFile(aFileName,&Resource);
			if (Result == FALSE) {
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
			resDoResLoadCallback();		// do callback.
		} else {
			debug(LOG_ERROR, "resLoadFile:  No load functions for this type (%s)", pType);
			return FALSE;
		}

		// Set up the resource structure if there is something to store
		if (pData != NULL)
		{
			psRes = (RES_DATA*)MALLOC(sizeof(RES_DATA));
			if (!psRes)
			{
				debug(LOG_ERROR, "resLoadFile: Out of memory");
				psT->release(pData);
				return FALSE;
			}
			// LastResourceFilename may have been changed (e.g. by TEXPAGE loading)
			resDataInit( psRes, LastResourceFilename, HashStringIgnoreCase(LastResourceFilename), pData, resBlockID );

			// Add the resource to the list
			psRes->psNext = psT->psRes;
			psT->psRes = psRes;
		}
	}
	return TRUE;
}


/* Return the resource for a type and hashedname */
void *resGetDataFromHash(STRING *pType, UDWORD HashedID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;
	UDWORD HashedType;

	// Find the correct type
	HashedType=HashString(pType);	// la da la

	for(psT = psResTypes; resValidType(psT); psT = resNextType(psT) )
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

	{
//		UDWORD HashedID=HashStringIgnoreCase(pID);
		for(psRes = psT->psRes; psRes; psRes = psRes->psNext)
		{
			if (psRes->HashedID==HashedID)
			{
				/* We found it */
				break;
			}
		}
	}

	if (psRes == NULL)
	{
		ASSERT( FALSE, "resGetDataFromHash: Unknown ID:" );
		return NULL;
	}

	psRes->usage += 1;

	return resGetResDataPointer(psRes);
}


/* Return the resource for a type and ID */
void *resGetData(STRING *pType, STRING *pID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;
	UDWORD HashedType;
	// Find the correct type

	HashedType=HashString(pType);	// la da la
//printf("[resGetData] entering with %s / %s  = %0x\n",pID,pType,HashedType);

	for(psT = psResTypes; resValidType(psT); psT = resNextType(psT) )
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

	{
		UDWORD HashedID=HashStringIgnoreCase(pID);
		for(psRes = psT->psRes; psRes; psRes = psRes->psNext)
		{
			if (psRes->HashedID==HashedID)
			{
				/* We found it */
//				printf("[resGetData] looking for %s = %0x  ******found!\n",pID,HashedID);
				break;
			}
		}
	}

	if (psRes == NULL)
	{
		ASSERT( FALSE, "resGetData: Unknown ID: %s", pID );
//		resLoadFile(pType,pID);
//		resGetData(pType,pID);
//		return NULL;
	}

	psRes->usage += 1;

	return resGetResDataPointer(psRes);
}


BOOL resGetHashfromData(STRING *pType, void *pData, UDWORD *pHash)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;

	// Find the correct type
	UDWORD	HashedType=HashString(pType);

	for(psT = psResTypes; resValidType(psT); psT = resNextType(psT) )
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

		if (resGetResDataPointer(psRes) == pData)
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
BOOL resPresent(STRING *pType, STRING *pID)
{
	RES_TYPE	*psT;
	RES_DATA	*psRes;

	// Find the correct type
	UDWORD HashedType=HashString(pType);

	for(psT = psResTypes; resValidType(psT); psT = resNextType(psT) )
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
	RES_TYPE	*psT, *psNT;
	RES_DATA	*psRes, *psNRes;

	for(psT = psResTypes; resValidType(psT); psT = psNT)
	{
		for(psRes = psT->psRes; psRes; psRes = psNRes) {
			if (psRes->usage == 0) {
				debug(LOG_WZ, "%s resource: %s(%04x) not used", psT->aType,
				      psRes->aID, psRes->HashedID);
			}
			if(psT->release != NULL) {
				psT->release( resGetResDataPointer(psRes) );
			} else {
				ASSERT( FALSE,"resReleaseAll: NULL release function" );
			}
			psNRes = psRes->psNext;
			FREE(psRes);
		}
		psNT = resNextType(psT);

		FREE(psT);
	}

	psResTypes = NULL;
}


// release the data for a particular block ID
void resReleaseBlockData(SDWORD blockID)
{
	RES_TYPE	*psT, *psNT;
	RES_DATA	*psPRes, *psRes, *psNRes;

	for(psT = psResTypes; resValidType(psT); psT = psNT)
	{
		psPRes = NULL;
		for(psRes = psT->psRes; psRes; psRes = psNRes)
		{
			ASSERT( psRes != NULL,"resReleaseBlockData: null pointer passed into loop" );

			if (resGetResBlockID(psRes) == blockID) {
				if (psRes->usage == 0) {
					debug(LOG_WZ, "%s resource: %s(%04x) not used", psT->aType, psRes->aID,
					      psRes->HashedID);
				}
				if(psT->release != NULL)
				{
					psT->release( resGetResDataPointer(psRes) );
				}
				else
				{
					ASSERT( FALSE,"resReleaseAllData: NULL release function" );
				}

				psNRes = psRes->psNext;
				FREE(psRes);

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
			ASSERT( psNRes != (RES_DATA *)0xdddddddd,"resReleaseBlockData: next data (next pointer) already freed" );
		}
		psNT = resNextType(psT);
		ASSERT( psNT != (RES_TYPE *)0xdddddddd,"resReleaseBlockData: next data (next pointer) already freed" );
	}
}


/* Release all the resources currently loaded but keep the resource load functions */
void resReleaseAllData(void)
{
	RES_TYPE	*psT, *psNT;
	RES_DATA	*psRes, *psNRes;

	for (psT = psResTypes; resValidType(psT); psT = psNT) {
		for (psRes = psT->psRes; psRes; psRes = psNRes) {
			if (psRes->usage == 0) {
				debug(LOG_WZ, "%s resource: %s(%04x) not used", psT->aType, psRes->aID,
				      psRes->HashedID);
			}
			if(psT->release != NULL) {
				psT->release( resGetResDataPointer(psRes) );
			} else {
				ASSERT( FALSE,"resReleaseAllData: NULL release function" );
			}

			psNRes = psRes->psNext;
			FREE(psRes);
		}
		psT->psRes = NULL;
		psNT = resNextType(psT);
	}
}
