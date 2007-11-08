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
/*
 * Animobj.c
 *
 * Anim object functions
 *
 * Gareth Jones 14/11/97
 */
/***************************************************************************/

#include <string.h>

#include "lib/framework/frame.h"
#include "hashtabl.h"
#include "gtime.h"
#include "animobj.h"

/***************************************************************************/

/* allocation sizes for anim object table */
#define	ANIM_OBJ_INIT		100
#define	ANIM_OBJ_EXT		20


/* max number of slots in hash table - prime numbers are best because hash
 * function used here is modulous of object pointer with table size -
 * prime number nearest 500 is 499.
 * Table of nearest primes in Binstock+Rex, "Practical Algorithms" p 91.
 */
#define	ANIM_HASH_TABLE_SIZE	499

/* Allocation sizes for the anim heap */
#define ANIM_INIT				10
#define ANIM_EXT				5

/***************************************************************************/
/* global variables */

static	HASHTABLE *g_pAnimObjTable = NULL;
static	ANIMOBJDIEDTESTFUNC g_pDiedFunc = NULL;

/***************************************************************************/
/* local functions */

static UDWORD		animObj_HashFunction(intptr_t iKey1, intptr_t iKey2);
static void		animObj_HashFreeElementFunc( void * psElement );

/***************************************************************************/
/*
 * Anim functions
 */
/***************************************************************************/

BOOL
animObj_Init( ANIMOBJDIEDTESTFUNC pDiedFunc )
{
	SDWORD	iSize = sizeof(ANIM_OBJECT);

	/* allocate hashtable */
	hashTable_Create( &g_pAnimObjTable, ANIM_HASH_TABLE_SIZE,
				ANIM_OBJ_INIT, ANIM_OBJ_EXT, iSize );

	/* set local hash table functions */
	hashTable_SetHashFunction( g_pAnimObjTable, animObj_HashFunction );
	hashTable_SetFreeElementFunction(g_pAnimObjTable, animObj_HashFreeElementFunc);

	/* set global died test function */
	g_pDiedFunc = pDiedFunc;

	return TRUE;
}

/***************************************************************************/

BOOL
animObj_Shutdown( void )
{
	/* destroy hash table */
	hashTable_Destroy( g_pAnimObjTable );
	g_pAnimObjTable = NULL;
	g_pDiedFunc = NULL;

	return TRUE;
}

/***************************************************************************/

void
animObj_SetDoneFunc( ANIM_OBJECT *psObj, ANIMOBJDONEFUNC pDoneFunc )
{
	psObj->pDoneFunc = pDoneFunc;
}

/***************************************************************************/
/*
 * animObj_HashFunction
 *
 * Takes object pointer and id and returns hashed index
 */
/***************************************************************************/

static UDWORD
animObj_HashFunction(intptr_t iKey1, intptr_t iKey2)
{
	return (iKey1 + iKey2)%ANIM_HASH_TABLE_SIZE;
}

/***************************************************************************/

static void
animObj_HashFreeElementFunc( void * psElement )
{
#ifdef DEBUG
	ANIM_OBJECT	*psObj = (ANIM_OBJECT *) psElement;

	ASSERT( psObj != NULL,
		"animObj_HashFreeElementFunc: object pointer invalid\n" );
#endif
}

/***************************************************************************/

void
animObj_Update( void )
{
	ANIM_OBJECT	*psObj;
	SDWORD		dwTime;
	BOOL		bRemove;

	psObj = (ANIM_OBJECT*)hashTable_GetFirst( g_pAnimObjTable );

	while ( psObj != NULL )
	{
		bRemove = FALSE;

		/* test whether parent object has died */
		if ( g_pDiedFunc != NULL )
		{
			bRemove = (g_pDiedFunc) (psObj->psParent);
		}

		/* remove any expired (non-looping) animations */
		if ( (bRemove == FALSE) && (psObj->uwCycles != 0) )
		{
			dwTime = gameTime - psObj->udwStartTime - psObj->udwStartDelay;

			if ( dwTime > (psObj->psAnim->uwAnimTime*psObj->uwCycles) )
			{
				/* fire callback if set */
				if ( psObj->pDoneFunc != NULL )
				{
					(psObj->pDoneFunc) (psObj);
				}

				bRemove = TRUE;
			}
		}

		/* remove object if flagged */
		if ( bRemove == TRUE )
		{
			if ( hashTable_RemoveElement( g_pAnimObjTable, psObj,
				(intptr_t) psObj->psParent, psObj->psAnim->uwID ) == FALSE )
			{
				debug( LOG_ERROR, "animObj_Update: couldn't remove anim obj\n" );
				abort();
			}
		}

		psObj = (ANIM_OBJECT*)hashTable_GetNext( g_pAnimObjTable );
	}
}

/***************************************************************************/
/*
 * anim_Add
 *
 * uwCycles=0 for infinite looping
 */
/***************************************************************************/

ANIM_OBJECT *
animObj_Add( void *pParentObj, int iAnimID,
				UDWORD udwStartDelay, UWORD uwCycles )
{
	ANIM_OBJECT		*psObj;
	BASEANIM		*psAnim = anim_GetAnim( (UWORD) iAnimID );
	UWORD			i, uwObj;

	ASSERT( psAnim != NULL,
			"anim_AddAnimObject: anim id %i not found\n", iAnimID );

	/* get object from table */
	psObj = (ANIM_OBJECT*)hashTable_GetElement( g_pAnimObjTable );

	if (psObj==NULL)
	{
		debug(LOG_ERROR, "animObj_Add: No room in hash table");
		return(NULL);
	}

	/* init object */
	psObj->uwID           = (UWORD) iAnimID;
	psObj->psAnim         = (ANIM3D *) psAnim;
	psObj->udwStartTime   = gameTime;
	psObj->udwStartDelay  = udwStartDelay;
	psObj->uwCycles       = uwCycles;
	psObj->bVisible       = TRUE;
	psObj->psParent       = pParentObj;
	psObj->pDoneFunc	  = NULL;

	/* allocate component objects */
	if ( psAnim->animType == ANIM_3D_TRANS )
	{
		uwObj = psAnim->uwObj;
	}
	else
	{
		uwObj = psAnim->uwStates;
	}

	if ( uwObj > ANIM_MAX_COMPONENTS )
	{
		debug( LOG_ERROR, "animObj_Add: number of components too small\n" );
		abort();
	}

	/* set parent pointer and shape pointer */
	for ( i=0; i<uwObj; i++ )
	{
		psObj->apComponents[i].psParent = pParentObj;
		psObj->apComponents[i].psShape  = psObj->psAnim->apFrame[i];
	}

	/* insert object in table by parent */
	hashTable_InsertElement(g_pAnimObjTable, psObj, (intptr_t) pParentObj, iAnimID);

	return psObj;
}


/***************************************************************************/

ANIM_OBJECT *
animObj_GetFirst( void )
{
	ANIM_OBJECT	*psObj;

	psObj = (ANIM_OBJECT *) hashTable_GetFirst( g_pAnimObjTable );

	return psObj;
}

/***************************************************************************/

ANIM_OBJECT *
animObj_GetNext( void )
{
	return (ANIM_OBJECT*)hashTable_GetNext( g_pAnimObjTable );
}

/***************************************************************************/

ANIM_OBJECT *
animObj_Find( void *pParentObj, int iAnimID )
{
	return (ANIM_OBJECT*)hashTable_FindElement(g_pAnimObjTable, (intptr_t) pParentObj, iAnimID);
}

/***************************************************************************/

BOOL
animObj_Remove( ANIM_OBJECT **ppsObj, int iAnimID )
{
	BOOL bRemOK = hashTable_RemoveElement(g_pAnimObjTable, *ppsObj, (intptr_t)(*ppsObj)->psParent, iAnimID);
    //init the animation
    *ppsObj = NULL;

    return bRemOK;
}

/***************************************************************************/
