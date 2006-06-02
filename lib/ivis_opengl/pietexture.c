/***************************************************************************/
/*
 * pieState.c
 *
 * renderer setup and state control routines for 3D rendering
 *
 */
/***************************************************************************/

#include "lib/framework/frame.h"
#include "pietexture.h"
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/tex.h"

/***************************************************************************/
/*
 *	Local Definitions
 */
/***************************************************************************/

typedef struct _textureState
{
	UDWORD	lastPageDownloaded;
	UDWORD	texPage;
} TEXTURE_STATE;

/***************************************************************************/
/*
 *	Local Variables
 */
/***************************************************************************/

TEXTURE_STATE	textureStates;

/***************************************************************************/
/*
 *	Local ProtoTypes
 */
/***************************************************************************/

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

BOOL pie_Download8bitTexturePage(void* bitmap,UWORD Width,UWORD Height)
{
	return TRUE;
}

BOOL pie_Reload8bitTexturePage(void* bitmap,UWORD Width,UWORD Height, SDWORD index)
{
//	return dtm_ReLoadTexture(index);
	return FALSE;
}

UDWORD pie_GetLastPageDownloaded(void)
{
	return _TEX_INDEX;
}
