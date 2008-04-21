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
	Edit3D.c - to ultimately contain the map editing functions -
	they are presently scattered in various files .
	Alex McLean, Pumpkin Studios, EIDOS Interactive, 1997
	*/

#include "lib/framework/frame.h"
#include "map.h"
#include "edit3d.h"
#include "display3d.h"
#include "objects.h"
#include "display.h"
#include "hci.h"

/*
Definition of a tile to highlight - presently more than is required
but means that we can highlight any individual tile in future. An
x coordinate that is greater than mapWidth implies that the highlight
is invalid (not currently being used)
*/

UDWORD	buildState = BUILD3D_NONE;
BUILDDETAILS	sBuildDetails;
HIGHLIGHT		buildSite;

// Initialisation function for statis & globals in this module.
//
void Edit3DInitVars(void)
{
	buildState = BUILD3D_NONE;
}

/* Raises a tile by a #defined height */
void raiseTile(int tile3dX, int tile3dY)
{
	MAPTILE	*psTile;

	if (tile3dX < 0 || tile3dX > mapWidth - 1 || tile3dY < 0 || tile3dY > mapHeight - 1)
	{
		return;
	}

	psTile = mapTile(tile3dX,tile3dY);
	adjustTileHeight(psTile,TILE_RAISE);

	psTile = mapTile(tile3dX+1,tile3dY);
	adjustTileHeight(psTile,TILE_RAISE);

	psTile = mapTile(tile3dX+1,tile3dY+1);
	adjustTileHeight(psTile,TILE_RAISE);

	psTile = mapTile(tile3dX,tile3dY+1);
	adjustTileHeight(psTile,TILE_RAISE);
}

/* Lowers a tile by a #defined height */
void lowerTile(int tile3dX, int tile3dY)
{
	MAPTILE	*psTile;

	if (tile3dX < 0 || tile3dX > mapWidth - 1 || tile3dY < 0 || tile3dY > mapHeight - 1)
	{
		return;
	}

	psTile = mapTile(tile3dX,tile3dY);
	adjustTileHeight(psTile,TILE_LOWER);

	psTile = mapTile(tile3dX+1,tile3dY);
	adjustTileHeight(psTile,TILE_LOWER);

	psTile = mapTile(tile3dX+1,tile3dY+1);
	adjustTileHeight(psTile,TILE_LOWER);

	psTile = mapTile(tile3dX,tile3dY+1);
	adjustTileHeight(psTile,TILE_LOWER);
}

/* Ensures any adjustment to tile elevation is within allowed ranges */
void	adjustTileHeight(MAPTILE *psTile, SDWORD adjust)
{
	SDWORD	newHeight;

	newHeight = psTile->height + adjust;
	if (newHeight>=MIN_TILE_HEIGHT && newHeight<=MAX_TILE_HEIGHT)
	{
		psTile->height=(unsigned char) newHeight;
	}
}

BOOL	inHighlight(UDWORD realX, UDWORD realY)
{
	BOOL	retVal = false;

	if (realX>=buildSite.xTL && realX<=buildSite.xBR)
	{
		if (realY>=buildSite.yTL && realY<=buildSite.yBR)
		{
			retVal = true;
		}
	}

	return(retVal);
}

void init3DBuilding(BASE_STATS *psStats,BUILDCALLBACK CallBack,void *UserData)
{
	ASSERT(psStats, "Bad parameter");
	if (!psStats)
	{
		return;
	}

	buildState = BUILD3D_POS;

	sBuildDetails.CallBack = CallBack;
	sBuildDetails.UserData = UserData;
	sBuildDetails.x = mouseTileX;
	sBuildDetails.y = mouseTileY;

	if (psStats->ref >= REF_STRUCTURE_START &&
		psStats->ref < (REF_STRUCTURE_START + REF_RANGE))
	{
		sBuildDetails.width = ((STRUCTURE_STATS *)psStats)->baseWidth;
		sBuildDetails.height = ((STRUCTURE_STATS *)psStats)->baseBreadth;
		sBuildDetails.psStats = psStats;

		// hack to increase the size of repair facilities
		if (((STRUCTURE_STATS *)psStats)->type == REF_REPAIR_FACILITY)
		{
			sBuildDetails.width += 2;
			sBuildDetails.height += 2;
		}
	}
	else if (psStats->ref >= REF_FEATURE_START &&
			 psStats->ref < (REF_FEATURE_START + REF_RANGE))
	{
		sBuildDetails.width = ((FEATURE_STATS *)psStats)->baseWidth;
		sBuildDetails.height = ((FEATURE_STATS *)psStats)->baseBreadth;
		sBuildDetails.psStats = psStats;
	}
	else /*if (psStats->ref >= REF_TEMPLATE_START &&
			 psStats->ref < (REF_TEMPLATE_START + REF_RANGE))*/
	{
		sBuildDetails.width = 1;
		sBuildDetails.height = 1;
		sBuildDetails.psStats = psStats;
	}
}

void	kill3DBuilding		( void )
{
	CancelDeliveryRepos();
    //cancel the drag boxes
    dragBox3D.status = DRAG_INACTIVE;
    wallDrag.status = DRAG_INACTIVE;
	buildState = BUILD3D_NONE;
}


// Call once per frame to handle structure positioning and callbacks.
//
BOOL process3DBuilding(void)
{
	UDWORD	bX,bY;

	//if not trying to build ignore
	if (buildState == BUILD3D_NONE)
  	{
		return true;
	}


	if (buildState != BUILD3D_FINISHED)// && buildState != BUILD3D_NONE)
  	{
		bX = mouseTileX;
		bY = mouseTileY;
		// lovely hack to make the repair facility 3x3 - need to offset the position by 1
		if (((STRUCTURE_STATS *)sBuildDetails.psStats)->type == REF_REPAIR_FACILITY)
		{
			bX += 1;
			bY += 1;
		}

      	if (validLocation(sBuildDetails.psStats, bX, bY, selectedPlayer, true))
        {
  		   	buildState = BUILD3D_VALID;
        }
  		else
  		{
  			buildState = BUILD3D_POS;
		}
  	}

	/* Need to update the building locations if we're building */
	bX = mouseTileX;
	bY = mouseTileY;

	if(mouseTileX<2)
	{
		bX = 2;
	}
	else
	{
		bX = mouseTileX;
	}
	if(mouseTileX > (SDWORD)(mapWidth-3))
	{
		bX = mapWidth-3;
	}
	else
	{
		bX = mouseTileX;
	}

	if(mouseTileY<2)
	{
		bY = 2;
	}
	else
	{
		bY = mouseTileY;
	}
	if(mouseTileY > (SDWORD)(mapHeight-3))
	{
		bY = mapHeight-3;
	}
	else
	{
		bY = mouseTileY;
	}

	sBuildDetails.x = buildSite.xTL = (UWORD)bX;
 	sBuildDetails.y = buildSite.yTL = (UWORD)bY;
	buildSite.xBR = (UWORD)(buildSite.xTL+sBuildDetails.width-1);
  	buildSite.yBR = (UWORD)(buildSite.yTL+sBuildDetails.height-1);

	if( (buildState == BUILD3D_FINISHED) && (sBuildDetails.CallBack != NULL) )
	{
		sBuildDetails.CallBack(sBuildDetails.x,sBuildDetails.y,sBuildDetails.UserData);
		buildState = BUILD3D_NONE;
		return true;
	}

	return false;
}


/* See if a structure location has been found */
BOOL found3DBuilding(UDWORD *x, UDWORD *y)
{
	if (buildState != BUILD3D_FINISHED || x == NULL || y == NULL)
	{
		return false;
	}

	*x = sBuildDetails.x;
	*y = sBuildDetails.y;

	// lovely hack to make the repair facility 3x3 - need to offset the position by 1
	if (((STRUCTURE_STATS *)sBuildDetails.psStats)->type == REF_REPAIR_FACILITY)
	{
		*x += 1;
		*y += 1;
	}

	buildState = BUILD3D_NONE;

	return true;
}

/* See if a second position for a build has been found */
BOOL found3DBuildLocTwo(UDWORD *px1, UDWORD *py1, UDWORD *px2, UDWORD *py2)
{
	if ( (((STRUCTURE_STATS *)sBuildDetails.psStats)->type != REF_WALL &&
		  ((STRUCTURE_STATS *)sBuildDetails.psStats)->type != REF_DEFENSE) ||
		wallDrag.status != DRAG_RELEASED)
	{
		return false;
	}

    //whilst we're still looking for a valid location - return false
    if (buildState == BUILD3D_POS)
    {
        return false;
    }

	wallDrag.status = DRAG_INACTIVE;
	*px1 = wallDrag.x1;
	*py1 = wallDrag.y1;
	*px2 = wallDrag.x2;
	*py2 = wallDrag.y2;
	return true;
}

/*returns true if the build state is not equal to BUILD3D_NONE*/
BOOL tryingToGetLocation(void)
{
    if (buildState == BUILD3D_NONE)
    {
        return false;
    }
    else
    {
        return true;
    }
}
