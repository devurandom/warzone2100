/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
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

#ifndef __INCLUDED_TOOLS_MAPLOAD_H__
#define __INCLUDED_TOOLS_MAPLOAD_H__

#include <stdint.h>
#include <stdbool.h>
#include <physfs.h>

static inline bool PHYSFS_writeSLE8(PHYSFS_file* file, int8_t val)
{
	return (PHYSFS_write(file, &val, sizeof(int8_t), 1) == 1);
}

static inline bool PHYSFS_writeULE8(PHYSFS_file* file, uint8_t val)
{
	return (PHYSFS_write(file, &val, sizeof(uint8_t), 1) == 1);
}

static inline bool PHYSFS_readSLE8(PHYSFS_file* file, int8_t* val)
{
	return (PHYSFS_read(file, val, sizeof(int8_t), 1) == 1);
}

static inline bool PHYSFS_readULE8(PHYSFS_file* file, uint8_t* val)
{
	return (PHYSFS_read(file, val, sizeof(uint8_t), 1) == 1);
}

/* Information stored with each tile */
typedef struct _maptile_type
{
	uint8_t			tileInfoBits;
	uint8_t			tileVisBits;	// COMPRESSED - bit per player
	uint8_t			height;			// The height at the top left of the tile
	uint8_t			illumination;	// How bright is this tile?
	uint32_t		texture;		// Which graphics texture is on this tile
	float			level;
} MAPTILE;

typedef struct _mapfile_type
{
	MAPTILE			*psMapTiles;
	uint32_t		height, width, version, numGateways;
	int32_t			scrollMinX;
	int32_t			scrollMinY;
	uint32_t		scrollMaxX;
	uint32_t		scrollMaxY;
} GAMEMAP;

/* Load the map data */
GAMEMAP *mapLoad(char *filename);
void mapFree(GAMEMAP *map);

#endif
