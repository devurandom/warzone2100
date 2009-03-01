#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "mapload.h"

#define MAX_PLAYERS	8
#define MAP_MAXAREA	(256 * 256)
#define debug(z, ...) do { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\n"); } while (0)

void mapFree(GAMEMAP *map)
{
	free(map->psMapTiles);
	free(map);
}

/* Initialise the map structure */
GAMEMAP *mapLoad(char *filename)
{
	char		path[PATH_MAX];
	GAMEMAP		*map = malloc(sizeof(*map));
	uint32_t	gwVersion, i, j, gameVersion, gameTime, gameType, featVersion;
	char		aFileType[4];
	bool		littleEndian = true;
	PHYSFS_file	*fp = NULL;

	// this cries out for a class based design
	#define readU8(v) ( littleEndian ? PHYSFS_readULE8(fp, v) : PHYSFS_readUBE8(fp, v) )
	#define readU16(v) ( littleEndian ? PHYSFS_readULE16(fp, v) : PHYSFS_readUBE16(fp, v) )
	#define readU32(v) ( littleEndian ? PHYSFS_readULE32(fp, v) : PHYSFS_readUBE32(fp, v) )
	#define readS8(v) ( littleEndian ? PHYSFS_readSLE8(fp, v) : PHYSFS_readSBE8(fp, v) )
	#define readS16(v) ( littleEndian ? PHYSFS_readSLE16(fp, v) : PHYSFS_readSBE16(fp, v) )
	#define readS32(v) ( littleEndian ? PHYSFS_readSLE32(fp, v) : PHYSFS_readSBE32(fp, v) )

	/* === Load map data === */

	strcpy(path, filename);
	strcat(path, "/game.map");
	fp = PHYSFS_openRead(path);

	if (!fp)
	{
		debug(LOG_ERROR, "Map file %s not found", path);
		return NULL;
	}
	else if (PHYSFS_read(fp, aFileType, 4, 1) != 1
	    || !readU32(&map->version)
	    || !readU32(&map->width)
	    || !readU32(&map->height)
	    || aFileType[0] != 'm'
	    || aFileType[1] != 'a'
	    || aFileType[2] != 'p')
	{
		debug(LOG_ERROR, "Bad header in %s", filename);
		return NULL;
	}
	else if (map->version <= 9)
	{
		debug(LOG_ERROR, "%s: Unsupported save format version %u", filename, map->version);
		return NULL;
	}
	else if (map->version > 36)
	{
		debug(LOG_ERROR, "%s: Undefined save format version %u", filename, map->version);
		return NULL;
	}
	else if (map->width * map->height > MAP_MAXAREA)
	{
		debug(LOG_ERROR, "Map %s too large : %d %d", filename, map->width, map->height);
		return NULL;
	}

	/* Allocate the memory for the map */
	map->psMapTiles = calloc(map->width * map->height, sizeof(*map->psMapTiles));
	if (!map->psMapTiles)
	{
		debug(LOG_ERROR, "Out of memory");
		return NULL;
	}
	
	/* Load in the map data */
	for (i = 0; i < map->width * map->height; i++)
	{
		uint16_t	texture;
		uint8_t		height;

		if (!readU16(&texture) || !readU8(&height))
		{
			debug(LOG_ERROR, "%s: Error during savegame load", filename);
			return NULL;
		}

		map->psMapTiles[i].texture = texture;
		map->psMapTiles[i].height = height;
		for (j = 0; j < MAX_PLAYERS; j++)
		{
			map->psMapTiles[i].tileVisBits = (uint8_t)(map->psMapTiles[i].tileVisBits &~ (uint8_t)(1 << j));
		}
	}

	if (!readU32(&gwVersion) || !readU32(&map->numGateways) || gwVersion != 1)
	{
		debug(LOG_ERROR, "Bad gateway in %s", filename);
		return NULL;
	}

	for (i = 0; i < map->numGateways; i++)
	{
		uint8_t		x0, y0, x1, y1;

		if (!readU8(&x0) || !readU8(&y0) || !readU8(&x1) || !readU8(&y1))
		{
			debug(LOG_ERROR, "%s: Failed to read gateway info", filename);
			return NULL;
		}
	}
	PHYSFS_close(fp);


	/* === Load game data === */

	strcpy(path, filename);
	strcat(path, ".gam");
	fp = PHYSFS_openRead(path);
	if (!fp)
	{
		debug(LOG_ERROR, "Game file %s not found", path);
		return NULL;
	}
	else if (PHYSFS_read(fp, aFileType, 4, 1) != 1
	    || aFileType[0] != 'g'
	    || aFileType[1] != 'a'
	    || aFileType[2] != 'm'
	    || aFileType[3] != 'e'
	    || !readU32(&gameVersion))
	{
		debug(LOG_ERROR, "Bad header in %s", path);
		return NULL;
	}
	if (gameVersion > 35)	// big-endian
	{
		littleEndian = false;
	}
	if (!readU32(&gameTime)
	    || !readU32(&gameType)
	    || !readS32(&map->scrollMinX)
	    || !readS32(&map->scrollMinY)
	    || !readU32(&map->scrollMaxX)
	    || !readU32(&map->scrollMaxY)
	    || PHYSFS_read(fp, map->levelName, 20, 1) != 1)
	{
		debug(LOG_ERROR, "Bad data in %s", filename);
		return NULL;
	}
	PHYSFS_close(fp);


	/* === Load game data === */

	littleEndian = true;
	strcpy(path, filename);
	strcat(path, "/feat.bjo");
	fp = PHYSFS_openRead(path);
	if (!fp)
	{
		debug(LOG_ERROR, "Feature file %s not found", path);
		return NULL;
	}
	else if (PHYSFS_read(fp, aFileType, 4, 1) != 1
	    || aFileType[0] != 'f'
	    || aFileType[1] != 'e'
	    || aFileType[2] != 'a'
	    || aFileType[3] != 't'
	    || !readU32(&featVersion)
	    || !readU32(&map->numFeatures))
	{
		debug(LOG_ERROR, "Bad features header in %s", path);
		return NULL;
	}
	PHYSFS_close(fp);

	return map;
}
