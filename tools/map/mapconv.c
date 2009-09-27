// Converter from old Warzone (savegame) map format to new format.

// gcc -o ~/bin/mapconv mapconv.c mapload.c pngsave.c -I. -lphysfs -g -I../../lib/framework -lpng -Wall

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "pngsave.h"
#include "mapload.h"

#define GRDLANDVERSION	4
#define	ELEVATION_SCALE	2
#define GRAVITY		1
#define SEALEVEL	0
#define TILE_NUMMASK	0x01ff
#define TILE_XFLIP	0x8000
#define TILE_YFLIP	0x4000
#define TILE_ROTMASK	0x3000
#define TILE_ROTSHIFT	12
#define TILE_TRIFLIP	0x0800
#define TRI_FLIPPED(x)	((x)->texture & TILE_TRIFLIP)
#define SNAP_MODE	0

static const char *tilesetTextures[] = { "Arizona", "Urban", "Rockies" };

int main(int argc, char **argv)
{
	char filename[PATH_MAX], base[PATH_MAX];
	char path[PATH_MAX], *delim;
	GAMEMAP *map;
	FILE *fp;
	uint16_t *terrain, *rotate;
	uint8_t *height, *flip;
	int i;
	MAPTILE *psTile;

	if (argc != 2)
	{
		printf("Usage: %s <map>\n", argv[0]);
		return -1;
	}
	strcpy(path, argv[1]);
	delim = strrchr(path, '/');
	if (delim)
	{
		*delim = '\0';
		delim++;
		strcpy(filename, delim);
	}
	else
	{
		path[1] = '.';	// TODO FIXME this must be bugged...
		path[1] = '\0';
		strcpy(filename, argv[1]);
	}
	PHYSFS_init(argv[0]);
	PHYSFS_addToSearchPath(path, 1);

	map = mapLoad(filename);
	if (!map)
	{
		fprintf(stderr, "Failed to load map\n");
		return -1;
	}

	mkdir(filename, 0777);
	strcpy(base, filename);
	strcat(filename, "/map-001");	
	mkdir(filename, 0777);

	/*** Map configuration ***/
	strcat(filename, "/map.ini");
	fp = fopen(filename, "w");
	if (!fp)
	{
		fprintf(stderr, "Could not open target: %s", filename);
		return -1;
	}
	#define MADD(...) fprintf(fp, __VA_ARGS__); fprintf(fp, "\n");
	MADD("[map]");
	if (map->levelName[0] != '\0')
	{
		MADD("Name = %s", map->levelName);
	}
	MADD("SnapMode = %d", SNAP_MODE);
	MADD("Gravity = %d", GRAVITY);
	MADD("HeightScale = %d", ELEVATION_SCALE);
	MADD("MapWidth = %d", map->width);
	MADD("MapHeight = %d", map->height);
	MADD("TileWidth = %d", TILE_HEIGHT);
	MADD("TileHeight = %d", TILE_WIDTH);
	MADD("SeaLevel = %d", SEALEVEL);
	MADD("Tileset = %s", tilesetTextures[map->tileset]);

	MADD("\n[scroll_limits]");
	MADD("x1 = %d", map->scrollMinX);
	MADD("y1 = %d", map->scrollMinY);
	MADD("x2 = %u", map->scrollMaxX);
	MADD("y2 = %u", map->scrollMaxY);
	fclose(fp);

	/*** Terrain data ***/
	terrain = malloc(map->width * map->height * 2);
	height = malloc(map->width * map->height);
	rotate = malloc(map->width * map->height * 2);
	flip = malloc(map->width * map->height);
	psTile = mapTile(map, 0, 0);
	for (i = 0; i < map->width * map->height; i++)
	{
		height[i] = psTile->height;
		terrain[i] = psTile->texture & TILE_NUMMASK;
		rotate[i] = ((psTile->texture & TILE_ROTMASK) >> TILE_ROTSHIFT) * 90;
		flip[i] = TRI_FLIPPED(psTile) ? 255 : 0;

		psTile++;
	}
	strcpy(filename, base);
	strcat(filename, "/terrain.png");
	savePngI16(filename, terrain, map->width, map->height);
	strcpy(filename, base);
	strcat(filename, "/height.png");
	savePngI8(filename, height, map->width, map->height);
	strcpy(filename, base);
	strcat(filename, "/rotations.png");
	savePngI16(filename, rotate, map->width, map->height);
	strcpy(filename, base);
	strcat(filename, "/flips.png");
	savePngI8(filename, flip, map->width, map->height);
	free(height);
	free(terrain);
	free(rotate);
	free(flip);

	/*** Features ***/
	strcpy(filename, base);
	strcat(filename, "/features.ini");
	fp = fopen(filename, "w");
	MADD("[feature_header]");
	MADD("entries = %u", map->numFeatures);
	for (i = 0; i < map->numDroids; i++)
	{
		LND_OBJECT *psObj = &map->mLndObjects[IMD_FEATURE][i];

		MADD("\n[feature_%04u]", i);
		MADD("pos.x = %u", psObj->x);
		MADD("pos.y = %u", psObj->y);
		MADD("pos.z = %u", psObj->z);
		MADD("direction = %u", psObj->direction);
		MADD("player = %u", psObj->player);
		MADD("template = %s", psObj->name);
	}
	fclose(fp);

	/*** Structures ***/
	strcpy(filename, base);
	strcat(filename, "/structure.ini");
	fp = fopen(filename, "w");
	MADD("[structure_header]");
	MADD("entries = %u", map->numStructures);
	for (i = 0; i < map->numDroids; i++)
	{
		LND_OBJECT *psObj = &map->mLndObjects[IMD_STRUCTURE][i];

		MADD("\n[structure_%04u]", i);
		MADD("pos.x = %u", psObj->x);
		MADD("pos.y = %u", psObj->y);
		MADD("pos.z = %u", psObj->z);
		MADD("direction = %u", psObj->direction);
		MADD("player = %u", psObj->player);
		MADD("template = %s", psObj->name);
	}
	fclose(fp);

	/*** Droids ***/
	strcpy(filename, base);
	strcat(filename, "/droids.ini");
	fp = fopen(filename, "w");
	MADD("[droid_header]");
	MADD("entries = %u", map->numDroids);
	for (i = 0; i < map->numDroids; i++)
	{
		LND_OBJECT *psObj = &map->mLndObjects[IMD_DROID][i];

		MADD("\n[droid_%04u]", i);
		MADD("pos.x = %u", psObj->x);
		MADD("pos.y = %u", psObj->y);
		MADD("pos.z = %u", psObj->z);
		MADD("direction = %u", psObj->direction);
		MADD("player = %u", psObj->player);
		MADD("template = %s", psObj->name);
	}
	fclose(fp);

	/*** Gateways ***/
	strcpy(filename, base);
	strcat(filename, "/gateways.ini");
	fp = fopen(filename, "w");
	MADD("[gateway_header]");
	MADD("entries = %u", map->numGateways);
	for (i = 0; i < map->numGateways; i++)
	{
		GATEWAY *psGate = mapGateway(map, i);

		MADD("\n[gateway_%04d]", i);
		MADD("x1=%hhu", psGate->x1);
		MADD("y1=%hhu",	psGate->y1);
		MADD("x2=%hhu",	psGate->x2);
		MADD("y2=%hhu",	psGate->y2);
	}

	mapFree(map);

	return 0;
}
