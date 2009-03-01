// Converter from Warzone (savegame) map format to Editworld LND format.
// Note that we are missing some information, most notably the height for
// the rightmost and bottom most vertices, which in Warzone is snapped to
// sea level, while it can be freely manipulated in Editworld.

// gcc -o ~/bin/map2lnd map2lnd.c mapload.c -I. -lphysfs -g

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "mapload.h"

#define GRDLANDVERSION	4
#define	ELEVATION_SCALE	2
#define GRAVITY		1
#define SEALEVEL	0
#define TILE_HEIGHT	128
#define TILE_WIDTH	128
#define TILE_NUMMASK	0x01ff
#define TILE_XFLIP	0x8000
#define TILE_YFLIP	0x4000
#define TILE_ROTMASK	0x3000
#define TILE_ROTSHIFT	12
#define TILE_TRIFLIP	0x0800
#define TRI_FLIPPED(x)	((x)->texture & TILE_TRIFLIP)
#define SNAP_MODE	0

typedef enum _tileset_type
{
	TILESET_ARIZONA = 0, TILESET_URBAN, TILESET_ROCKIES
} TILESET;

static const char *tilesetDataSet[] = { "WarzoneDataC1.eds", "WarzoneDataC2.eds", "WarzoneDataC3.eds" };
static const char *tilesetTextures[] = { "texpages\\tertilesc1.pcx", "texpages\\tertilesc2.pcx", "texpages\\tertilesc3.pcx" };

static MAPTILE *mapTile(GAMEMAP *map, int x, int y)
{
	return &map->psMapTiles[y * map->width + x];
}

int main(int argc, char **argv)
{
	char filename[PATH_MAX];
	char path[PATH_MAX], *delim;
	GAMEMAP *map;
	FILE *fp;
	int i, x, y;
	TILESET tileset = TILESET_ARIZONA; // FIXME, hack for now

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
		path[1] = '.';
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

	strcat(filename, ".lnd");
	fp = fopen(filename, "w");
	if (!fp)
	{
		fprintf(stderr, "Could not open target: %s", filename);
		return -1;
	}
	#define MADD(...) fprintf(fp, __VA_ARGS__); fprintf(fp, "\n");
	MADD("DataSet %s", tilesetDataSet[tileset]);
	MADD("GrdLand {");
	MADD("    Version %d", GRDLANDVERSION);
	MADD("    3DPosition %f %f %f", 0.0, 0.0, 0.0);	// FIXME
	MADD("    3DRotation %f %f %f", 0.0, 0.0, 0.0);	// FIXME
	MADD("    2DPosition %d %d", 0, 0);		// FIXME
	MADD("    CustomSnap %d %d", 0, 0);		// FIXME
	MADD("    SnapMode %d", SNAP_MODE);
	MADD("    Gravity %d", GRAVITY);
	MADD("    HeightScale %d", ELEVATION_SCALE);
	MADD("    MapWidth %d", map->width);
	MADD("    MapHeight %d", map->height);
	MADD("    TileWidth %d", TILE_HEIGHT);
	MADD("    TileHeight %d", TILE_WIDTH);
	MADD("    SeaLevel %d", SEALEVEL);
	MADD("    TextureWidth %d", 64);		// Hack for editworld
	MADD("    TextureHeight %d", 64);
	MADD("    NumTextures %d", 1);
	MADD("    Textures {");
	MADD("        %s", tilesetTextures[tileset]);
	MADD("    }");
	MADD("    NumTiles %d",  map->width * map->height);
	MADD("    Tiles {");
	for (i = 0, x = 0, y = 0; i < map->width * map->height; i++)
	{
		MAPTILE *psTile = mapTile(map, x, y);

		// Example: TID 1 VF 0 TF 0 F 0 VH 128 128 128 128
		// TID is texture identification
		// VF is triangle (vertex) flip. If value of one, it is flipped.
		// TF is tile or texture flip. If value of one, it is flipped in the X direction. Otherwise Y direction.
		// F are bitflags, with these values: 
		//	 2 : Triangle flip
		//	 4 : Texture flip X (yes, duplicate info)
		//	 8 : Texture flip Y (ditto)
		//	16 : Rotate 90 degrees
		//	32 : Rotate 180 degrees (yes, a 270 degree is possible)
		//	64 : Gateway?
		// VH is vertex height, and gives height of all the four vertices that make up our tile
		int tid = psTile->texture & TILE_NUMMASK;
		int vf = TRI_FLIPPED(psTile);
		int tf = (psTile->texture & TILE_XFLIP) > 0 ? 1 : 0;
		int f = 0;
		int vh[4];

		// Compose flag
		if (TRI_FLIPPED(psTile)) f += 2;
		if (psTile->texture & TILE_XFLIP) f += 4;
		if (psTile->texture & TILE_YFLIP) f += 8;
		switch ((psTile->texture & TILE_ROTMASK) >> TILE_ROTSHIFT)
		{
		case 0:		break;
		case 1:		f += 16; break;
		case 2:		f += 32; break;
		case 3:		f += 48; break;
		default:	fprintf(stderr, "Bad rotation value: %d\n", (psTile->texture & TILE_ROTMASK) >> TILE_ROTSHIFT); return -1;
		}

		// CHECK: Should these be multiplied by ELEVATION_SCALE?
		// CHECK: I am simply assuming counter-clockwise orientation here
		vh[0] = psTile->height;
		if (y + 1 < map->height)
		{
			vh[3] = mapTile(map, x, y + 1)->height;
		}
		else
		{
			vh[3] = 0;
		}

		if (x + 1 < map->width)
		{
			vh[1] = mapTile(map, x + 1, y)->height;
		}
		else
		{
			vh[1] = 0;
		}

		if (x + 1 < map->width && y + 1 < map->height)
		{
			vh[2] = mapTile(map, x + 1, y + 1)->height;
		}
		else
		{
			vh[2] = 0;
		}

		// No idea why +1 to TID. In EditWorld source, it is a "hide" flag.
		MADD("        TID %d VF %d TF %d F %d VH %d %d %d %d", 
		     tid + 1, vf, tf, f, vh[0], vh[1], vh[2], vh[3]);
		x++;
		if (x == map->width)
		{
			x = 0;
			y++;
		}
	}
	MADD("    }");
	MADD("}");
	MADD("ObjectList {");
	MADD("    Version 3");
	MADD("    FeatureSet %s", tilesetDataSet[tileset]);
	MADD("    NumObjects 0");
	MADD("    Objects {");
	MADD("    }");
	MADD("}");
	MADD("ScrollLimits {");
	MADD("    Version 1");
	MADD("    NumLimits 1");		// FIXME: do scroll limits go here?
	MADD("    Limits {");
	MADD("        \"Entire Map\" 0 0 0 %d %d", map->width, map->height);
	MADD("    }");
	MADD("}");
	MADD("Gateways {");
	MADD("    Version 1");
	MADD("    NumGateways 0");	// FIXME
	MADD("    Gates {");
	MADD("    }");
	MADD("}");
	MADD("TileTypes {");
	MADD("    NumTiles 128");		// ??? FIXME - read from ttypes file
	MADD("    Tiles {");
	switch (tileset)
	{
	case TILESET_ARIZONA:
		MADD("        2 1 0 2 2 0 2 2 2 2 1 1 1 0 7 7\n"
		     "        7 7 7 8 6 4 4 6 3 3 3 2 4 1 4 7\n"
		     "        7 7 7 4 4 2 2 2 2 1 4 0 4 4 8 8\n"
		     "        2 4 4 4 4 4 4 4 9 9 6 9 6 4 4 9\n"
		     "        9 9 9 9 9 9 9 9 8 4 4 4 8 5 6 2\n"
		     "        2 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
		break;
	case TILESET_URBAN:
		MADD("        2 2 2 2 2 1 2 2 1 1 1 1 1 1 7 7\n"
		     "        7 7 7 1 8 4 4 0 7 7 7 7 4 4 2 4\n"
		     "        0 2 0 0 2 4 4 0 4 6 2 6 6 6 6 6\n"
		     "        6 4 6 3 4 4 2 2 9 9 9 2 4 2 4 9\n"
		     "        9 9 9 9 8 8 8 8 4 2 0 4 4 2 2 2\n"
		     "        3 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
		break;
	case TILESET_ROCKIES:
		MADD("        2 0 0 2 2 2 2 2 2 1 8 11 2 11 6 7\n"
		     "        7 7 7 8 6 1 2 6 11 11 0 11 1 1 8 8\n"
		     "        7 7 7 0 0 1 8 0 4 5 11 8 5 8 8 8\n"
		     "        11 11 1 1 1 1 1 8 9 9 5 2 6 6 8 9\n"
		     "        8 10 10 11 11 8 8 10 8 2 10 0 10 8 8 8\n"
		     "        3 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n"
		     "        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
		break;
	}
	MADD("  }");
	MADD("}");
	MADD("TileFlags {");		// ??
	MADD("    NumTiles 128");
	MADD("    Flags {");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0");
	MADD("    }");
	MADD("}");
	MADD("Brushes {");
	MADD("    Version 2");
	MADD("    NumEdgeBrushes 16");
	MADD("    NumUserBrushes 0");
	MADD("    EdgeBrushes {");
	MADD("        0 128 0 -1 0 46 16 46 32 30 32 46 0 30 16 47 0 64 0 46 48 47 0 30 48 64 16 30 0 64 48 64 32 65 16");
	MADD("        0 128 0 -1 0 43 16 43 32 30 32 43 0 30 16 47 0 10 0 43 48 47 0 30 48 10 16 30 0 10 48 10 32 42 0");
	MADD("        0 128 0 -1 0 10 32 10 48 45 32 10 16 45 16 47 0 64 0 10 0 47 0 45 48 64 16 45 0 64 48 64 32 65 128");
	MADD("        0 128 0 -1 0 34 0 34 16 32 0 34 48 32 48 -1 0 33 0 34 32 -1 0 32 16 33 16 32 32 33 48 33 32 18 0");
	MADD("        0 128 0 -1 0 4 32 4 48 3 48 4 16 3 32 -1 0 5 32 4 0 -1 0 3 0 5 48 3 16 5 16 5 0 6 0");
	MADD("        0 128 0 -1 0 46 16 46 32 47 0 46 0 47 16 -1 0 19 0 46 48 -1 0 47 48 19 16 47 32 19 48 19 32 6 128");
	MADD("        0 128 0 -1 0 5 0 5 16 3 16 5 48 3 0 -1 0 4 0 5 32 -1 0 3 32 4 16 3 48 4 48 4 32 1 128");
	MADD("        0 128 0 -1 0 11 0 11 16 13 0 11 48 13 48 6 128 12 0 11 32 6 128 13 16 12 16 13 32 12 48 12 32 6 128");
	MADD("        0 128 0 -1 0 41 32 41 48 39 48 41 16 39 32 -1 0 40 32 41 0 -1 0 39 0 40 48 39 16 40 16 40 0 54 128");
	MADD("        0 128 0 -1 0 36 0 36 16 35 16 36 48 35 0 -1 0 37 0 36 32 -1 0 35 32 37 16 35 48 37 48 37 32 54 0");
	MADD("        0 128 0 -1 0 66 0 66 16 68 0 66 48 68 48 -1 0 67 0 66 32 -1 0 68 16 67 16 68 32 67 48 67 32 42 128");
	MADD("        0 128 0 -1 0 78 32 78 48 77 32 78 16 77 16 -1 0 55 32 78 0 -1 0 77 48 55 48 77 0 55 16 55 0 65 128");
	MADD("        0 128 0 -1 0 21 32 21 48 22 48 21 16 22 32 -1 0 20 32 21 0 -1 0 22 0 20 48 22 16 20 16 20 0 23 128");
	MADD("        0 128 0 -1 0 67 32 67 48 68 32 67 16 68 16 -1 0 66 32 67 0 -1 0 68 48 66 48 68 0 66 16 66 0 65 128");
	MADD("        0 128 0 -1 0 12 32 12 48 49 32 12 16 49 16 -1 0 11 32 12 0 -1 0 49 48 11 48 49 0 11 16 11 0 42 0");
	MADD("        0 128 0 -1 0 16 16 16 32 15 32 16 0 15 16 15 16 17 16 16 48 15 16 15 48 17 32 15 0 17 0 17 48 18 0");
	MADD("    }");
	MADD("}");

	fclose(fp);
	mapFree(map);

	return 0;
}
