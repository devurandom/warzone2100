#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "map/mapload.h"

int main(int argc, char **argv)
{
	char filename[PATH_MAX];
	FILE *fp = fopen("maplist.txt", "r");

	if (!fp)
	{
		fprintf(stderr, "maptest: Failed to open list file\n");
		return -1;
	}
	PHYSFS_init(argv[0]);
	PHYSFS_addToSearchPath("../data", 1);

	while (!feof(fp))
	{
		GAMEMAP *map;

		fscanf(fp, "%s\n", &filename);
		printf("Testing map: %s\n", filename);
		map = mapLoad(filename);
		if (!map)
		{
			fprintf(stderr, "maptest: Failed to load %s\n", filename);
			return -1;
		}
		mapFree(map);
	}
	fclose(fp);

	return 0;
}
