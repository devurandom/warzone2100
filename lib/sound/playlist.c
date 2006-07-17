#include <stdio.h>
#include <physfs.h>

#include "lib/framework/frame.h"

#define BUFFER_SIZE 2048
static char buffer[BUFFER_SIZE], ByteBuf='\0';
static unsigned int ByteBufPos=0;

#define NB_TRACKS 3

typedef struct {
	char**		songs;
	unsigned int	nb_songs;
	unsigned int	list_size;
	BOOL		shuffle;
} WZ_TRACK;

static unsigned int current_track = 0;
static unsigned int current_song = 0;

static WZ_TRACK playlist[NB_TRACKS];

#define CURRENT_TRACK playlist[current_track]

void PlayList_Init() {
	unsigned int i;

	for (i = 0; i < NB_TRACKS; ++i) {
		playlist[i].songs = malloc(2*sizeof(char*));
		playlist[i].list_size = 2;
		playlist[i].nb_songs = 0;
		memset( playlist[i].songs, 0, playlist[i].list_size*sizeof(char*) );
	}
}

void PlayList_Quit() {
	unsigned int i, j;

	for( i = 0; i < NB_TRACKS; ++i ) {
		for( j = 0; j < playlist[i].list_size; ++j ) {
			free( playlist[i].songs[j] );
		}
		free( playlist[i].songs );
	}
}

char PlayList_Read(const char* path) {
	PHYSFS_file * f;
	char* path_to_music = NULL;

	sprintf(buffer, "%s/music.wpl", path);

	f = PHYSFS_openRead(buffer);

	if (f == NULL) {
		return 1;
	}

	while (!PHYSFS_eof(f)) {
		char* filename;

		while( ByteBufPos < BUFFER_SIZE-1 && PHYSFS_read( f, &ByteBuf, 1, 1 ) && ByteBuf != '\n' )
		{
			buffer[ByteBufPos]=ByteBuf;
			ByteBufPos++;
		}
		buffer[ByteBufPos]='\0';
		ByteBufPos=0;

		if (strncmp(buffer, "[game]", 6) == 0) {
			current_track = 1;
			free(path_to_music);
			path_to_music = NULL;
			CURRENT_TRACK.shuffle = FALSE;
		} else if (strncmp(buffer, "[menu]", 6) == 0) {
			current_track = 2;
			free(path_to_music);
			path_to_music = NULL;
			CURRENT_TRACK.shuffle = FALSE;
		} else if (strncmp(buffer, "path=", 5) == 0) {
			free(path_to_music);
			path_to_music = strtok(buffer+5, "\n");
			if (strcmp(path_to_music, ".") == 0) {
				path_to_music = strdup(path);
			} else {
				path_to_music = strdup(path_to_music);
			}
			debug( LOG_WZ, "  path = %s\n", path_to_music );
		} else if (strncmp(buffer, "shuffle=", 8) == 0) {
			if (strcmp(strtok(buffer+8, "\n"), "yes") == 0) {
				CURRENT_TRACK.shuffle = TRUE;
			}
			debug( LOG_WZ, "  shuffle = yes\n" );
		} else if (   buffer[0] != '\0'
			   && (filename = strtok(buffer, "\n")) != NULL
			   && strlen(filename) != 0) {
			char* filepath;

			if (path_to_music == NULL) {
				filepath = malloc(strlen(filename)+1);
				sprintf(filepath, "%s", filename);
			} else {
				filepath = malloc(  strlen(filename)
						  + strlen(path_to_music)+2);
				sprintf(filepath, "%s/%s", path_to_music, filename);
			}
			debug( LOG_WZ, "  adding song %s\n", filepath );

			if (CURRENT_TRACK.nb_songs == CURRENT_TRACK.list_size) {
				CURRENT_TRACK.list_size <<= 1;
				CURRENT_TRACK.songs = realloc(CURRENT_TRACK.songs,
							      CURRENT_TRACK.list_size*sizeof(char*));
			}

			CURRENT_TRACK.songs[CURRENT_TRACK.nb_songs++] = filepath;
		}
	}

	free(path_to_music);

	PHYSFS_close(f);

	return 0;
}

void PlayList_Shuffle() {
	if (CURRENT_TRACK.shuffle) {
		unsigned int i;

		for (i = CURRENT_TRACK.nb_songs-1; i > 0; --i) {
			unsigned int j = rand() % (i + 1);
			char* swap = CURRENT_TRACK.songs[j];

			CURRENT_TRACK.songs[j] = CURRENT_TRACK.songs[i];
			CURRENT_TRACK.songs[i] = swap;
		}
	}
}

void PlayList_SetTrack(unsigned int t) {
	if (t >= 0 && t < NB_TRACKS) {
		current_track = t;
	} else {
		current_track = 0;
	}
	PlayList_Shuffle();
	current_song = 0;
}

char* PlayList_CurrentSong() {
	if (current_song >= CURRENT_TRACK.nb_songs) {
		return NULL;
	} else {
		return CURRENT_TRACK.songs[current_song];
	}
}

char* PlayList_NextSong() {
	if (++current_song >= CURRENT_TRACK.nb_songs) {
		PlayList_Shuffle();
		current_song = 0;
	}

	if (CURRENT_TRACK.nb_songs == 0) {
		return NULL;
	} else {
		return CURRENT_TRACK.songs[current_song];
	}
}

void PlayList_DeleteCurrentSong() {
}

