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
#include "lib/framework/frame.h"

#include <SDL/SDL_opengl.h>
#ifdef __APPLE__
#include <opengl/glu.h>
#else
#include <GL/glu.h>
#endif

#include "lib/ivis_common/ivisdef.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/tex.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/piepalette.h"
#include "lib/ivis_common/ivispatch.h"
#include "lib/ivis_common/png_util.h"

#include "screen.h"

//*************************************************************************

iTexPage _TEX_PAGE[iV_TEX_MAX];
unsigned int _TEX_INDEX;

static void pie_PrintLoadedTextures(void);

//*************************************************************************


/**************************************************************************
	Add an image buffer given in s as a new texture page in the texture
	table.  We check first if the given image has already been loaded,
	as a sanity check (should never happen).  The texture numbers are
	stored in a special texture table, not in the resource system, for
	some unknown reason. Start looking for an available slot in the
	texture table at the given slot number.

	Returns the texture number of the image.
**************************************************************************/
int pie_AddTexPage(iV_Image *s, const char* filename, int slot)
{
	unsigned int i = 0;

	/* Have we already loaded this one? Should not happen here. */
	while (i < _TEX_INDEX)
	{
		if (strncmp(filename, _TEX_PAGE[i].name, iV_TEXNAME_MAX) == 0)
		{
			pie_PrintLoadedTextures();
		}
		ASSERT(strncmp(filename, _TEX_PAGE[i].name, iV_TEXNAME_MAX) != 0, 
		       "pie_AddTexPage: %s loaded again! Already loaded as %s|%u", filename,
		       _TEX_PAGE[i].name, i);
		i++;
	}

	/* Use first unused slot */
	for (i = slot; i < iV_TEX_MAX && _TEX_PAGE[i].name[0] != '\0'; i++);
	
	if (i == _TEX_INDEX)
	{
		_TEX_INDEX++; // increase table
	}
	ASSERT(i != iV_TEX_MAX, "pie_AddTexPage: too many texture pages");

	debug(LOG_TEXTURE, "pie_AddTexPage: %s page=%d", filename, _TEX_INDEX);
	assert(s != NULL);

	/* Stick the name into the tex page structures */
	strncpy(_TEX_PAGE[i].name, filename, iV_TEXNAME_MAX);

	glGenTextures(1, (GLuint *) &_TEX_PAGE[i].id);
	// FIXME: This function is used instead of glBindTexture, but we're juggling with difficult to trace global state here. Look into pie_SetTexturePage's definition for details.
	pie_SetTexturePage(i);

	if ((s->width & (s->width-1)) == 0 && (s->height & (s->height-1)) == 0)
	{
		gluBuild2DMipmaps(GL_TEXTURE_2D, wz_texture_compression, s->width, s->height, iV_getPixelFormat(s), GL_UNSIGNED_BYTE, s->bmp);
	} else {
		debug(LOG_ERROR, "pie_AddTexPage: non POT texture %s", filename);
	}
	free(s->bmp); // it is uploaded, we do not need it anymore
	s->bmp = NULL;

	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	// Use anisotropic filtering, if available, but only max 4.0 to reduce processor burden
	if (check_extension("GL_EXT_texture_filter_anisotropic"))
	{
		GLfloat max;

		glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &max);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, MIN(4.0f, max));
	}

	if( strncmp( filename, SKY_TEXPAGE, iV_TEXNAME_MAX ) == 0 )
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	}
	else
	{
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	}
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

	/* Send back the texpage number so we can store it in the IMD */

	_TEX_INDEX++;

	return i;
}

void pie_MakeTexPageName(char * filename)
{
	if (strncmp(filename, "page-", 5) == 0)
	{
		int i;
		for( i = 5; i < iV_TEXNAME_MAX-1 && isdigit(filename[i]); i++);
		filename[i] = '\0';
	}
}

/*!
 * Print the names of all loaded textures to LOG_ERROR
 */
static void pie_PrintLoadedTextures(void)
{
	unsigned int i = 0;

	debug(LOG_ERROR, "Available texture pages in memory (%d out of %d max):", _TEX_INDEX, iV_TEX_MAX);

	for ( i = 0; i < iV_TEX_MAX && _TEX_PAGE[i].name[0] != '\0'; i++ )
	{
		debug(LOG_ERROR, "%02d : %s", i, _TEX_PAGE[i].name);
	}
}


/**************************************************************************
	Return the texture number for a given texture resource.  We keep
	textures in a separate data structure _TEX_PAGE apart from the
	normal resource system.
**************************************************************************/
int iV_GetTexture(const char *filename)
{
	unsigned int i = 0;

	/* Have we already loaded this one then? */
	for ( i = 0; i < iV_TEX_MAX; i++ ) {
		if (strncmp(filename, _TEX_PAGE[i].name, iV_TEXNAME_MAX) == 0) {
			return i;
		}
	}

	/* This should never happen - by now all textures should have been loaded. */
	debug(LOG_ERROR, "*** texture %s not loaded! ***", filename);
	debug(LOG_ERROR, "This error probably means you did not specify this texture to be preloaded in the appropriate wrf files before referencing it in some pie file");
	debug(LOG_ERROR, "Remember that patches override several standard wrf files as well");

	pie_PrintLoadedTextures();

	return -1;
}


/**************************************************************************
	WRF files may specify overrides for the textures on a map. This
	is done through an ugly hack involving cutting the texture name
	down to just "page-NN", where NN is the page number, and 
	replaceing the texture page with the same name if another file
	with this prefix is loaded.
**************************************************************************/
int pie_ReplaceTexPage(iV_Image *s, const char *texPage)
{
	int i = iV_GetTexture(texPage);

	ASSERT(i >= 0, "pie_ReplaceTexPage: Cannot find any %s to replace!", texPage);
	if (i < 0)
	{
		return -1;
	}

	glDeleteTextures(1, (GLuint *) &_TEX_PAGE[i].id);
	debug(LOG_ERROR, "Reloading texture %s from index %d", texPage, i);
	_TEX_PAGE[i].name[0] = '\0';
	pie_AddTexPage(s, texPage, i);

	return i;
}

/*
	Alex - fixed this so it doesn't try to free up the memory if it got the page from resource
	handler - this is because the resource handler will deal with freeing it, and in all probability
	will have already done so by the time this is called, thus avoiding an 'already freed' moan.
*/
void pie_TexShutDown(void)
{
	unsigned int i = 0;

	while (i < _TEX_INDEX) 
	{
		glDeleteTextures(1, (GLuint *) &_TEX_PAGE[i].id);
		i++;
	}

	debug(LOG_TEXTURE, "pie_TexShutDown successful - did free %u texture pages", i);
}

void pie_TexInit(void)
{
	int i = 0;

	while (i < iV_TEX_MAX) {
		_TEX_PAGE[i].name[0] = '\0';
		i++;
	}
	debug(LOG_TEXTURE, "pie_TexInit successful - initialized %d texture pages\n", i);
}


void iV_unloadImage(iV_Image *image)
{
	if (image)
	{
		if (image->bmp)
		{
			free(image->bmp);
			image->bmp = NULL;
		}
	}
	else
	{
		debug(LOG_ERROR, "Tried to free invalid image!");
	}
}


unsigned int iV_getPixelFormat(const iV_Image *image)
{
	switch (image->depth)
	{
		case 3:
			return GL_RGB;
		case 4:
			return GL_RGBA;
		default:
			debug(LOG_ERROR, "iV_getPixelFormat: Unsupported image depth: %u", image->depth);
			return GL_INVALID_ENUM;
	}
}
