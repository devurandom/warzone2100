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
/***************************************************************************/
/*
 * pieBlitFunc.c
 *
 * patch for exisitng ivis rectangle draw functions.
 *
 */
/***************************************************************************/

#include "lib/ivis_opengl/GLee.h"
#include "lib/framework/frame.h"
#include <time.h>

#include "lib/ivis_common/pieblitfunc.h"
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/piemode.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/pieclip.h"
#include "lib/ivis_common/piefunc.h"
#include "lib/ivis_common/piepalette.h"
#include "lib/ivis_common/tex.h"
#include "piematrix.h"
#include "screen.h"

/***************************************************************************/
/*
 *	Local Variables
 */
/***************************************************************************/

#define pie_FILLRED	 16
#define pie_FILLGREEN	 16
#define pie_FILLBLUE	128
#define pie_FILLTRANS	128

static GLuint radarTexture;
static GLuint radarSizeX, radarSizeY;
static GLfloat radarTexX, radarTexY;

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

void pie_Line(int x0, int y0, int x1, int y1, PIELIGHT colour)
{
	pie_SetTexturePage(TEXPAGE_NONE);
	pie_SetAlphaTest(false);

	glColor4ubv(colour.vector);
	glBegin(GL_LINE_STRIP);
	glVertex2f(x0, y0);
	glVertex2f(x1, y1);
	glEnd();
}

/**
 *	Assumes render mode set up externally, draws filled rectangle.
 */
static void pie_DrawRect(float x0, float y0, float x1, float y1, PIELIGHT colour)
{
	pie_SetAlphaTest(false);

	glColor4ubv(colour.vector);
	glBegin(GL_TRIANGLE_STRIP);
		glVertex2i(x0, y0);
		glVertex2i(x1, y0);
		glVertex2i(x0, y1);
		glVertex2i(x1, y1);
	glEnd();
}


/***************************************************************************/

void pie_Box(int x0,int y0, int x1, int y1, PIELIGHT colour)
{
	pie_SetTexturePage(TEXPAGE_NONE);
	pie_SetAlphaTest(false);

	if (x0>psRendSurface->clip.right || x1<psRendSurface->clip.left ||
		y0>psRendSurface->clip.bottom || y1<psRendSurface->clip.top)
	{
		return;
	}

	if (x0<psRendSurface->clip.left)
		x0 = psRendSurface->clip.left;
	if (x1>psRendSurface->clip.right)
		x1 = psRendSurface->clip.right;
	if (y0<psRendSurface->clip.top)
		y0 = psRendSurface->clip.top;
	if (y1>psRendSurface->clip.bottom)
		y1 = psRendSurface->clip.bottom;

	glColor4ubv(colour.vector);
	glBegin(GL_LINE_STRIP);
	glVertex2f(x0, y0);
	glVertex2f(x1, y0);
	glVertex2f(x1, y1);
	glVertex2f(x0, y1);
	glVertex2f(x0, y0);
	glEnd();
}

/***************************************************************************/

void pie_BoxFill(int x0,int y0, int x1, int y1, PIELIGHT colour)
{
	pie_SetRendMode(REND_FLAT);
	pie_SetTexturePage(TEXPAGE_NONE);
	pie_DrawRect(x0, y0, x1, y1, colour);
}

/***************************************************************************/

void pie_TransBoxFill(float x0, float y0, float x1, float y1)
{
	PIELIGHT light;

	light.byte.r = pie_FILLRED;
	light.byte.g = pie_FILLGREEN;
	light.byte.b = pie_FILLBLUE;
	light.byte.a = pie_FILLTRANS;
	pie_UniTransBoxFill(x0, y0, x1, y1, light);
}

/***************************************************************************/

void pie_UniTransBoxFill(float x0, float y0, float x1, float y1, PIELIGHT light)
{
	pie_SetTexturePage(TEXPAGE_NONE);
	pie_SetRendMode(REND_ALPHA_FLAT);
	pie_DrawRect(x0, y0, x1, y1, light);
}

/***************************************************************************/

void pie_ImageFileID(IMAGEFILE *ImageFile, UWORD ID, int x, int y)
{
	IMAGEDEF *Image;
	PIEIMAGE pieImage;
	PIERECT dest;

	ASSERT_OR_RETURN(, ID < ImageFile->NumImages, "Out of range 1: %d", (int)ID);
	Image = &ImageFile->ImageDefs[ID];

	ASSERT_OR_RETURN(, Image->TPageID < MAX_NUM_TPAGEIDS, "Out of range 2: %d", (int)Image->TPageID);
	pie_SetRendMode(REND_ALPHA_TEX);
	pie_SetAlphaTest(true);

	pieImage.texPage = ImageFile->TPageIDs[Image->TPageID];
	pieImage.tu = Image->Tu;
	pieImage.tv = Image->Tv;
	pieImage.tw = Image->Width;
	pieImage.th = Image->Height;
	dest.x = x + Image->XOffset;
	dest.y = y + Image->YOffset;
	dest.w = Image->Width;
	dest.h = Image->Height;
	pie_DrawImage(&pieImage, &dest);
}

void pie_ImageFileIDTile(IMAGEFILE *ImageFile, UWORD ID, int x, int y, int Width, int Height)
{
	IMAGEDEF *Image;
	SDWORD hRep, hRemainder, vRep, vRemainder;
	PIEIMAGE pieImage;
	PIERECT dest;

	ASSERT_OR_RETURN(, ID < ImageFile->NumImages, "Out of range 1: %d", (int)ID);
	Image = &ImageFile->ImageDefs[ID];

	ASSERT_OR_RETURN(, Image->TPageID < MAX_NUM_TPAGEIDS, "Out of range 2: %d", (int)Image->TPageID);
	pie_SetRendMode(REND_GOURAUD_TEX);
	pie_SetAlphaTest(true);

	pieImage.texPage = ImageFile->TPageIDs[Image->TPageID];
	pieImage.tu = Image->Tu;
	pieImage.tv = Image->Tv;
	pieImage.tw = Image->Width;
	pieImage.th = Image->Height;

	dest.x = x + Image->XOffset;
	dest.y = y + Image->YOffset;
	dest.w = Image->Width;
	dest.h = Image->Height;

	vRemainder = Height % Image->Height;
	hRemainder = Width % Image->Width;

	for (vRep = 0; vRep < Height/Image->Height; vRep++)
	{
		pieImage.tw = Image->Width;
		dest.x = x + Image->XOffset;
		dest.w = Image->Width;

		for (hRep = 0; hRep < Width/Image->Width; hRep++)
		{
			pie_DrawImage(&pieImage, &dest);
			dest.x += Image->Width;
		}

		//draw remainder
		if (hRemainder > 0)
		{
			pieImage.tw = hRemainder;
			dest.w = hRemainder;
			pie_DrawImage(&pieImage, &dest);
		}

		dest.y += Image->Height;
	}

	//draw remainder
	if (vRemainder > 0)
	{
		//as above
		pieImage.tw = Image->Width;
		dest.x = x + Image->XOffset;
		dest.w = Image->Width;

		pieImage.th = vRemainder;
		dest.h = vRemainder;

		for (hRep = 0; hRep < Width/Image->Width; hRep++)
		{
			pie_DrawImage(&pieImage, &dest);
			dest.x += Image->Width;
		}

		//draw remainder
		if (hRemainder > 0)
		{
			pieImage.tw = hRemainder;
			dest.w = hRemainder;
			pie_DrawImage(&pieImage, &dest);
		}
	}
}

/* FIXME: WTF is this supposed to do? Looks like some other functionality
 * was retrofitted onto something else. - Per */
void pie_UploadDisplayBuffer()
{
	screen_Upload(NULL);
}

BOOL pie_InitRadar(void)
{
	radarTexture = _TEX_INDEX;
	glGenTextures(1, (GLuint *) &_TEX_PAGE[_TEX_INDEX].id);
	_TEX_INDEX++;
	return true;
}

BOOL pie_ShutdownRadar(void)
{
	glDeleteTextures(1, &_TEX_PAGE[radarTexture].id);
	return true;
}

/** Store radar texture with given width and height. */
void pie_DownLoadRadar(UDWORD *buffer, int width, int height)
{
	int w = 1, h = 1;

	/* Find power of two size */
	while (width > (w *= 2));
	while (height > (h *= 2));

	pie_SetTexturePage(radarTexture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	radarSizeX = width;
	radarSizeY = height;
	radarTexX = ((GLfloat)width / (GLfloat)w) * 256.0;
	radarTexY = ((GLfloat)height / (GLfloat)h) * 256.0;
}

/** Display radar texture using the given height and width, depending on zoom level. */
void pie_RenderRadar(int x, int y, int width, int height)
{
	pie_SetTexturePage(radarTexture);
	pie_SetRendMode(REND_GOURAUD_TEX);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glColor4ubv(WZCOL_WHITE.vector);
	glBegin(GL_TRIANGLE_STRIP);
		glTexCoord2f(0, 0);			glVertex2f(x, y);
		glTexCoord2f(radarTexX, 0);		glVertex2f(x + width, y);
		glTexCoord2f(0, radarTexY);		glVertex2f(x, y + height);
		glTexCoord2f(radarTexX, radarTexY);	glVertex2f(x + width, y + height);
	glEnd();
}

void pie_LoadBackDrop(SCREENTYPE screenType)
{
	char backd[128];

	//randomly load in a backdrop piccy.
	srand( (unsigned)time(NULL) + 17 ); // Use offset since time alone doesn't work very well

	switch (screenType)
	{
		case SCREEN_RANDOMBDROP:
			snprintf(backd, sizeof(backd), "texpages/bdrops/backdrop%i.png", rand() % 8); // Range: 0-7
			break;
		case SCREEN_MISSIONEND:
			sstrcpy(backd, "texpages/bdrops/missionend.png");
			break;

		case SCREEN_CREDITS:
		default:
			sstrcpy(backd, "texpages/bdrops/credits.png");
			break;
	}

	screen_SetBackDropFromFile(backd);
}
