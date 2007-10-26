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

#include <stdlib.h>
#include <string.h>
#include "lib/ivis_common/ivisdef.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/pieclip.h"
#include "lib/ivis_common/pieblitfunc.h"
#include "lib/ivis_common/piepalette.h"
#include "lib/ivis_common/ivispatch.h"
#include "lib/ivis_common/textdraw.h"
#include "lib/ivis_common/bitimage.h"

#include <GL/gl.h>
#include <GL/glc.h>

static const char font_family[] = "DejaVu Sans Mono";
static const char font_face_regular[] = "Book";
static const char font_face_bold[] = "Bold";

static float font_size = 12.f;
// Contains the font color in the following order: red, green, blue, alpha
static float font_colour[4] = {1.f, 1.f, 1.f, 1.f};

static GLint GLC_Context = 0;
static GLint GLC_Font_Regular = 0;
static GLint GLC_Font_Bold = 0;

static bool anti_aliasing = true;

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

static inline void iV_printFontList()
{
	unsigned int i;
	GLint font_count = glcGeti(GLC_CURRENT_FONT_COUNT);
	debug(LOG_NEVER, "GLC_CURRENT_FONT_COUNT = %d", font_count);

	if (font_count == 0)
	{
		debug(LOG_ERROR, "iV_printFontList: The required font (%s) isn't loaded", font_family);

		// Fall back to unselected fonts since the requested font apparently
		// isn't available.
		glcEnable(GLC_AUTO_FONT);
	}

	for (i = 0; i < font_count; ++i)
	{
		GLint font = glcGetListi(GLC_CURRENT_FONT_LIST, i);
		/* The output of the family name and the face is printed using 2 steps
		 * because glcGetFontc and glcGetFontFace return their result in the
		 * same buffer (according to GLC specs).
		 */
		char prBuffer[1024];
		snprintf(prBuffer, sizeof(prBuffer), "Font #%d : %s ", font, (const char*)glcGetFontc(font, GLC_FAMILY));
		prBuffer[sizeof(prBuffer) - 1] = 0;
		strncat(prBuffer, glcGetFontFace(font), sizeof(prBuffer));
		prBuffer[sizeof(prBuffer) - 1] = 0;
		debug(LOG_NEVER, prBuffer);
	}
}

static void iV_initializeGLC()
{
	if (GLC_Context)
		return;

	GLC_Context = glcGenContext();
	if (!GLC_Context)
		debug(LOG_ERROR, "glcGenContext() failed");
	else
		debug(LOG_NEVER, "glcGenContext() succesful: GLC_Context = %d", GLC_Context);

	glcContext(GLC_Context);

	glcDisable(GLC_AUTO_FONT);
	glcRenderStyle(GLC_TRIANGLE);

	GLC_Font_Regular = glcGenFontID();
	GLC_Font_Bold = glcGenFontID();

	if (!glcNewFontFromFamily(GLC_Font_Regular, font_family))
			debug(LOG_ERROR, "glcNewFontFromFamily(GLC_Font_Regular (%d), \"%s\") failed", GLC_Font_Regular, font_family);
		else
			debug(LOG_NEVER, "glcNewFontFromFamily(GLC_Font_Regular (%d), \"%s\") succesful", GLC_Font_Regular, font_family);

	if (!glcFontFace(GLC_Font_Regular, font_face_regular))
			debug(LOG_ERROR, "glcFontFace(GLC_Font_Regular (%d), \"%s\") failed", GLC_Font_Regular, font_face_regular);
		else
			debug(LOG_NEVER, "glcFontFace(GLC_Font_Regular (%d), \"%s\") succesful", GLC_Font_Regular, font_face_regular);

	if (!glcNewFontFromFamily(GLC_Font_Bold, font_family))
			debug(LOG_ERROR, "glcNewFontFromFamily(GLC_Font_Bold (%d), \"%s\") failed", GLC_Font_Bold, font_family);
		else
			debug(LOG_NEVER, "glcNewFontFromFamily(GLC_Font_Bold (%d), \"%s\") succesful", GLC_Font_Bold, font_family);

	if (!glcFontFace(GLC_Font_Bold, font_face_bold))
			debug(LOG_ERROR, "glcFontFace(GLC_Font_Bold (%d), \"%s\") failed", GLC_Font_Bold, font_face_bold);
		else
			debug(LOG_NEVER, "glcFontFace(GLC_Font_Bold (%d), \"%s\") succesful", GLC_Font_Bold, font_face_bold);

	debug(LOG_NEVER, "finished initializing GLC");

	// Set GLC's string type to UTF-8
	glcStringType(GLC_UTF8_QSO);
}

void iV_TextInit()
{
	iV_initializeGLC();
	iV_SetFont(font_regular);

#ifdef DEBUG
	iV_printFontList();
#endif
}

void iV_TextShutdown()
{
	if (GLC_Font_Regular)
		glcDeleteFont(GLC_Font_Regular);

	if (GLC_Font_Bold)
		glcDeleteFont(GLC_Font_Bold);

	glcContext(0);

	if (GLC_Context)
		glcDeleteContext(GLC_Context);
}

void iV_SetTextAntialias(bool enable)
{
	anti_aliasing = enable;
}

bool iV_TextAntialiased()
{
	return anti_aliasing;
}

void iV_SetFont(enum iV_fonts FontID)
{
	switch (FontID)
	{
		case font_regular:
			iV_SetTextSize(12.f);
			glcFont(GLC_Font_Regular);
			break;

		case font_large:
			iV_SetTextSize(21.f);
			glcFont(GLC_Font_Bold);
			break;
	}
}

static inline float getGLCResolution()
{
	float resolution = glcGetf(GLC_RESOLUTION);

	// The default resolution as used by OpenGLC is 72 dpi
	if (resolution == 0.f)
		return 72.f;

	return resolution;
}

static inline float getGLCPixelSize()
{
	float pixel_size = font_size * getGLCResolution() / 72.f;
	return pixel_size;
}

static inline float getGLCPointWidth(const float* boundingbox)
{
	// boundingbox contains: [ xlb ylb xrb yrb xrt yrt xlt ylt ]
	// l = left; r = right; b = bottom; t = top;
	float rightTopX = boundingbox[4];
	float leftTopX = boundingbox[6];

	float point_width = rightTopX - leftTopX;

	return point_width;
}

static inline float getGLCPointHeight(const float* boundingbox)
{
	// boundingbox contains: [ xlb ylb xrb yrb xrt yrt xlt ylt ]
	// l = left; r = right; b = bottom; t = top;
	float leftBottomY = boundingbox[1];
	float leftTopY = boundingbox[7];

	float point_height = fabsf(leftTopY - leftBottomY);

	return point_height;
}

static inline float getGLCPointToPixel(float point_width)
{
	float pixel_width = point_width * getGLCPixelSize();

	return pixel_width;
}

unsigned int iV_GetTextWidth(const char* string)
{
	float boundingbox[8];
	float pixel_width, point_width;

	glcMeasureString(GL_FALSE, string);
	if (!glcGetStringMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetTextWidth: couldn't retrieve a bounding box for the string");
		return 0;
	}

	point_width = getGLCPointWidth(boundingbox);
	pixel_width = getGLCPointToPixel(point_width);
	return (unsigned int)pixel_width;
}

unsigned int iV_GetCountedTextWidth(const char* string, size_t string_length)
{
	float boundingbox[8];
	float pixel_width, point_width;

	glcMeasureCountedString(GL_FALSE, string_length, string);
	if (!glcGetStringMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetCountedTextWidth: couldn't retrieve a bounding box for the string");
		return 0;
	}

	point_width = getGLCPointWidth(boundingbox);
	pixel_width = getGLCPointToPixel(point_width);
	return (unsigned int)pixel_width;
}

unsigned int iV_GetTextHeight(const char* string)
{
	float boundingbox[8];
	float pixel_height, point_height;

	glcMeasureString(GL_FALSE, string);
	if (!glcGetStringMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetTextHeight: couldn't retrieve a bounding box for the string");
		return 0;
	}

	point_height = getGLCPointHeight(boundingbox);
	pixel_height = getGLCPointToPixel(point_height);
	return (unsigned int)pixel_height;
}

unsigned int iV_GetCharWidth(uint32_t charCode)
{
	float boundingbox[8];
	float pixel_width, point_width;

	if (!glcGetCharMetric(charCode, GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetCharWidth: couldn't retrieve a bounding box for the character");
		return 0;
	}

	point_width = getGLCPointWidth(boundingbox);
	pixel_width = getGLCPointToPixel(point_width);
	return (unsigned int)pixel_width;
}

int iV_GetTextLineSize()
{
	float boundingbox[8];
	float pixel_height, point_height;

	if (!glcGetMaxCharMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetTextLineSize: couldn't retrieve a bounding box for the character");
		return 0;
	}

	point_height = getGLCPointHeight(boundingbox);
	pixel_height = getGLCPointToPixel(point_height);
	return (unsigned int)pixel_height;
}

static float iV_GetMaxCharBaseY()
{
	float base_line[4]; // [ xl yl xr yr ]

	if (!glcGetMaxCharMetric(GLC_BASELINE, base_line))
	{
		debug(LOG_ERROR, "iV_GetMaxCharBaseY: couldn't retrieve the baseline for the character");
		return 0;
	}

	return base_line[1];
}

int iV_GetTextAboveBase(void)
{
	float point_base_y = iV_GetMaxCharBaseY();
	float point_top_y;
	float boundingbox[8];
	float pixel_height, point_height;

	if (!glcGetMaxCharMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetTextAboveBase: couldn't retrieve a bounding box for the character");
		return 0;
	}

	point_top_y = boundingbox[7];
	point_height = point_base_y - point_top_y;
	pixel_height = getGLCPointToPixel(point_height);
	return (int)pixel_height;
}

int iV_GetTextBelowBase(void)
{
	float point_base_y = iV_GetMaxCharBaseY();
	float point_bottom_y;
	float boundingbox[8];
	float pixel_height, point_height;

	if (!glcGetMaxCharMetric(GLC_BOUNDS, boundingbox))
	{
		debug(LOG_ERROR, "iV_GetTextBelowBase: couldn't retrieve a bounding box for the character");
		return 0;
	}

	point_bottom_y = boundingbox[1];
	point_height = point_bottom_y - point_base_y;
	pixel_height = getGLCPointToPixel(point_height);
	return (int)pixel_height;
}

void iV_SetTextColour(SWORD Index)
{
	switch (Index)
	{
		case PIE_TEXT_WHITE:
			font_colour[0] = 1.f;
			font_colour[1] = 1.f;
			font_colour[2] = 1.f;
			font_colour[3] = 1.f;
			break;

		case PIE_TEXT_LIGHTBLUE:
			font_colour[0] = 0.627451f;
			font_colour[1] = 0.627451f;
			font_colour[2] = 1.f;
			font_colour[3] = 1.f;
			break;

		case PIE_TEXT_DARKBLUE:
			font_colour[0] = 0.376471f;
			font_colour[1] = 0.376471f;
			font_colour[2] = 0.752941f;
			font_colour[3] = 1.f;
			break;
	};
}

// --------------------------------------------------------------------------

enum {
	EXTENTS_NONE,
	EXTENTS_START,
	EXTENTS_END
};

static char FString[256];		// Must do something about these wastefull static arrays.
static char FWord[256];
static int LastX;				// Cursor position after last draw.
static int LastY;
static int LastTWidth;			// Pixel width of the last string draw.
static int RecordExtents = EXTENTS_NONE;
static int ExtentsStartX;
static int ExtentsStartY;
static int ExtentsEndX;
static int ExtentsEndY;

// Draws formatted text with word wrap, long word splitting, embedded
// newlines ( uses @ rather than \n ) and colour mode toggle ( # ) which enables
// or disables font colouring.
//
//	UBYTE *String		The string to display.
//	UDWORD x			x coord of top left of formatted text window.
//	UDWORD y			y coord of top left of formatted text window.
//	UDWORD Width		Width of formatted text window.
//	UDWORD Justify		Justify mode, one of the following:
//							FTEXT_LEFTJUSTIFY
//							FTEXT_CENTRE
//							FTEXT_RIGHTJUSTIFY
//	BOOL DrawBack		If TRUE then draws transparent box behind text.
//
// Returns y coord of next text line.
//
UDWORD iV_DrawFormattedText(const char* String, UDWORD x, UDWORD y, UDWORD Width, UDWORD Justify)
{
	int i;
	int jx = x;		// Default to left justify.
	int jy = y;
	UDWORD WWidth;
	int TWidth;

	const char* curChar = String;

	curChar = String;
	while (*curChar != 0)
	{
		char* curSpaceChar;

		bool GotSpace = false;
		bool NewLine = false;

		// Reset text draw buffer
		FString[0] = 0;

		WWidth = 0;

		// Parse through the string, adding words until width is achieved.
		while (*curChar != 0 && WWidth < Width && !NewLine)
		{
			const char* startOfWord = curChar;
			const unsigned int FStringWidth = iV_GetTextWidth(FString);

			// Get the next word.
			i = 0;
			for (; *curChar != 0
			    && *curChar != ASCII_SPACE
			    && *curChar != ASCII_NEWLINE;
			     ++i, ++curChar)
			{
				if (*curChar == ASCII_COLOURMODE) // If it's a colour mode toggle char then just add it to the word.
				{
					FWord[i] = *curChar;

					// this character won't be drawn so don't deal with its width
					continue;
				}

				// Update this line's pixel width.
				WWidth = FStringWidth + iV_GetCountedTextWidth(FWord, i + 1);

				// If this word doesn't fit on the current line then break out
				if (WWidth > Width)
					break;

				// If width ok then add this character to the current word.
				FWord[i] = *curChar;
			}

			// Don't forget the space.
			if (*curChar == ASCII_SPACE)
			{
				WWidth += iV_GetCharWidth('-'); // cannot use space; QuesoGLC may hang
				if (WWidth <= Width)
				{
					FWord[i] = ' ';
					++i;
					++curChar;
					GotSpace = true;
				}
			}
			// Check for new line character.
			else if (*curChar == ASCII_NEWLINE)
			{
				NewLine = true;
				++curChar;
			}

			// If we've passed a space on this line and the word goes past the
			// maximum width and this isn't caused by the appended space then
			// rewind to the start of this word and finish this line.
			if (GotSpace
			 && WWidth > Width
			 && FWord[i - 1] != ' ')
			{
				// Skip back to the beginning of this
				// word and draw it on the next line
				curChar = startOfWord;
				break;
			}

			// Terminate the word.
			FWord[i] = 0;

			// And add it to the output string.
			strncat(FString, FWord, sizeof(FString));
			// Guarantee to nul-terminate
			FString[sizeof(FString) - 1] = '\0';
		}


		// Remove trailing spaces, useful when doing center alignment.
		curSpaceChar = &FString[strlen(FString) - 1];
		while (curSpaceChar != &FString[-1] && *curSpaceChar == ASCII_SPACE)
		{
			*(curSpaceChar--) = 0;
		}

		TWidth = iV_GetTextWidth(FString);

		// Do justify.
		switch (Justify)
		{
			case FTEXT_CENTRE:
				jx = x + (Width - TWidth) / 2;
				break;

			case FTEXT_RIGHTJUSTIFY:
				jx = x + Width - TWidth;
				break;

			case FTEXT_LEFTJUSTIFY:
				jx = x;
				break;
		}

		// draw the text.
		//iV_SetTextSize(12.f);
		iV_DrawText(FString, jx, jy);

		/* callback type for resload display callback*/
		// remember where we were..
		LastX = jx + TWidth;
		LastY = jy;
		LastTWidth = TWidth;

		// and move down a line.
		jy += iV_GetTextLineSize();
	}

	if (RecordExtents == EXTENTS_START)
	{
		RecordExtents = EXTENTS_END;

		ExtentsStartY = y + iV_GetTextAboveBase();
		ExtentsEndY = jy - iV_GetTextLineSize() + iV_GetTextBelowBase();

		ExtentsStartX = x;	// Was jx, but this broke the console centre justified text background.
		ExtentsEndX = x + Width;
	}
	else if (RecordExtents == EXTENTS_END)
	{
		ExtentsEndY = jy - iV_GetTextLineSize() + iV_GetTextBelowBase();

		ExtentsEndX = x + Width;
	}

	return jy;
}

void iV_DrawTextRotated(const char* string, float XPos, float YPos, float rotation)
{
	pie_SetTexturePage(-2);

	// Enable Anti Aliasing if it's enabled
	if (anti_aliasing)
	{
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE);
		glEnable(GL_POLYGON_SMOOTH);
	}

	if (rotation != 0.f)
	{
		rotation = 360.f - rotation;
	}

	glTranslatef(XPos, YPos, 0.f);
	glRotatef(180.f, 1.f, 0.f, 0.f);
	glRotatef(rotation, 0.f, 0.f, 1.f);
	glScalef(font_size, font_size, 0.f);

	glColor4fv(font_colour);

	glFrontFace(GL_CW);
	glcRenderString(string);
	glFrontFace(GL_CCW);

	// Turn off anti aliasing (if we enabled it above)
	if (anti_aliasing)
	{
		glDisable(GL_BLEND);
		glDisable(GL_POLYGON_SMOOTH);
	}

	// Reset the current model view matrix
	glLoadIdentity();
}

void iV_SetTextSize(float size)
{
	font_size = size;
}
