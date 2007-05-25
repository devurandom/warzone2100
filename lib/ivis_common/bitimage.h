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
#ifndef __INCLUDED_BITIMAGE__
#define __INCLUDED_BITIMAGE__


WZ_DECL_PURE static inline unsigned short iV_GetImageWidth(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].Width;
}


WZ_DECL_PURE static inline unsigned short iV_GetImageHeight(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].Height;
}


WZ_DECL_PURE static inline short iV_GetImageXOffset(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].XOffset;
}


WZ_DECL_PURE static inline short iV_GetImageYOffset(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].YOffset;
}


WZ_DECL_PURE static inline unsigned short iV_GetImageCenterX(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].XOffset + ImageFile->ImageDefs[ID].Width/2;
}


WZ_DECL_PURE static inline unsigned short iV_GetImageCenterY(const IMAGEFILE *ImageFile, const unsigned short ID)
{
	assert(ID < ImageFile->Header.NumImages);
	return ImageFile->ImageDefs[ID].YOffset + ImageFile->ImageDefs[ID].Height/2;
}


IMAGEFILE *iV_LoadImageFile(const char *FileData, const UDWORD FileSize);
void iV_FreeImageFile(IMAGEFILE *ImageFile);

#endif
