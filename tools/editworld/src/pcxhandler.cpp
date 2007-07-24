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

	$Revision$
	$Id$
	$HeadURL$
*/

//#include "stdafx.h"
#include "windows.h"
#include "windowsx.h"
#include "stdio.h"
#include "typedefs.h"
#include "debugprint.h"

#include "pcxhandler.h"

#include <assert.h>

// Round the given value up to the nearest power of 2.
int Power2(int value)
{
	if(IsPower2(value))
		return value;

	for(int i = 31; i >= 0; --i)
	{
		if(value & 1 << i)
			break;
	}

	assert(i != 31);	// Overflow.

	return 1 << (i + 1);
}

PCXHandler::PCXHandler() :
	_BitmapInfo(NULL),
	_DIBBitmap(NULL),
	_Palette(NULL)
{
}

PCXHandler::~PCXHandler()
{
//	DebugPrint("Deleted PCXHandler\n");

	delete _Palette;

	if (_DIBBitmap != NULL)
		DeleteObject(_DIBBitmap);

	delete _BitmapInfo;
}

bool PCXHandler::ReadPCX(const char* FilePath, DWORD Flags)
{
	FILE* fid = fopen(FilePath,"rb");
    if (!fid)
	{
		MessageBox( NULL, FilePath, "Unable to open file.", MB_OK );
		return false;
	}
	
// Read the PCX header.
	fread(&_Header, sizeof(_Header), 1, fid);

	if (_Header.Manufacturer != 10)
	{
		fclose(fid);
		MessageBox(NULL, FilePath, "File is not a valid PCX.", MB_OK);
		return false;
	}

	if(_Header.NPlanes != 1)
	{
		MessageBox(NULL, FilePath, "Unable to load PCX. Not 256 colour.", MB_OK);
		return false;
	}

// Allocate memory for the bitmap.

	int Height = _Header.Window[3] + 1;
	if(Flags & BMR_ROUNDUP)
	{
		Height = Power2(Height);
	}

	char* Bitmap = new char[_Header.NPlanes * _Header.BytesPerLine * Height];
	LONG Size = _Header.NPlanes * _Header.BytesPerLine * (_Header.Window[3] + 1);

// Decode the bitmap.
	WORD	chr,cnt;
	LONG	decoded=0;
	char*	bufr = Bitmap;

	while( (!EncodedGet(&chr, &cnt, fid)) && (decoded < Size))
	{
		for(unsigned int i = 0; i < cnt; ++i)
		{
			*bufr++ = (UBYTE)chr;
			decoded++;
		}
	}

	WORD PaletteSize=0;
	unsigned int i;

// If there's a palette on the end then read it.

//	DebugPrint("PCX Version %d\n",m_Header.Version);

	switch(_Header.Version)
	{
		case 5:
			fseek(fid, -769, SEEK_END);
			if(getc(fid) != 12)
				break;

			PaletteSize = 1 << _Header.BitsPerPixel;
			_Palette = new PALETTEENTRY[PaletteSize];

			for (i = 0; i < PaletteSize; ++i)
			{
				_Palette[i].peRed   = (BYTE)getc(fid);
				_Palette[i].peGreen = (BYTE)getc(fid);
				_Palette[i].peBlue  = (BYTE)getc(fid);
				_Palette[i].peFlags = (BYTE)0;
			}
			break;

		default:
			fclose(fid);
			MessageBox(NULL, FilePath, "PCX version not supported.", MB_OK);
			return false;
	}

	fclose(fid);
	
	_BitmapInfo = reinterpret_cast<BITMAPINFO*>(new char[sizeof(BITMAPINFO) + sizeof(RGBQUAD) * PaletteSize]);
	_BitmapInfo->bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
	_BitmapInfo->bmiHeader.biWidth         = _Header.Window[2]+1;
	_BitmapInfo->bmiHeader.biHeight        = -Height;	//(m_Header.Window[3]+1);
	_BitmapInfo->bmiHeader.biPlanes        = 1;
	_BitmapInfo->bmiHeader.biBitCount      = _Header.BitsPerPixel * _Header.NPlanes;
	_BitmapInfo->bmiHeader.biCompression   = BI_RGB;
	_BitmapInfo->bmiHeader.biSizeImage     = 0;
	_BitmapInfo->bmiHeader.biXPelsPerMeter = 0;
	_BitmapInfo->bmiHeader.biYPelsPerMeter = 0;
	_BitmapInfo->bmiHeader.biClrUsed       = 0;
	_BitmapInfo->bmiHeader.biClrImportant  = 0;

	for(i = 0; i < PaletteSize; ++i)
	{
		_BitmapInfo->bmiColors[i].rgbRed   = _Palette[i].peRed;
		_BitmapInfo->bmiColors[i].rgbGreen = _Palette[i].peGreen;
		_BitmapInfo->bmiColors[i].rgbBlue  = _Palette[i].peBlue;
		_BitmapInfo->bmiColors[i].rgbReserved = 0;
	}

	_DIBBitmap = CreateDIBSection(NULL, _BitmapInfo, DIB_RGB_COLORS, &_DIBBits, NULL, 0);

	if(!_DIBBitmap)
	{
		MessageBox( NULL, FilePath, "Failed to create DIB.", MB_OK );
		return false;
	}

	char* Dst = reinterpret_cast<char*>(_DIBBits);

	for(int j = 0; j < Height; ++j)
	{
		char* Src = Bitmap + j * _Header.BytesPerLine * _Header.NPlanes;

		for(i = 0; i < _Header.BytesPerLine; ++i)
		{
			for(unsigned int p = 0; p < _Header.NPlanes; ++p)
			{
				*Dst = *(Src + _Header.BytesPerLine * p);
				Dst++;
			}

			Src++;
		}
	}

	delete [] Bitmap;

	return true;
}


bool PCXHandler::Create(int Width,int Height,void *Bits, PALETTEENTRY *Palette)
{
	_Header.Manufacturer = 10;
	_Header.Version      = 5;
	_Header.Encoding     = 1;
	_Header.BitsPerPixel = 8;
	_Header.Window[0]    = 0;
	_Header.Window[1]    = 0;
	_Header.Window[2]    = Width - 1;
	_Header.Window[3]    = Height - 1;
	_Header.HRes         = 150;
	_Header.VRes         = 150;
	memset(_Header.Colormap, 0, 48 * sizeof(char));
	_Header.Reserved     = 0;
	_Header.NPlanes      = 1;
	_Header.BytesPerLine = Width;
	_Header.PaletteInfo  = 1;
	memset(_Header.Filler, 0, 58 * sizeof(char));

	unsigned int PaletteSize = 1 << _Header.BitsPerPixel;
	_Palette = new PALETTEENTRY[PaletteSize];

	for (unsigned int i = 0; i < PaletteSize; ++i)
	{
		_Palette[i] = Palette[i];
	}

	_BitmapInfo = reinterpret_cast<BITMAPINFO*>(new char[sizeof(BITMAPINFO) + sizeof(RGBQUAD) * PaletteSize]);
	_BitmapInfo->bmiHeader.biSize          = sizeof(BITMAPINFOHEADER);
	_BitmapInfo->bmiHeader.biWidth         = _Header.Window[2] + 1;
	_BitmapInfo->bmiHeader.biHeight        = -Height;
	_BitmapInfo->bmiHeader.biPlanes        = 1;
	_BitmapInfo->bmiHeader.biBitCount      = _Header.BitsPerPixel * _Header.NPlanes;
	_BitmapInfo->bmiHeader.biCompression   = BI_RGB;
	_BitmapInfo->bmiHeader.biSizeImage     = 0;
	_BitmapInfo->bmiHeader.biXPelsPerMeter = 0;
	_BitmapInfo->bmiHeader.biYPelsPerMeter = 0;
	_BitmapInfo->bmiHeader.biClrUsed       = 0;
	_BitmapInfo->bmiHeader.biClrImportant  = 0;

	for(i = 0; i < PaletteSize; ++i)
	{
		_BitmapInfo->bmiColors[i].rgbRed      = _Palette[i].peRed;
		_BitmapInfo->bmiColors[i].rgbGreen    = _Palette[i].peGreen;
		_BitmapInfo->bmiColors[i].rgbBlue     = _Palette[i].peBlue;
		_BitmapInfo->bmiColors[i].rgbReserved = 0;
	}

	_DIBBitmap = CreateDIBSection(NULL, _BitmapInfo, DIB_RGB_COLORS, &_DIBBits, NULL, 0);

	if(!_DIBBitmap)
	{
		MessageBox( NULL, "Error", "Failed to create DIB.", MB_OK );
		return false;
	}

	char* Dst = reinterpret_cast<char*>(_DIBBits);
	char* Src;

	for(int j = 0; j < Height; ++j)
	{
		Src = reinterpret_cast<char*>(Bits) + j * _Header.BytesPerLine * _Header.NPlanes;
		for(i = 0; i < _Header.BytesPerLine; ++i)
		{
			for(unsigned int p = 0; p < _Header.NPlanes; ++p)
			{
				*Dst = *(Src + _Header.BytesPerLine * p);
				++Dst;
			}

			++Src;
		}
	}

	return true;
}


bool PCXHandler::WritePCX(char *FilePath)
{
	FILE *fid = fopen(FilePath,"wb");
    if(!fid)
	{
		MessageBox( NULL, FilePath, "Unable to create file.", MB_OK );
		return false;
	}

// Write the PCX header.
	fwrite(&_Header, sizeof(_Header), 1, fid);

// Encode and write the body.
	char* Ptr = reinterpret_cast<char*>(_DIBBits);
	for(unsigned int i = 0; i < GetBitmapHeight(); ++i)
	{
		EncodeLine(Ptr, (WORD)GetBitmapWidth(), fid);
		Ptr += GetBitmapWidth();
	}

// Write the palette.
	putc(12, fid);
	unsigned int PaletteSize = 1 << _Header.BitsPerPixel;
	for (i = 0; i < PaletteSize; ++i)
	{
		putc(_Palette[i].peRed, fid);
		putc(_Palette[i].peGreen, fid);
		putc(_Palette[i].peBlue, fid);
	}
	
	fclose(fid);

	return true;
}


/* This procedure reads one encoded block from the image file and
stores a count and data byte. Result:
    0 = valid data stored
    EOF = out of data in file
int *pbyt;     where to place data
int *pcnt;     where to place count
FILE *fid;     image file handle
*/
WORD PCXHandler::EncodedGet(WORD *pbyt, WORD *pcnt, FILE *fid)
{
	*pcnt = 1;     /* safety play */

	int i = getc(fid);
	if (i == EOF)
		return EOF;

	if ((0xc0 & i) == 0xc0)
	{
		*pcnt = 0x3f & i;

		i = getc(fid);
		if(i == EOF)
			return EOF;
	}

	*pbyt = i;

	return 0;
}


/* This subroutine encodes one scanline and writes it to a file
unsigned char *inBuff;  pointer to scanline data
int inLen;              length of raw scanline in bytes
FILE *fp;               file to be written to
*/
WORD PCXHandler::EncodeLine(const char* inBuff, WORD inLen, FILE *fid)
{  /* returns number of bytes written into outBuff, 0 if failed */

	WORD i;

	int total = 0;
	unsigned int runCount = 1; /* max single runlength is 63 */

	char last = *inBuff;
	char thisone;

	for (unsigned int srcIndex = 1; srcIndex < inLen; srcIndex++)
	{
		thisone = *(++inBuff);
		if (thisone == last)
		{
			++runCount;  /* it encodes */
			if (runCount == 63)
			{
				if (!(i = EncodedPut(last, runCount, fid)))
					return 0;

				total += i;
				runCount = 0;
			}
		}
		else
		{  /* thisone != last */
			if (runCount)
			{
				if (!(i = EncodedPut(last, runCount, fid)))
					return 0;

				total += i;
			}

			last = thisone;
			runCount = 1;
		}
	} /* endloop */

	if (runCount)
	{  /* finish up */
		if (!(i = EncodedPut(last, runCount, fid)))
			return 0;

		return total + i;
	}

	return total;
}



/* subroutine for writing an encoded byte pair 
(or single byte  if it doesn't encode) to a file
unsigned char byt, cnt;
FILE *fid;
*/
WORD PCXHandler::EncodedPut(UBYTE byt, UBYTE cnt, FILE *fid) /* returns count of bytes written, 0 if err */
{
	if(cnt)
	{
		if((cnt==1) && (0xc0 != (0xc0&byt)))
		{
			if(EOF == putc((int)byt, fid))
				return 0; /* disk write error (probably full) */

			return 1;
		}
		else
		{
			if(EOF == putc((int)0xC0 | cnt, fid))
				return 0;  /* disk write error */

			if(EOF == putc((int)byt, fid))
				return 0;  /* disk write error */

			return 2;
		}
	}

	return 0;
}
