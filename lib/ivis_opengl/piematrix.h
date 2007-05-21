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
/***************************************************************************/
/*
 * pieMatrix.h
 *
 * matrix functions for pumpkin image library.
 *
 */
/***************************************************************************/
#ifndef _pieMatrix_h
#define _pieMatrix_h

#include "lib/ivis_common/piedef.h"

/***************************************************************************/
/*
 *	Global Definitions
 */
/***************************************************************************/

typedef struct {SDWORD a, b, c,  d, e, f,  g, h, i,  j, k, l;} SDMATRIX;

/***************************************************************************/
/*
 *	Global Variables
 */
/***************************************************************************/

extern SDMATRIX *psMatrix;
extern SDWORD aSinTable[];

//*************************************************************************

// FIXME DUPLICATE CODE! Already present in trig.c!
#define SIN(X) aSinTable[(Uint16)(X) >> 4]
#define COS(X) aSinTable[((Uint16)(X) >> 4) + 1024]


//*************************************************************************

/*!
 * Rotate and translate v with the worldmatrix. Store the result in s
 * \param[in] v Vector to translate
 * \param[out] s Resulting vector
 */
static inline void pie_RotateTranslate3iv(Vector3i * v, Vector3i * s)
{
	s->x = ( v->x * psMatrix->a + v->z * psMatrix->d + v->y * psMatrix->g
			+ psMatrix->j ) / FP12_MULTIPLIER;
	s->z = ( v->x * psMatrix->b + v->z * psMatrix->e + v->y * psMatrix->h
			+ psMatrix->k ) / FP12_MULTIPLIER;
	s->y = ( v->x * psMatrix->c + v->z * psMatrix->f + v->y * psMatrix->i
			+ psMatrix->l ) / FP12_MULTIPLIER;
}


/*!
 * returns true if both vectors are equal
 */
static inline BOOL Vector3i_compare(const Vector3i *a, const Vector3i *b)
{
	return a->x == b->x && a->y == b->y && a->z == b->z;
}


/*!
 * returns true if both vectors are equal
 */
static inline BOOL Vector3f_compare(const Vector3f *a, const Vector3f *b)
{
	return a->x == b->x && a->y == b->y && a->z == b->z;
}


//*************************************************************************


extern void pie_MatInit(void);


//*************************************************************************

extern void pie_MatBegin(void);
extern void pie_MatEnd(void);
extern void pie_MATTRANS(int x, int y, int z);
extern void pie_TRANSLATE(int x, int y, int z);
extern void pie_MatScale( UDWORD percent );
extern void pie_MatRotX(int x);
extern void pie_MatRotY(int y);
extern void pie_MatRotZ(int z);
extern Sint32 pie_RotateProject(Vector3i *v3d, Vector2i *v2d);

//*************************************************************************

extern void pie_PerspectiveBegin(void);
extern void pie_PerspectiveEnd(void);

//*************************************************************************

extern void pie_VectorNormalise3iv(Vector3i *v);
extern void pie_VectorNormalise3fv(Vector3f *v);
extern void pie_VectorInverseRotate0(Vector3i *v1, Vector3i *v2);
extern void pie_SurfaceNormal3iv(Vector3i *p1, Vector3i *p2, Vector3i *p3, Vector3i *v);
extern void pie_SurfaceNormal3fv(Vector3f *p1, Vector3f *p2, Vector3f *p3, Vector3f *v);
extern BOOL pie_Clockwise(iVertex *s);
extern void pie_SetGeometricOffset(int x, int y);

extern BOOL pie_PieClockwise(PIEVERTEX *s);

void pie_Begin3DScene(void);
void pie_BeginInterface(void);

#endif
