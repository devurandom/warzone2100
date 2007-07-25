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
 * pieMatrix.c
 *
 * matrix functions for pumpkin image library.
 *
 */
/***************************************************************************/

#include "lib/framework/frame.h"

#include <SDL/SDL_opengl.h>

#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/pieclip.h"
#include "piematrix.h"
#include "lib/ivis_common/rendmode.h"

/***************************************************************************/
/*
 *	Local Definitions
 */
/***************************************************************************/

#define MATRIX_MAX 8
#define ONE_PERCENT 4096/100

static SDMATRIX	aMatrixStack[MATRIX_MAX];
SDMATRIX *psMatrix = &aMatrixStack[0];

BOOL drawing_interface = TRUE;


void pie_VectorNormalise3iv(Vector3i *v)
{
	Sint32 size;
	Vector3i av;

	av.x = ABS(v->x);
	av.y = ABS(v->y);
	av.z = ABS(v->z);
	if (av.x >= av.y)
	{
		if (av.x > av.z)
			size = av.x + av.z/4 + av.y/4;
		else
			size = av.z + av.x/4 + av.y/4;
	}
	else
	{
		if (av.y > av.z)
			size = av.y + av.z/4 + av.x/4;
		else
			size = av.z + av.y/4 + av.x/4;
	}

	if (size > 0)
	{
		v->x = (v->x * FP12_MULTIPLIER) / size;
		v->y = (v->y * FP12_MULTIPLIER) / size;
		v->z = (v->z * FP12_MULTIPLIER) / size;
	}
}


void pie_VectorNormalise3fv(Vector3f *v)
{
	Sint32 size;
	Vector3f av;

	av.x = ABS(v->x);
	av.y = ABS(v->y);
	av.z = ABS(v->z);
	if (av.x >= av.y) {
		if (av.x > av.z)
			size = av.x + av.z/4 + av.y/4;
		else
			size = av.z + av.x/4 + av.y/4;
	} else {
		if (av.y > av.z)
			size = av.y + av.z/4 + av.x/4;
		else
			size = av.z + av.y/4 + av.x/4;
	}

	if (size > 0) {
		v->x = (v->x * FP12_MULTIPLIER) / size;
		v->y = (v->y * FP12_MULTIPLIER) / size;
		v->z = (v->z * FP12_MULTIPLIER) / size;
	}
}


/*!
 * Calculate surface normal
 * Eg. if a polygon (with n points in clockwise order) normal is required,
 * p1 = point 0, p2 = point 1, p3 = point n-1
 * \param[in] p1,p1,p3 points for forming 2 vector for cross product
 * \param[out] v normal vector returned << FP12_SHIFT
 */
void pie_SurfaceNormal3iv(Vector3i *p1, Vector3i *p2, Vector3i *p3, Vector3i *v)
{
	Vector3i a, b;

	a.x = p3->x - p1->x;
	a.y = p3->y - p1->y;
	a.z = p3->z - p1->z;
	pie_VectorNormalise3iv(&a);

 	b.x = p2->x - p1->x;
	b.y = p2->y - p1->y;
	b.z = p2->z - p1->z;
	pie_VectorNormalise3iv(&b);

	v->x = ((a.y * b.z) - (a.z * b.y)) / FP12_MULTIPLIER;
	v->y = ((a.z * b.x) - (a.x * b.z)) / FP12_MULTIPLIER;
	v->z = ((a.x * b.y) - (a.y * b.x)) / FP12_MULTIPLIER;
	pie_VectorNormalise3iv(v);
}


/*!
 * Calculate surface normal
 * Eg. if a polygon (with n points in clockwise order) normal is required,
 * p1 = point 0, p2 = point 1, p3 = point n-1
 * \param[in] p1,p1,p3 points for forming 2 vector for cross product
 * \param[out] v normal vector returned << FP12_SHIFT
 */
void pie_SurfaceNormal3fv(Vector3f *p1, Vector3f *p2, Vector3f *p3, Vector3f *v)
{
	Vector3f a, b;

	a.x = p3->x - p1->x;
	a.y = p3->y - p1->y;
	a.z = p3->z - p1->z;
	pie_VectorNormalise3fv(&a);

 	b.x = p2->x - p1->x;
	b.y = p2->y - p1->y;
	b.z = p2->z - p1->z;
	pie_VectorNormalise3fv(&b);

	v->x = ((a.y * b.z) - (a.z * b.y)) / FP12_MULTIPLIER;
	v->y = ((a.z * b.x) - (a.x * b.z)) / FP12_MULTIPLIER;
	v->z = ((a.x * b.y) - (a.y * b.x)) / FP12_MULTIPLIER;
	pie_VectorNormalise3fv(v);
}


#define SC_TABLESIZE	4096

//*************************************************************************

static SDMATRIX _MATRIX_ID = {FP12_MULTIPLIER, 0, 0, 0, FP12_MULTIPLIER, 0, 0, 0, FP12_MULTIPLIER, 0L, 0L, 0L};
static SDWORD _MATRIX_INDEX;

//*************************************************************************

SDWORD aSinTable[SC_TABLESIZE + (SC_TABLESIZE/4)];

//*************************************************************************
//*** reset transformation matrix stack and make current identity
//*
//******

static void pie_MatReset(void)
{
	psMatrix = &aMatrixStack[0];

	// make 1st matrix identity
	*psMatrix = _MATRIX_ID;

	glLoadIdentity();
}


//*************************************************************************
//*** create new matrix from current transformation matrix and make current
//*
//******

void pie_MatBegin(void)
{
	_MATRIX_INDEX++;
	ASSERT( _MATRIX_INDEX < MATRIX_MAX, "pie_MatBegin past top of the stack" );

	psMatrix++;
	aMatrixStack[_MATRIX_INDEX] = aMatrixStack[_MATRIX_INDEX-1];

	glPushMatrix();
}


//*************************************************************************
//*** make current transformation matrix previous one on stack
//*
//******

void pie_MatEnd(void)
{
	_MATRIX_INDEX--;
	ASSERT( _MATRIX_INDEX >= 0, "pie_MatEnd of the bottom of the stack" );

	psMatrix--;

	glPopMatrix();
}


void pie_MATTRANS(int x, int y, int z) {
	GLfloat matrix[16];

	psMatrix->j = x<<FP12_SHIFT;
	psMatrix->k = y<<FP12_SHIFT;
	psMatrix->l = z<<FP12_SHIFT;

	glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
	matrix[12] = x;
	matrix[13] = y;
	matrix[14] = z;
	glLoadIdentity();
	glMultMatrixf(matrix);
}

void pie_TRANSLATE(int x, int y, int z) {
	psMatrix->j += ((x) * psMatrix->a + (y) * psMatrix->d + (z) * psMatrix->g);
	psMatrix->k += ((x) * psMatrix->b + (y) * psMatrix->e + (z) * psMatrix->h);
	psMatrix->l += ((x) * psMatrix->c + (y) * psMatrix->f + (z) * psMatrix->i);

	glTranslatef(x, y, z);
}

//*************************************************************************
//*** matrix scale current transformation matrix
//*
//******
void pie_MatScale( UDWORD percent )
{
	SDWORD scaleFactor;

	if (percent == 100)
	{
		return;
	}

	scaleFactor = percent * ONE_PERCENT;

	psMatrix->a = (psMatrix->a * scaleFactor) / 4096;
	psMatrix->b = (psMatrix->b * scaleFactor) / 4096;
	psMatrix->c = (psMatrix->c * scaleFactor) / 4096;

	psMatrix->d = (psMatrix->d * scaleFactor) / 4096;
	psMatrix->e = (psMatrix->e * scaleFactor) / 4096;
	psMatrix->f = (psMatrix->f * scaleFactor) / 4096;

	psMatrix->g = (psMatrix->g * scaleFactor) / 4096;
	psMatrix->h = (psMatrix->h * scaleFactor) / 4096;
	psMatrix->i = (psMatrix->i * scaleFactor) / 4096;

	glScalef(0.01f*percent, 0.01f*percent, 0.01f*percent);
}


//*************************************************************************
//*** matrix rotate y (yaw) current transformation matrix
//*
//******

void pie_MatRotY(int y)

{
// printf("pie_MatRotY %i\n", y);
	int t;
	int cra, sra;

	if (y != 0) {
   	cra = COS(y);
	sra = SIN(y);

		t = ((cra * psMatrix->a) - (sra * psMatrix->g))>>FP12_SHIFT;
		psMatrix->g = ((sra * psMatrix->a) + (cra * psMatrix->g))>>FP12_SHIFT;
		psMatrix->a = t;

		t = ((cra * psMatrix->b) - (sra * psMatrix->h))>>FP12_SHIFT;
		psMatrix->h = ((sra * psMatrix->b) + (cra * psMatrix->h))>>FP12_SHIFT;
		psMatrix->b = t;

		t = ((cra * psMatrix->c) - (sra * psMatrix->i))>>FP12_SHIFT;
		psMatrix->i = ((sra * psMatrix->c) + (cra * psMatrix->i))>>FP12_SHIFT;
		psMatrix->c = t;
	}

	glRotatef(y*22.5f/4096.0f, 0.0f, 1.0f, 0.0f);
}


//*************************************************************************
//*** matrix rotate z (roll) current transformation matrix
//*
//******

void pie_MatRotZ(int z)

{
// printf("pie_MatRotZ %i\n", z);
	int t;
	int cra, sra;

	if (z != 0) {
		cra = COS(z);
		sra = SIN(z);

		t = ((cra * psMatrix->a) + (sra * psMatrix->d))>>FP12_SHIFT;
		psMatrix->d = ((cra * psMatrix->d) - (sra * psMatrix->a))>>FP12_SHIFT;
		psMatrix->a = t;

		t = ((cra * psMatrix->b) + (sra * psMatrix->e))>>FP12_SHIFT;
		psMatrix->e = ((cra * psMatrix->e) - (sra * psMatrix->b))>>FP12_SHIFT;
		psMatrix->b = t;

		t = ((cra * psMatrix->c) + (sra * psMatrix->f))>>FP12_SHIFT;
		psMatrix->f = ((cra * psMatrix->f) - (sra * psMatrix->c))>>FP12_SHIFT;
		psMatrix->c = t;
	}

	glRotatef(z*22.5f/4096.0f, 0.0f, 0.0f, 1.0f);
}


//*************************************************************************
//*** matrix rotate x (pitch) current transformation matrix
//*
//******

void pie_MatRotX(int x)
{
// printf("pie_MatRotX %i\n", x);
	int cra, sra;
	int t;

	if (x != 0) {
		cra = COS(x);
		sra = SIN(x);

		t = ((cra * psMatrix->d) + (sra * psMatrix->g))>>FP12_SHIFT;
		psMatrix->g = ((cra * psMatrix->g) - (sra * psMatrix->d))>>FP12_SHIFT;
		psMatrix->d = t;

		t = ((cra * psMatrix->e) + (sra * psMatrix->h))>>FP12_SHIFT;
		psMatrix->h = ((cra * psMatrix->h) - (sra * psMatrix->e))>>FP12_SHIFT;
		psMatrix->e = t;

		t = ((cra * psMatrix->f) + (sra * psMatrix->i))>>FP12_SHIFT;
		psMatrix->i = ((cra * psMatrix->i) - (sra * psMatrix->f))>>FP12_SHIFT;
		psMatrix->f = t;
	}

	glRotatef(x*22.5f/4096.0f, 1.0f, 0.0f, 0.0f);
}


/*!
 * 3D vector perspective projection
 * Projects 3D vector into 2D screen space
 * \param v3d 3D vector to project
 * \param v2d resulting 2D vector
 * \return projected z component of v2d
 */
Sint32 pie_RotateProject(const Vector3i *v3d, Vector2i *v2d)
{
	Sint32 zfx, zfy;
	Sint32 zz, _x, _y, _z;

	_x = v3d->x * psMatrix->a + v3d->y * psMatrix->d + v3d->z * psMatrix->g + psMatrix->j;
	_y = v3d->x * psMatrix->b + v3d->y * psMatrix->e + v3d->z * psMatrix->h + psMatrix->k;
	_z = v3d->x * psMatrix->c + v3d->y * psMatrix->f + v3d->z * psMatrix->i + psMatrix->l;

	zz = _z >> STRETCHED_Z_SHIFT;

	zfx = _z >> psRendSurface->xpshift;
	zfy = _z >> psRendSurface->ypshift;

	if (zfx <= 0 || zfy <= 0 || zz < MIN_STRETCHED_Z)
	{
		v2d->x = LONG_WAY; //just along way off screen
		v2d->y = LONG_WAY;
	}
	else
	{
		v2d->x = psRendSurface->xcentre + (_x / zfx);
		v2d->y = psRendSurface->ycentre - (_y / zfy);
	}

	return zz;
}


//*************************************************************************

void pie_PerspectiveBegin(void) {
	const float width = pie_GetVideoBufferWidth();
	const float height = pie_GetVideoBufferHeight();
	const float xangle = width/6;
	const float yangle = height/6;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glTranslatef((2*psRendSurface->xcentre-width)/width,
		     (height-2*psRendSurface->ycentre)/height , 0);
	glFrustum(-xangle, xangle, -yangle, yangle, 330, 100000);
	glScalef(1, 1, -1);
	glMatrixMode(GL_MODELVIEW);
}

void pie_PerspectiveEnd(void) {
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, pie_GetVideoBufferWidth(), pie_GetVideoBufferHeight(), 0, 1, -1);
	glMatrixMode(GL_MODELVIEW);
}

void pie_Begin3DScene(void) {
	glDepthRange(0.1, 1);
	drawing_interface = FALSE;
}

void pie_BeginInterface(void) {
	glDepthRange(0, 0.1);
	drawing_interface = TRUE;
}

//*************************************************************************

void pie_SetGeometricOffset(int x, int y)

{
	psRendSurface->xcentre = x;
	psRendSurface->ycentre = y;
}


//*************************************************************************
//*** inverse rotate 3D vector with current rotation matrix
//*
//* params	v1 = pointer to 3D vector to rotate
//* 			v2 = pointer to 3D resultant vector
//*
//* on exit	v2 = inverse-rotated vector
//*
//******

void pie_VectorInverseRotate0(Vector3i *v1, Vector3i *v2)
{
	Sint32 x, y, z;

	x = v1->x; y = v1->y; z = v1->z;

	v2->x = (x * psMatrix->a+y * psMatrix->b+z * psMatrix->c) >> FP12_SHIFT;
	v2->y = (x * psMatrix->d+y * psMatrix->e+z * psMatrix->f) >> FP12_SHIFT;
	v2->z = (x * psMatrix->g+y * psMatrix->h+z * psMatrix->i) >> FP12_SHIFT;
}

//*************************************************************************
//*** setup transformation matrices/quaternions and trig tables
//*
//******

void pie_MatInit(void)
{
	unsigned i, scsize;
	double conv, v;

	// sin/cos table

	scsize = SC_TABLESIZE + (SC_TABLESIZE / 4);
  	conv = (double)(M_PI / (0.5 * SC_TABLESIZE));

	for (i=0; i<scsize; i++) {
		v = (double) sin(i * conv) * FP12_MULTIPLIER;

		if (v >= 0.0)
			aSinTable[i] = (Sint32)(v + 0.5);
		else
			aSinTable[i] = (Sint32)(v - 0.5);
	}

	// init matrix/quat stack

	pie_MatReset();


	debug(LOG_3D, "geo[_geo_setup] = setup successful\n");
}
