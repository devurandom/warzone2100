#include "pieclip.h"
#include "ivi.h"

static BOOL bClipSpecular = TRUE;
static UDWORD	videoBufferWidth = 640, videoBufferHeight = 480;
extern iSurface	*psRendSurface;

BOOL pie_SetVideoBuffer(UDWORD width, UDWORD height)
{
	videoBufferWidth = width;
	videoBufferHeight = height;
	return(TRUE);
}

UDWORD pie_GetVideoBufferWidth(void)
{
	return(videoBufferWidth);
}

UDWORD pie_GetVideoBufferHeight(void)
{
	return(videoBufferHeight);
}

void pie_Set2DClip(int x0, int y0, int x1, int y1)
{
	psRendSurface->clip.left = x0;
	psRendSurface->clip.top = y0;
	psRendSurface->clip.right = x1;
	psRendSurface->clip.bottom = y1;
}

static int pie_ClipXT(PIEVERTEX *s1, PIEVERTEX *s2, PIEVERTEX *clip)
{
	int n, dx;
	int32 t;

	n = 1;

	if (s2->sx >= s1->sx) {
		if (s1->sx < psRendSurface->clip.left) {
			if (s2->sx <= psRendSurface->clip.left)
				return 0;

			dx = s2->sx - s1->sx;

			if (dx != 0)
				clip->sy = s1->sy + (s2->sy - s1->sy) * (psRendSurface->clip.left - s1->sx) / dx;
			else
				clip->sy = s1->sy;

			clip->sx = psRendSurface->clip.left;

			// clip uv
			t = ((clip->sx - s1->sx)<<iV_DIVSHIFT) / dx;

			clip->tu = s1->tu + ((t * (s2->tu - s1->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s2->tv - s1->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s2->sz - s1->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s2->light.byte.r - s1->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s2->light.byte.g - s1->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s2->light.byte.b - s1->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s2->light.byte.a - s1->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular)
			{
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s2->specular.byte.r - s1->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s2->specular.byte.g - s1->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s2->specular.byte.b - s1->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s2->specular.byte.a - s1->specular.byte.a)) >> iV_DIVSHIFT);
			}

		} else
			*clip = *s1;

		if (s2->sx > psRendSurface->clip.right) {
			if (s1->sx > psRendSurface->clip.right)
				return 0;

			clip++;
			dx = s2->sx - s1->sx;

			if (dx != 0)
				clip->sy = s2->sy - (s2->sy - s1->sy) * (s2->sx - psRendSurface->clip.right) / dx;
			else
				clip->sy = s2->sy;

			clip->sx = psRendSurface->clip.right;

			// clip uv
			t = ((clip->sx - s1->sx)<<iV_DIVSHIFT) / dx;
			clip->tu = s1->tu + ((t * (s2->tu - s1->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s2->tv - s1->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s2->sz - s1->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s2->light.byte.r - s1->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s2->light.byte.g - s1->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s2->light.byte.b - s1->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s2->light.byte.a - s1->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular) {
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s2->specular.byte.r - s1->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s2->specular.byte.g - s1->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s2->specular.byte.b - s1->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s2->specular.byte.a - s1->specular.byte.a)) >> iV_DIVSHIFT);
			}

			n = 2;
		}
		return n;

	} else {
		if (s1->sx > psRendSurface->clip.right) {

			if (s2->sx >= psRendSurface->clip.right) return 0;

			dx = s1->sx - s2->sx;

			if (dx != 0)
				clip->sy = s1->sy - (s1->sy - s2->sy) * (s1->sx - psRendSurface->clip.right) / dx;
			else
				clip->sy = s1->sy;

			clip->sx = psRendSurface->clip.right;

			// clip uv
			t = ((clip->sx - s1->sx)<<iV_DIVSHIFT) / dx;
			clip->tu = s1->tu + ((t * (s1->tu - s2->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s1->tv - s2->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s1->sz - s2->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s1->light.byte.r - s2->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s1->light.byte.g - s2->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s1->light.byte.b - s2->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s1->light.byte.a - s2->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular)
			{
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s1->specular.byte.r - s2->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s1->specular.byte.g - s2->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s1->specular.byte.b - s2->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s1->specular.byte.a - s2->specular.byte.a)) >> iV_DIVSHIFT);
			}


		} else
			*clip = *s1;


		if (s2->sx < psRendSurface->clip.left) {
			if (s1->sx < psRendSurface->clip.left)
				return 0;

			clip++;
			dx = s1->sx - s2->sx;

			if (dx != 0)
				clip->sy = s2->sy + (s1->sy - s2->sy) * (psRendSurface->clip.left - s2->sx) / dx;
			else
				clip->sy = s2->sy;

			clip->sx = psRendSurface->clip.left;

			// clip uv
			t = ((clip->sx - s1->sx)<<iV_DIVSHIFT) / dx;
			clip->tu = s1->tu + ((t * (s1->tu - s2->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s1->tv - s2->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s1->sz - s2->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s1->light.byte.r - s2->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s1->light.byte.g - s2->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s1->light.byte.b - s2->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s1->light.byte.a - s2->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular)
			{
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s1->specular.byte.r - s2->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s1->specular.byte.g - s2->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s1->specular.byte.b - s2->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s1->specular.byte.a - s2->specular.byte.a)) >> iV_DIVSHIFT);
			}

			n = 2;
		}
		return n;
	}
}

static int pie_ClipYT(PIEVERTEX *s1, PIEVERTEX *s2, PIEVERTEX *clip)
{
	int n, dy;
	int32 t;

	n = 1;

	if (s2->sy >= s1->sy) {

		if (s1->sy < psRendSurface->clip.top) {

			if (s2->sy <= psRendSurface->clip.top) return 0;

			dy = s2->sy - s1->sy;

			if (dy != 0)
				clip->sx = s1->sx + (s2->sx - s1->sx) * (psRendSurface->clip.top - s1->sy) / dy;
			else
				clip->sx = s1->sx;

			clip->sy = psRendSurface->clip.top;

			// clip uv
			t = ((clip->sy - s1->sy)<<iV_DIVSHIFT) / dy;
			clip->tu = s1->tu + ((t * (s2->tu - s1->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s2->tv - s1->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s2->sz - s1->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s2->light.byte.r - s1->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s2->light.byte.g - s1->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s2->light.byte.b - s1->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s2->light.byte.a - s1->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular)
			{
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s2->specular.byte.r - s1->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s2->specular.byte.g - s1->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s2->specular.byte.b - s1->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s2->specular.byte.a - s1->specular.byte.a)) >> iV_DIVSHIFT);
			}


		} else
			*clip = *s1;


			if (s2->sy > psRendSurface->clip.bottom) {

				if (s1->sy > psRendSurface->clip.bottom) return 0;

				clip++;

				dy = s2->sy - s1->sy;

				if (dy != 0)
					clip->sx = s2->sx - (s2->sx - s1->sx) * (s2->sy - psRendSurface->clip.bottom) / dy;
				else
					clip->sx = s2->sx;

				clip->sy = psRendSurface->clip.bottom;

			// clip uv
				t = ((clip->sy - s1->sy)<<iV_DIVSHIFT) / dy;
				clip->tu = s1->tu + ((t * (s2->tu - s1->tu)) >> iV_DIVSHIFT);
				clip->tv = s1->tv + ((t * (s2->tv - s1->tv)) >> iV_DIVSHIFT);
				clip->sz = s1->sz + ((t * (s2->sz - s1->sz)) >> iV_DIVSHIFT);
				clip->light.byte.r = s1->light.byte.r + ((t * (s2->light.byte.r - s1->light.byte.r)) >> iV_DIVSHIFT);
				clip->light.byte.g = s1->light.byte.g + ((t * (s2->light.byte.g - s1->light.byte.g)) >> iV_DIVSHIFT);
				clip->light.byte.b = s1->light.byte.b + ((t * (s2->light.byte.b - s1->light.byte.b)) >> iV_DIVSHIFT);
				clip->light.byte.a = s1->light.byte.a + ((t * (s2->light.byte.a - s1->light.byte.a)) >> iV_DIVSHIFT);
				if (bClipSpecular)
				{
					clip->specular.byte.r = s1->specular.byte.r + ((t * (s2->specular.byte.r - s1->specular.byte.r)) >> iV_DIVSHIFT);
					clip->specular.byte.g = s1->specular.byte.g + ((t * (s2->specular.byte.g - s1->specular.byte.g)) >> iV_DIVSHIFT);
					clip->specular.byte.b = s1->specular.byte.b + ((t * (s2->specular.byte.b - s1->specular.byte.b)) >> iV_DIVSHIFT);
					clip->specular.byte.a = s1->specular.byte.a + ((t * (s2->specular.byte.a - s1->specular.byte.a)) >> iV_DIVSHIFT);
				}


				n = 2;
			}

			return n;

	} else {
		if (s1->sy > psRendSurface->clip.bottom) {

			if (s2->sy >= psRendSurface->clip.bottom) return 0;

			dy = s1->sy - s2->sy;

			if (dy != 0)
				clip->sx = s1->sx - (s1->sx - s2->sx) * (s1->sy - psRendSurface->clip.bottom) / dy;
			else
				clip->sx = s1->sx;

			clip->sy = psRendSurface->clip.bottom;

			// clip uv
			t = ((clip->sy - s1->sy)<<iV_DIVSHIFT) / dy;
			clip->tu = s1->tu + ((t * (s1->tu - s2->tu)) >> iV_DIVSHIFT);
			clip->tv = s1->tv + ((t * (s1->tv - s2->tv)) >> iV_DIVSHIFT);
			clip->sz = s1->sz + ((t * (s1->sz - s2->sz)) >> iV_DIVSHIFT);
			clip->light.byte.r = s1->light.byte.r + ((t * (s1->light.byte.r - s2->light.byte.r)) >> iV_DIVSHIFT);
			clip->light.byte.g = s1->light.byte.g + ((t * (s1->light.byte.g - s2->light.byte.g)) >> iV_DIVSHIFT);
			clip->light.byte.b = s1->light.byte.b + ((t * (s1->light.byte.b - s2->light.byte.b)) >> iV_DIVSHIFT);
			clip->light.byte.a = s1->light.byte.a + ((t * (s1->light.byte.a - s2->light.byte.a)) >> iV_DIVSHIFT);
			if (bClipSpecular)
			{
				clip->specular.byte.r = s1->specular.byte.r + ((t * (s1->specular.byte.r - s2->specular.byte.r)) >> iV_DIVSHIFT);
				clip->specular.byte.g = s1->specular.byte.g + ((t * (s1->specular.byte.g - s2->specular.byte.g)) >> iV_DIVSHIFT);
				clip->specular.byte.b = s1->specular.byte.b + ((t * (s1->specular.byte.b - s2->specular.byte.b)) >> iV_DIVSHIFT);
				clip->specular.byte.a = s1->specular.byte.a + ((t * (s1->specular.byte.a - s2->specular.byte.a)) >> iV_DIVSHIFT);
			}


		} else
			*clip = *s1;

			if (s2->sy < psRendSurface->clip.top) {

				if (s1->sy < psRendSurface->clip.top) return 0;

				clip++;

				dy = s1->sy - s2->sy;

				if (dy != 0)
					clip->sx = s2->sx + (s1->sx - s2->sx) * (psRendSurface->clip.top - s2->sy) / dy;
				else
					clip->sx = s2->sx;

				clip->sy = psRendSurface->clip.top;

			// clip uv
				t = ((clip->sy - s1->sy)<<iV_DIVSHIFT) / dy;
				clip->tu = s1->tu + ((t * (s1->tu - s2->tu)) >> iV_DIVSHIFT);
				clip->tv = s1->tv + ((t * (s1->tv - s2->tv)) >> iV_DIVSHIFT);
				clip->sz = s1->sz + ((t * (s1->sz - s2->sz)) >> iV_DIVSHIFT);
				clip->light.byte.r = s1->light.byte.r + ((t * (s1->light.byte.r - s2->light.byte.r)) >> iV_DIVSHIFT);
				clip->light.byte.g = s1->light.byte.g + ((t * (s1->light.byte.g - s2->light.byte.g)) >> iV_DIVSHIFT);
				clip->light.byte.b = s1->light.byte.b + ((t * (s1->light.byte.b - s2->light.byte.b)) >> iV_DIVSHIFT);
				clip->light.byte.a = s1->light.byte.a + ((t * (s1->light.byte.a - s2->light.byte.a)) >> iV_DIVSHIFT);
				if (bClipSpecular)
				{
					clip->specular.byte.r = s1->specular.byte.r + ((t * (s1->specular.byte.r - s2->specular.byte.r)) >> iV_DIVSHIFT);
					clip->specular.byte.g = s1->specular.byte.g + ((t * (s1->specular.byte.g - s2->specular.byte.g)) >> iV_DIVSHIFT);
					clip->specular.byte.b = s1->specular.byte.b + ((t * (s1->specular.byte.b - s2->specular.byte.b)) >> iV_DIVSHIFT);
					clip->specular.byte.a = s1->specular.byte.a + ((t * (s1->specular.byte.a - s2->specular.byte.a)) >> iV_DIVSHIFT);
				}

				n = 2;

			}

			return n;
	}
}

int pie_ClipTextured(int npoints, PIEVERTEX *points, PIEVERTEX *clip, BOOL bSpecular)
{
	static PIEVERTEX xclip[iV_POLY_MAX_POINTS+4];
	PIEVERTEX *p0, *p1;
	int n1, n, i;

	bClipSpecular = bSpecular;

	p0 = &points[0];
	p1 = &points[1];

	for (i=0, n1=0; i<npoints; i++, p0++, p1++) {

		if (i==(npoints-1))
			p1 = &points[0];

		if ((p0->sx == 1<<15) || (p0->sy == -1<<15))//check for invalid points jps19aug97
			return 0;

		n1 += pie_ClipXT(p0,p1,&xclip[n1]);
	}

	p0 = &xclip[0];
	p1 = &xclip[1];

	for (i=0, n=0; i<n1; p0++, p1++, i++) {
		if (i==(n1-1))
			p1 = &xclip[0];
		n += pie_ClipYT(p0,p1,&clip[n]);
	}

	return n;
}

//*************************************************************************
/* Alex - much faster tri clipper - won't clip owt else tho' */
int	pie_ClipTexturedTriangleFast(PIEVERTEX *v1, PIEVERTEX *v2, PIEVERTEX *v3, PIEVERTEX *clipped, BOOL bSpecular)
{
	static	PIEVERTEX	xClip[iV_POLY_MAX_POINTS+4];	// plus 4 hopefully is limit?
	static	PIEVERTEX	*p0,*p1;
	UDWORD	numPreY,numAll;
	UDWORD	i;

	bClipSpecular = bSpecular;

	numPreY = 0;
	if( (v1->sx > LONG_TEST) OR (v1->sy > LONG_TEST) )
	{
		/* bomb out for out of range points */
		return(0);
	}
	numPreY += pie_ClipXT(v1,v2,&xClip[numPreY]);

	if( (v2->sx > LONG_TEST) OR (v2->sy > LONG_TEST) )
	{
		/* bomb out for out of range points */
		return(0);
	}
	numPreY += pie_ClipXT(v2,v3,&xClip[numPreY]);

	if( (v3->sx > LONG_TEST) OR (v3->sy > LONG_TEST) )
	{
		/* bomb out for out of range points */
		return(0);
	}
	numPreY += pie_ClipXT(v3,v1,&xClip[numPreY]);

	/* We've now clipped against x axis - now for Y */

	p0 = &xClip[0];
	p1 = &xClip[1];

	for (i=0, numAll=0; i<numPreY; p0++, p1++, i++) {
		if (i==(numPreY-1))
			p1 = &xClip[0];
		numAll += pie_ClipYT(p0,p1,&clipped[numAll]);
	}

	return numAll;
}
