/***************************************************************************/
/*
 * piefunc.c
 *
 * extended render routines for 3D rendering
 *
 */
/***************************************************************************/

#include <string.h>
#ifdef _MSC_VER
#include <windows.h>  //needed for gl.h!  --Qamly
#endif
#include <SDL/SDL_opengl.h>

#include "lib/framework/frame.h"
#include "lib/gamelib/gtime.h"
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/rendmode.h"
#include "lib/ivis_common/piefunc.h"
#include "lib/ivis_common/piestate.h"
#include "piematrix.h"
#include "pietexture.h"
#include "lib/ivis_common/pieclip.h"

/***************************************************************************/
/*
 *	Local Definitions
 */
/***************************************************************************/

/***************************************************************************/
/*
 *	Local Variables
 */
/***************************************************************************/
static PIEVERTEX	pieVrts[pie_MAX_POLY_VERTS];
static PIEVERTEX	clippedVrts[pie_MAX_POLY_VERTS];
static UBYTE		aByteScale[256][256];

/***************************************************************************/
/*
 *	Local ProtoTypes
 */
/***************************************************************************/

/***************************************************************************/
/*
 *	Source
 */
/***************************************************************************/

void pie_DownLoadBufferToScreen(void *pSrcData, UDWORD destX, UDWORD destY,UDWORD srcWidth,UDWORD srcHeight,UDWORD srcStride)
{
	/*
	if (pie_GetRenderEngine() == ENGINE_OPENGL)	//Was ENGINE_D3D -Q
	{
		pie_D3DSetupRenderForFlip(destX, destY, pSrcData, srcWidth, srcHeight, srcStride);
	}
	*/
	return;
}

/***************************************************************************/
/*
 *	void pie_RectFilter(SDWORD x0, SDWORD y0, SDWORD x1, SDWORD y1, UDWORD colour)
 *
 * Draws rectangular filter to screen ivis mode defaults to
 *
 */
/***************************************************************************/
void pie_RectFilter(SDWORD x0, SDWORD y0, SDWORD x1, SDWORD y1, UDWORD colour)
{
	iV_TransBoxFill(x0, y0, x1, y1);
}

/* ---------------------------------------------------------------------------------- */
void	pie_CornerBox(SDWORD x0, SDWORD y0, SDWORD x1, SDWORD y1, UDWORD colour,
					  UBYTE a, UBYTE b, UBYTE c, UBYTE d)
{
}

/* ---------------------------------------------------------------------------------- */

void	pie_DrawViewingWindow(iVector *v, UDWORD x1, UDWORD y1, UDWORD x2, UDWORD y2, UDWORD colour)
{
	SDWORD clip, i;

	pie_SetTexturePage(-1);
	pie_SetRendMode(REND_ALPHA_FLAT);
//PIE verts
	pieVrts[0].sx = v[1].x;
	pieVrts[0].sy = v[1].y;
	//cull triangles with off screen points
	pieVrts[0].sz  = INTERFACE_DEPTH;


	pieVrts[0].tu = 0.0;
	pieVrts[0].tv = 0.0;
	pieVrts[0].light.argb = colour;//0x7fffffff;
	pieVrts[0].specular.argb = 0;

	memcpy(&pieVrts[1],&pieVrts[0],sizeof(PIEVERTEX));
	memcpy(&pieVrts[2],&pieVrts[0],sizeof(PIEVERTEX));
	memcpy(&pieVrts[3],&pieVrts[0],sizeof(PIEVERTEX));
	memcpy(&pieVrts[4],&pieVrts[0],sizeof(PIEVERTEX));

	pieVrts[1].sx = v[0].x;
	pieVrts[1].sy = v[0].y;

	pieVrts[2].sx = v[2].x;
	pieVrts[2].sy = v[2].y;

	pieVrts[3].sx = v[3].x;
	pieVrts[3].sy = v[3].y;

	pie_Set2DClip(x1,y1,x2-1,y2-1);
	clip = pie_ClipTextured(4, &pieVrts[0], &clippedVrts[0], FALSE);
	pie_Set2DClip(CLIP_BORDER,CLIP_BORDER,psRendSurface->width-CLIP_BORDER,psRendSurface->height-CLIP_BORDER);

	if (clip >= 3) {
		PIELIGHT c;

		c.argb = colour;
		glBegin(GL_TRIANGLE_FAN);
		glColor4ub(c.byte.r, c.byte.g, c.byte.b, c.byte.a >> 1);
		for (i = 0; i < clip; i++) {
			glVertex2f(clippedVrts[i].sx, clippedVrts[i].sy);
		}
		glEnd();
		glBegin(GL_LINE_STRIP);
		glColor4ub(c.byte.r, c.byte.g, c.byte.b, c.byte.a);
		for (i = 0; i < clip; i++) {
			glVertex2f(clippedVrts[i].sx, clippedVrts[i].sy);
		}
		glVertex2f(clippedVrts[0].sx, clippedVrts[0].sy);
		glEnd();
	}
}

/* ---------------------------------------------------------------------------------- */
void pie_TransColouredTriangle(PIEVERTEX *vrt, UDWORD rgb, UDWORD trans)
{
        PIELIGHT c;
	UDWORD i;

	c.argb = rgb;

	pie_SetTexturePage(-1);
	pie_SetRendMode(REND_ALPHA_ITERATED);

        glBegin(GL_TRIANGLE_FAN);
        glColor4ub(c.byte.r, c.byte.g, c.byte.b, 128);
        for (i = 0; i < 3; ++i)
        {
		glVertex3f(vrt[i].sx, vrt[i].sy, vrt[i].sz);
	}
        glEnd();}


/* ---------------------------------------------------------------------------------- */

void pie_InitMaths(void)
{
	UBYTE c;
	UDWORD a,b,bigC;

	for(a=0; a<=UBYTE_MAX; a++)
	{
		for(b=0; b<=UBYTE_MAX; b++)
		{
			bigC = a * b;
			bigC /= UBYTE_MAX;
			ASSERT( bigC <= UBYTE_MAX,"light_InitMaths; rounding error" );
			c = (UBYTE)bigC;
			aByteScale[a][b] = c;
		}
	}
}

UBYTE pie_ByteScale(UBYTE a, UBYTE b)
{
	return (((UDWORD)a)*((UDWORD)b))>>8;
}

void	pie_doWeirdBoxFX(UDWORD x, UDWORD y, UDWORD x2, UDWORD y2, UDWORD	trans)
{
UDWORD	val;
UDWORD	xDif;
UDWORD	yDif;
UDWORD	radius;

	val = getTimeValueRange(5760, 360);
	radius = 100;
	xDif = radius * (SIN(DEG(val)));
	yDif = radius * (COS(DEG(val)));

	xDif = xDif/4096;	 // cos it's fixed point
	yDif = yDif/4096;

 	pie_SetDepthBufferStatus(DEPTH_CMP_ALWAYS_WRT_ON);
   	pie_CornerBox(x,y,x2,y2,trans,20+(radius+xDif),20+(radius+yDif),20+(radius-xDif),20+(radius-yDif));
	/*
	val = 360-getTimeValueRange(2880,360);
	xDif = radius * (SIN(DEG(val)));
	yDif = radius * (COS(DEG(val)));

	xDif = xDif/4096;	 // cos it's fixed point
	yDif = yDif/4096;
//	pie_BoxFill(100,100,200,200,234);
   	pie_CornerBox(x,y,x2,y2,trans,20+(radius+xDif),20+(radius+yDif),20+(radius-xDif),20+(radius-yDif));
   	pie_SetDepthBufferStatus(DEPTH_CMP_LEQ_WRT_ON);
	*/
}
