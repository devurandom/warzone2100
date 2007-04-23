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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

// we need BSPIMD defined if we want to read & use the BSP imd files

// Define this if we are compile ivis for the BSP generating tool, also for PIEBIN tool
//#define PIETOOL

#ifdef PIETOOL
#define BSPIMD
#define SAVEIMD
#endif

#include "ivisdef.h"
#include "imd.h"
#include "tex.h"
#include "ivispatch.h"

#ifdef PIETOOL
#include "bspimd.h"
#endif

//*************************************************************************

//*************************************************************************

#define MAX_SHAPE_RADIUS	160

//*************************************************************************


//*************************************************************************

// Output BSP Tree to a file


// This stuff saves out the BSP tree as generated by the BSP tool
//
// Because of the complexities of the BSP generation tool. The IMD is stored in a different
// format when it is generated compared to when it is loaded and used in the game.
// This means that when saving out a BSP tree from the tool other functions need to be included
// most of them are in ptrlist.c which is no longer included in ivis. See the source for BSPIMD
// for this file. Becareful not to confuse this with the same named file in deliverance/src which
// might or might not be the same.
//
#ifdef PIETOOL

// Prototypes for the linked list handling
void *list_GetFirst(PSBSPPTRLIST pList);
void *list_GetNext(PSBSPPTRLIST pList, void *pData);


int BSPPolys, BSPNodes;
PSBSPTREENODE BSPNodeTable[iV_IMD_MAX_POLYS];
iIMDPoly *BSPPolyTable[iV_IMD_MAX_POLYS];


void OutputTriangleList(FILE *fp, PSBSPPTRLIST TriList)
{
	iIMDPoly *Triangle;
	int TriNum=0;
	int d;

	if (TriList == NULL) return;

	Triangle = list_GetFirst(TriList);

	while (Triangle != NULL) {
		fprintf(fp,"\t%8x %d",Triangle->flags,Triangle->npnts);
		for (d=0; d<Triangle->npnts; d++)
			fprintf(fp," %d",Triangle->pindex[d]);

		if (Triangle->flags & iV_IMD_TEXANIM) {

			if (Triangle->pTexAnim == NULL) {
				debug( LOG_NEVER, "No TexAnim pointer!\n" );
			} else {
				fprintf(fp," %d %d %d %d",
					Triangle->pTexAnim->nFrames,
					Triangle->pTexAnim->playbackRate,
					Triangle->pTexAnim->textureWidth,
					Triangle->pTexAnim->textureHeight);
			}
		}

		// if textured write texture uv's
		if (Triangle->flags & (iV_IMD_TEX | iV_IMD_PSXTEX)) {
			for (d = 0; d < Triangle->npnts; d++) {
				fprintf(fp," %d %d",Triangle->vrt[d].u,Triangle->vrt[d].v);
			}
		}
		fprintf(fp, "\n");
		Triangle = list_GetNext(TriList, Triangle);
	}
}


void CountTriangleList(PSBSPPTRLIST TriList)
{
	iIMDPoly *Triangle;

	assert(BSPPolys<iV_IMD_MAX_POLYS);

	if (TriList == NULL) return;
	if (TriList->iNumNodes == 0) return;

	Triangle=list_GetFirst(TriList);

	while (Triangle != NULL) {
		BSPPolyTable[BSPPolys] = Triangle;
		BSPPolys++;

		Triangle = list_GetNext(TriList, Triangle);
	}
}


void CountBSPPolys(PSBSPTREENODE psNode)
{
	if (psNode == NULL) {
		return;
	}

	BSPNodeTable[BSPNodes] = psNode;
	BSPNodes++;

	CountBSPPolys(psNode->link[LEFT]);
	CountTriangleList(psNode->psTriSameDir);
	CountTriangleList(psNode->psTriOppoDir);
	CountBSPPolys(psNode->link[RIGHT]);
}


void OutputBSPPolys(FILE *fp, PSBSPTREENODE psNode)
{
	if (psNode == NULL) {
		return;
	}
	OutputBSPPolys(fp, psNode->link[LEFT]);
	OutputTriangleList(fp, psNode->psTriSameDir);
	OutputTriangleList(fp, psNode->psTriOppoDir);
	OutputBSPPolys(fp, psNode->link[RIGHT]);
}


int HuntNodeList(PSBSPTREENODE psNode)
{
	int i;

	for (i = 0; i < BSPNodes; i++) {
		if (BSPNodeTable[i] == psNode) {
			return i;
		}
	}
	return -1;
}


int HuntPolyList(iIMDPoly *Poly)
{
	int i;

	for (i = 0; i < BSPPolys; i++) {
		if (BSPPolyTable[i] == Poly) {
			return i;
		}
	}
	return 999;
}


void DumpTriangleList(FILE *fp,PSBSPPTRLIST TriList)
{

	iIMDPoly *Triangle;

	assert(BSPPolys<iV_IMD_MAX_POLYS);

//	DBPRINTF(("Dumping TriList %p\n",TriList));

	if (TriList==NULL) return;

	Triangle = list_GetFirst(TriList);

	while (Triangle != NULL) {
		fprintf(fp,"%d ",HuntPolyList(Triangle));
		Triangle=list_GetNext(TriList,Triangle);
	}
	fprintf(fp,"-1 ");
}


void OutputBSPNodes(FILE *fp, PSBSPTREENODE psNode)
{
	if (psNode == NULL) {
		return;
	}
	fprintf(fp,"\t%d ",HuntNodeList(psNode->link[LEFT]));
	DumpTriangleList( fp, psNode->psTriSameDir );
	DumpTriangleList( fp, psNode->psTriOppoDir );
	fprintf(fp,"%d\n",HuntNodeList(psNode->link[RIGHT]));

	OutputBSPNodes( fp, psNode->link[LEFT]);

	OutputBSPNodes( fp, psNode->link[RIGHT]);
}


/*

	Count Polys & Nodes in bsp - also makes a table of them

*/
int GetBSPPolyCount(PSBSPTREENODE psNode, int *NodeCount)
{
	BSPPolys=0;
	BSPNodes=0;
	CountBSPPolys(psNode);
	*NodeCount=BSPNodes;
	return BSPPolys;
}

#endif
#ifdef SAVEIMD
// new code the write out the connectors !
void _imd_save_connectors(FILE *fp, iIMDShape *s)
{
	Vector3i *p;
	int i;

	if (s->nconnectors != 0) {
		fprintf(fp,"CONNECTORS %d\n",s->nconnectors);
		p = s->connectors;
		for (i=0; i<s->nconnectors; i++, p++) {
			fprintf(fp,"\t%d %d %d\n", p->x,p->y,p->z);
		}
	}
}


//*************************************************************************
//*** save IMD file
//*
//* pre		shape successfully loaded
//*
//* params	filename = name of file to save to including .IMD extention
//*			s 			= pointer to IMD shape
//*
//* returns TRUE -> ok, FLASE -> error
//*
//******
BOOL iV_IMDSave(char *filename, iIMDShape *s, BOOL PieIMD)
{
	FILE *fp;
	iIMDShape *sp;
	iIMDPoly *poly;
	int nlevel, i, j, d;

	if ((fp = fopen(filename,"w")) == NULL) {
		return FALSE;
  }

	if (PieIMD==TRUE) {
		fprintf(fp,"%s %d\n",PIE_NAME,PIE_VER);
	} else {
		fprintf(fp,"%s %d\n",IMD_NAME,IMD_VER);
	}
	fprintf(fp,"TYPE %x\n",s->flags);

	// if textured write tex page file info
	if (s->flags & iV_IMD_XTEX) {
		fprintf(fp,"TEXTURE %d %s %d %d\n",iV_TEXTYPE(s->texpage),
				iV_TEXNAME(s->texpage),iV_TEXWIDTH(s->texpage),
				iV_TEXHEIGHT(s->texpage));
	}

	// find number of levels in shape
	for (nlevel=0, sp = s; sp != NULL; sp = sp->next, nlevel++)
		;

	fprintf(fp,"LEVELS %d\n",nlevel);

	for (sp = s, i=0; i<nlevel; sp = sp->next, i++) {
		fprintf(fp,"LEVEL %d\n",(i+1));
		fprintf(fp,"POINTS %d\n",sp->npoints);

		// write shape points
		for (j = 0; j < sp->npoints; j++) {
			fprintf(fp,"\t%d %d %d\n",sp->points[j].x,sp->points[j].y,
					sp->points[j].z);
		}

		// write shape polys
#ifdef PIETOOL
		if (sp->BSPNode==NULL)
#endif
		{
			fprintf(fp,"POLYGONS %d\n",sp->npolys);
			for (poly = sp->polys, j=0; j<sp->npolys; j++, poly++) {
				fprintf(fp,"\t%8x %d",poly->flags,poly->npnts);
				for (d=0; d<poly->npnts; d++) {
					fprintf(fp," %d",poly->pindex[d]);
				}

				if (poly->flags & iV_IMD_TEXANIM) {

					if (poly->pTexAnim == NULL) {
						debug( LOG_3D, "No TexAnim pointer!\n" );
					} else {
						fprintf(fp," %d %d %d %d",
							poly->pTexAnim->nFrames,
							poly->pTexAnim->playbackRate,
							poly->pTexAnim->textureWidth,
							poly->pTexAnim->textureHeight);
					}
				}

				// if textured write texture uv's
				if (poly->flags & (iV_IMD_TEX | iV_IMD_PSXTEX)) {
					for (d=0; d<poly->npnts; d++) {
						fprintf(fp," %d %d",poly->vrt[d].u,poly->vrt[d].v);
					}
				}
				fprintf(fp,"\n");
			}
		}
#ifdef PIETOOL
		else
		{
			int NodeCount;

			fprintf(fp,"POLYGONS %d\n",GetBSPPolyCount(sp->BSPNode,&NodeCount) );
			OutputBSPPolys(fp,sp->BSPNode);	//

			fprintf(fp,"BSP %d\n",NodeCount );
			OutputBSPNodes(fp,sp->BSPNode);
		}
#endif

	}

	_imd_save_connectors(fp,s);	// Write out the connectors if they exist

	fclose(fp);

	return TRUE;
}
#endif

//*************************************************************************
//*** free IMD shape memory
//*
//* pre		shape successfully allocated
//*
//* params	shape = pointer to IMD shape
//*
//******
void iV_IMDRelease(iIMDShape *s)
{
   int i;
   iIMDShape *d;

   if (s) {
		if (s->points) {
			free(s->points);
		}
		if (s->connectors) {
			free(s->connectors);
		}
#ifdef BSPIMD
		if (s->BSPNode) {
				free(s->BSPNode);	// I used malloc() so i'm going to use FREE()
		}
#endif
		if (s->polys) {
			for (i = 0; i < s->npolys; i++) {
				if (s->polys[i].pindex) {
					free(s->polys[i].pindex);
				}
				if (s->polys[i].pTexAnim) {
					free(s->polys[i].pTexAnim);
				}
				if (s->polys[i].vrt) {
					free(s->polys[i].vrt);
				}
			}
			free(s->polys);
		}
		if (s->shadowEdgeList)
		{
			free(s->shadowEdgeList);
			s->shadowEdgeList = NULL;
		}
		iV_DEBUG0("imd[IMDRelease] = release successful\n");
		d = s->next;
		free(s);
		iV_IMDRelease(d);
	}
}
