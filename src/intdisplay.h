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

#ifndef __INCLUDED_SRC_INTDISPLAY_H__
#define __INCLUDED_SRC_INTDISPLAY_H__

#include "lib/widget/widget.h"
#include "lib/widget/form.h"
#include "intimage.h"
#include "droid.h"

#define NUM_OBJECTSURFACES		(100)
#define NUM_TOPICSURFACES		(50)
#define NUM_STATSURFACES		(200)
#define NUM_SYSTEM0SURFACES		(100)
#define NUM_OBJECTBUFFERS		(NUM_OBJECTSURFACES*4)
#define NUM_STATBUFFERS			(NUM_STATSURFACES*4)
#define NUM_TOPICBUFFERS		(NUM_TOPICSURFACES*4)

#define NUM_SYSTEM0BUFFERS		(NUM_SYSTEM0SURFACES*8)


/* Power levels are divided by this for power bar display. The extra factor has
been included so that the levels appear the same for the power bar as for the
power values in the buttons */
#define POWERBAR_SCALE			(5 * WBAR_SCALE/STAT_PROGBARWIDTH)

#define BUTTONOBJ_ROTSPEED		90	// Speed to rotate objects rendered in
									// buttons ( degrees per second )

//the two types of button used in the object display (bottom bar)
#define		TOPBUTTON			0
#define		BTMBUTTON			1


enum {
	IMDTYPE_NONE,
	IMDTYPE_DROID,
	IMDTYPE_DROIDTEMPLATE,
	IMDTYPE_COMPONENT,
	IMDTYPE_STRUCTURE,
	IMDTYPE_RESEARCH,
	IMDTYPE_STRUCTURESTAT,
};

typedef struct {
	char *Token;
	SWORD ID;
} TOKENID;

typedef struct {
	char *Token;
	SWORD ID;
	SWORD IMD;
} RESEARCHICON;


typedef struct {
	UBYTE *Buffer;		// Bitmap buffer.
	iSurface *Surface;	// Ivis surface definition.
} BUTTON_SURFACE;


#define RENDERBUTTON_INUSE(x)  ((x)->InUse=true)
#define RENDERBUTTON_NOTINUSE(x)  ((x)->InUse=false)

#define RENDERBUTTON_INITIALISED(x)  ((x)->Initialised=true)
#define RENDERBUTTON_NOTINITIALISED(x)  ((x)->Initialised=false)

#define IsBufferInitialised(x) ((x)->Initialised)
#define IsBufferInUse(x) ((x)->InUse)

typedef struct {
	BOOL InUse;			// Is it in use.
	BOOL Initialised;	// Is it initialised.
	SDWORD ImdRotation;	// Rotation if button is an IMD.
	UDWORD State;		// Copy of widget's state so we know if state has changed.
	void *Data;			// Any data we want to attach.
	void *Data2;		// Any data we want to attach.
	BUTTON_SURFACE *ButSurf;	// Surface to render the button into.
//	uint8 *Buffer;		// Bitmap buffer.
//	iSurface *Surface;	// Ivis surface definition.
} RENDERED_BUTTON;

extern RENDERED_BUTTON TopicBuffers[NUM_TOPICBUFFERS];
extern RENDERED_BUTTON ObjectBuffers[NUM_OBJECTBUFFERS];
extern RENDERED_BUTTON StatBuffers[NUM_STATBUFFERS];
extern RENDERED_BUTTON System0Buffers[NUM_SYSTEM0BUFFERS];

extern UDWORD ManuPower;		// Power required to manufacture the current item.
extern BASE_STATS *CurrentStatsTemplate;

// Set audio IDs for form opening/closing anims.
void SetFormAudioIDs(int OpenID,int CloseID);

// Initialise interface graphics.
void intInitialiseGraphics(void);

// Free up interface graphics.
void intDeleteGraphics(void);

// Intialise button surfaces.
void InitialiseButtonData(void);

// Free up button surfaces.
void DeleteButtonData(void);

// Get a free RENDERED_BUTTON structure for an object window button.
SDWORD GetObjectBuffer(void);

// Clear ( make unused ) all RENDERED_BUTTON structures for the object window.
void ClearObjectBuffers(void);

// Clear ( make unused ) all RENDERED_BUTTON structures for the topic window.
void ClearTopicBuffers(void);

// Clear ( make unused ) a RENDERED_BUTTON structure.
void ClearObjectButtonBuffer(SDWORD BufferID);

// Clear ( make unused ) a RENDERED_BUTTON structure.
void ClearTopicButtonBuffer(SDWORD BufferID);

void RefreshObjectButtons(void);
void RefreshSystem0Buttons(void);
void RefreshTopicButtons(void);
void RefreshStatsButtons(void);


// Get a free RENDERED_BUTTON structure for a stat window button.
SDWORD GetStatBuffer(void);

// Clear ( make unused ) all RENDERED_BUTTON structures for the stat window.
void ClearStatBuffers(void);

/*these have been set up for the Transporter - the design screen DOESN'T use them*/
// Clear ( make unused ) *all* RENDERED_BUTTON structures.
void ClearSystem0Buffers(void);
// Clear ( make unused ) a RENDERED_BUTTON structure.
void ClearSystem0ButtonBuffer(SDWORD BufferID);
// Get a free RENDERED_BUTTON structure.
SDWORD GetSystem0Buffer(void);

// callback to update the command droid size label
void intUpdateCommandSize(WIDGET *psWidget, W_CONTEXT *psContext);

// callback to update the command droid experience
void intUpdateCommandExp(WIDGET *psWidget, W_CONTEXT *psContext);

// callback to update the command droid factories
void intUpdateCommandFact(WIDGET *psWidget, W_CONTEXT *psContext);

void intUpdateProgressBar(WIDGET *psWidget, W_CONTEXT *psContext);

void intUpdateOptionText(WIDGET *psWidget, W_CONTEXT *psContext);

void intUpdateQuantity(WIDGET *psWidget, W_CONTEXT *psContext);
//callback to display the factory number
extern void intAddFactoryInc(WIDGET *psWidget, W_CONTEXT *psContext);
//callback to display the production quantity number for a template
extern void intAddProdQuantity(WIDGET *psWidget, W_CONTEXT *psContext);

void intDisplayPowerBar(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayStatusButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayObjectButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayStatsButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void AdjustTabFormSize(W_TABFORM *Form,UDWORD *x0,UDWORD *y0,UDWORD *x1,UDWORD *y1);

void intDisplayStatsForm(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intOpenPlainForm(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intClosePlainForm(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayPlainForm(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayImage(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayImageHilight(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayButtonHilight(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayButtonFlash(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayButtonPressed(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayReticuleButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayTab(WIDGET *psWidget,UDWORD TabType, UDWORD Position,
					UDWORD Number,BOOL Selected,BOOL Hilight,UDWORD x,UDWORD y,UDWORD Width,UDWORD Height);
void intDisplaySlider(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void intDisplayNumber(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);
void intAddLoopQuantity(WIDGET *psWidget, W_CONTEXT *psContext);

void intDisplayEditBox(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

void OpenButtonRender(UWORD XPos,UWORD YPos,UWORD Width,UWORD Height);
void CloseButtonRender(void);

void ClearButton(BOOL Down,UDWORD Size, UDWORD buttonType);

void RenderToButton(IMAGEFILE *ImageFile,UWORD ImageID,void *Object,UDWORD Player,RENDERED_BUTTON *Buffer,
					BOOL Down,UDWORD IMDType, UDWORD buttonType);

void CreateIMDButton(IMAGEFILE *ImageFile,UWORD ImageID,void *Object,UDWORD Player,RENDERED_BUTTON *Buffer,
					BOOL Down,UDWORD IMDType,UDWORD buttonType);

void CreateImageButton(IMAGEFILE *ImageFile,UWORD ImageID,RENDERED_BUTTON *Buffer,BOOL Down, UDWORD buttonType);

void CreateBlankButton(RENDERED_BUTTON *Buffer,BOOL Down, UDWORD buttonType);

void RenderImageToButton(IMAGEFILE *ImageFile,UWORD ImageID,RENDERED_BUTTON *Buffer,BOOL Down, UDWORD buttonType);
void RenderBlankToButton(RENDERED_BUTTON *Buffer,BOOL Down, UDWORD buttonType);


extern BOOL DroidIsRepairing(DROID *Droid);

BOOL DroidIsBuilding(DROID *Droid);
STRUCTURE *DroidGetBuildStructure(DROID *Droid);
BOOL DroidGoingToBuild(DROID *Droid);
BASE_STATS *DroidGetBuildStats(DROID *Droid);
iIMDShape *DroidGetIMD(DROID *Droid);
UDWORD DroidGetIMDIndex(DROID *Droid);
BOOL DroidIsDemolishing(DROID *Droid);

BOOL StructureIsManufacturing(STRUCTURE *Structure);
RESEARCH_FACILITY *StructureGetResearch(STRUCTURE *Structure);
BOOL StructureIsResearching(STRUCTURE *Structure);
FACTORY *StructureGetFactory(STRUCTURE *Structure);
iIMDShape *StructureGetIMD(STRUCTURE *Structure);

DROID_TEMPLATE *FactoryGetTemplate(FACTORY *Factory);

//iIMDShape *TemplateGetIMD(DROID_TEMPLATE *DroidTemp,UDWORD Player);
//UDWORD TemplateGetIMDIndex(DROID_TEMPLATE *Template,UDWORD Player);

//SDWORD ResearchGetImage(RESEARCH_FACILITY *Research);

BOOL StatIsStructure(BASE_STATS *Stat);
iIMDShape *StatGetStructureIMD(BASE_STATS *Stat,UDWORD Player);
BOOL StatIsTemplate(BASE_STATS *Stat);
BOOL StatIsFeature(BASE_STATS *Stat);

SDWORD StatIsComponent(BASE_STATS *Stat);
BOOL StatGetComponentIMD(BASE_STATS *Stat, SDWORD compID,iIMDShape **CompIMD,iIMDShape **MountIMD);

BOOL StatIsResearch(BASE_STATS *Stat);
//void StatGetResearchImage(BASE_STATS *Stat,SDWORD *Image,iIMDShape **Shape, BOOL drawTechIcon);
void StatGetResearchImage(BASE_STATS *psStat, SDWORD *Image, iIMDShape **Shape,
                          BASE_STATS **ppGraphicData, BOOL drawTechIcon);

/* Draws a stats bar for the design screen */
extern void intDisplayStatsBar(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset,
							   PIELIGHT *pColours);
/* Draws a Template Power Bar for the Design Screen */
void intDisplayDesignPowerBar(WIDGET *psWidget, UDWORD xOffset,
							  UDWORD yOffset, PIELIGHT *pColours);

// Widget callback function to play an audio track.
extern void WidgetAudioCallback(int AudioID);

// Widget callback to display a contents button for the Transporter
extern void intDisplayTransportButton(WIDGET *psWidget, UDWORD xOffset,
						  UDWORD yOffset, PIELIGHT *pColours);
/*draws blips on radar to represent Proximity Display*/
extern void drawRadarBlips(float pixSizeH, float pixSizeV, int RadarOffsetX, int RadarOffsetY);

/*Displays the proximity messages blips over the world*/
extern void intDisplayProximityBlips(WIDGET *psWidget, UDWORD xOffset,
					UDWORD yOffset, PIELIGHT *pColours);

extern void intUpdateQuantitySlider(WIDGET *psWidget, W_CONTEXT *psContext);



extern void intDisplayDPButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

extern void intDisplayTime(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);
extern void intDisplayNum(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

extern void intDisplayResSubGroup(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

extern void intDisplayMissionClock(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

extern void intDisplayAllyIcon(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, PIELIGHT *pColours);

#endif // __INCLUDED_SRC_INTDISPLAY_H__
