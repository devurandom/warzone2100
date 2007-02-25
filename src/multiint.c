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
/*
 * MultiInt.c
 *
 * Alex Lee, 98. Pumpkin Studios, Bath.
 * Functions to display and handle the multiplayer interface screens
 * Arena and Campaign styles, along with connection and game options.
 */

#include <stdio.h>		// get rid of a couple of warnings.
#include <string.h>
#ifdef WIN32
#include <windows.h>
#endif
#include <SDL/SDL_opengl.h>
#include <physfs.h>

#include "lib/framework/frame.h"
#include "lib/framework/frameresource.h"
#include "lib/framework/strres.h"
#include "lib/framework/frameint.h"
// FIXME Direct iVis implementation include!
#include "lib/ivis_opengl/screen.h"
#include "lib/widget/widget.h"

#include "winmain.h"
#include "objects.h"
#include "display.h"// pal stuff
#include "display3d.h"

/* Includes direct access to render library */
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/piestate.h"
#include "lib/ivis_common/pieclip.h"
// FIXME Direct iVis implementation include!
#include "lib/ivis_common/rendmode.h"
#include "objmem.h"
#include "gateway.h"
#include <time.h>
#include "lib/gamelib/gtime.h"
#include "text.h"
#include "configuration.h"
#include "intdisplay.h"
#include "design.h"
#include "hci.h"
#include "csnap.h"
#include "power.h"
#include "loadsave.h"			// for blueboxes.
// FIXME Direct iVis implementation include!
#include "lib/ivis_opengl/piematrix.h"			// for setgeometricoffset
#include "component.h"
#include "map.h"
#include "console.h"			// chat box stuff
#include "frend.h"
#include "advvis.h"
//#include "editbox.h"
#include "frontend.h"
//#include "texture.h"
#include "data.h"
#include "lib/script/script.h"
#include "keymap.h"
#include "game.h"

#include "lib/netplay/netplay.h"
#include "multiplay.h"
#include "multiint.h"
#include "multijoin.h"
#include "multistat.h"
#include "multirecv.h"
#include "multimenu.h"

#include "init.h"
#include "levels.h"

#include <jpeglib.h>

// ////////////////////////////////////////////////////////////////////////////
// vars
extern char	MultiForcesPath[255];
extern char	MultiCustomMapsPath[255];
extern char	MultiPlayersPath[255];

extern BOOL				bSendingMap;

extern void intDisplayTemplateButton(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);

extern BOOL NETsetupTCPIP(void ** addr, char * machine);

BOOL						bHosted			= FALSE;				//we have set up a game
char						sPlayer[128];							// player name (to be used)
char						buildTime[8]	 = "67HGDV3"; //RODZ was __TIME__ ;
static BOOL					bColourChooserUp= FALSE;
static BOOL					bTeamChooserUp[MAX_PLAYERS]= {FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE};
SDWORD						playerTeamGUI[MAX_PLAYERS] = {0,1,2,3,4,5,6,7};		//team each player belongs to (in skirmish setup screen)
SDWORD						playerTeam[MAX_PLAYERS] = {-1,-1,-1,-1,-1,-1,-1,-1};		//team each player belongs to (in the game)
static SWORD				SettingsUp		= 0;
static UBYTE				InitialProto	= 0;
static W_SCREEN				*psConScreen;
static SDWORD				dwSelectedGame	=0;						//player[] and games[] indexes
static UDWORD				gameNumber;								// index to games icons
static BOOL					safeSearch		= FALSE;				// allow auto game finding.

//whiteboard stuff
#define	NUMWHITE			150
#define WHITEW				(MULTIOP_CHATBOXW-10)
#define WHITEH				(MULTIOP_CHATBOXH-25)
#define WHITEX				(MULTIOP_CHATBOXX+5+D_W)		//(639-WHITEW)
#define WHITEY				(MULTIOP_CHATBOXY+5+D_H)		//(479-WHITEH)

static	W_SCREEN			*psWhiteScreen;
static	BOOL				bWhiteBoardUp;
UWORD						whiteBoard[MAX_PLAYERS][NUMWHITE];
static	UBYTE				curWhite = 0;

static UDWORD hideTime=0;

#define FORCEEDIT_POWER		1200

#define DEFAULTCAMPAIGNMAP	"Rush"
#define DEFAULTSKIRMISHMAP	"Sk-Rush"


/// end of globals.
// ////////////////////////////////////////////////////////////////////////////
// Function protos

// widget functions
BOOL		addMultiBut					(W_SCREEN *screen, UDWORD formid,UDWORD id,UDWORD x, UDWORD y, UDWORD width, UDWORD height,UDWORD tipres,UDWORD norm, UDWORD hi,BOOL showmouseover);
BOOL		addMultiEditBox				(UDWORD formid,UDWORD id,UDWORD x, UDWORD y, UDWORD tip, char tipres[128],UDWORD icon,UDWORD iconid);
static void addBlueForm					(UDWORD parent,UDWORD id,char *txt,UDWORD x,UDWORD y,UDWORD w,UDWORD h);

// Drawing Functions
void		displayChatEdit				(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayMultiBut				(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayWhiteBoard			(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		intDisplayFeBox				(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayRemoteGame			(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayPlayer				(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayTeamChooser				(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayMultiEditBox			(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		displayForceDroid			(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
void		setLockedTeamsMode			(void);

// find games
static void addGames				(void);
void		runGameFind				(void);
void		startGameFind			(void);

// Connection option functions
static BOOL	OptionsInet				(UDWORD);
static void addConnections			(UDWORD);
void		runConnectionScreen		(void);
BOOL		startConnectionScreen	(void);

// Game option functions
static	void	addOkBut			(void);
static	void	addGameOptions		(BOOL bRedo);				// options (rhs) boxV
UDWORD	addPlayerBox		(BOOL);				// players (mid) box
static	void	addChatBox			(void);
static	void	disableMultiButs	(void);
static	void	processMultiopWidgets(UDWORD);
static	void	SendFireUp			(void);
void	kickPlayer			(UDWORD dpid);
void			runMultiOptions		(void);
BOOL			startMultiOptions	(BOOL);
void			frontendMultiMessages(void);

// whiteboard funcs
static	BOOL	removeWhiteBoard	(void);
static	BOOL	addWhiteBoard		(void);
static	BOOL	runWhiteBoard		(void);

static	UDWORD	bestPlayer			(UDWORD);
static	void	decideWRF			(void);

static void		closeColourChooser	(void);
static void		closeTeamChooser	(void);
static BOOL		SendColourRequest	(UDWORD player, UBYTE col,UBYTE chosenPlayer);
static BOOL		safeToUseColour		(UDWORD player,UDWORD col);
BOOL			chooseColour		(UDWORD);
BOOL			recvColourRequest	(NETMSG *pMsg);

// Force selection functions
static void		AvailableForces		(void);				// draw available templates
static void		CurrentForce		(void);				// draw the current force

// ////////////////////////////////////////////////////////////////////////////
// map previews..

void loadMapPreview(void)
{
	char			aFileName[256];
	UDWORD			fileSize;
	char			*pFileData = NULL;
	LEVEL_DATASET	*psLevel;

	UDWORD			i, j, x, y, height, offX2, offY2;
	UBYTE			scale,col;
	MAPTILE			*psTile,*WTile;
	UDWORD oursize;
	char  *ptr, *imageData;

	if(psMapTiles)
	{
		mapShutdown();
	}

	levFindDataSet(game.map, &psLevel);
	rebuildSearchPath(psLevel->dataDir, FALSE);
	strcpy(aFileName,psLevel->apDataFiles[0]);
	aFileName[strlen(aFileName)-4] = '\0';
	strcat(aFileName, "/game.map");

	pFileData = DisplayBuffer;
	if (!loadFileToBuffer(aFileName, pFileData, displayBufferSize, &fileSize))
	{
		debug(LOG_NEVER, "loadMapPreview: Failed to load file");
		return;
	}
	if (!mapLoad(pFileData, fileSize))
	{
		debug(LOG_NEVER, "loadMapPreview: Failed to load map");
		return;
	}
	gwShutDown();

	scale =1;
	if((mapHeight <  240)&&(mapWidth < 320))
	{
		scale = 2;
	}
	if((mapHeight <  120)&&(mapWidth < 160))
	{
		scale = 3;
	}
	if((mapHeight <  60)&&(mapWidth < 80))
	{
		scale = 4;
	}
	if((mapHeight <  30)&&(mapWidth < 40))
	{
		scale = 5;
	}
	oursize = sizeof(char) * BACKDROP_HACK_WIDTH * BACKDROP_HACK_HEIGHT;
	imageData = (char*)malloc(oursize * 3);
	ptr = imageData;
	memset(ptr, 0x45, sizeof(char) * BACKDROP_HACK_WIDTH * BACKDROP_HACK_HEIGHT * 3); //dunno about background color
	psTile = psMapTiles;
	offX2 = (BACKDROP_HACK_WIDTH / 2) - ((scale * mapWidth) / 2);
	offY2 = (BACKDROP_HACK_HEIGHT / 2 )  - ((scale * mapHeight) / 2);

	for (i = 0; i < mapHeight; i++)
	{
		WTile = psTile;
		for (j = 0; j < mapWidth; j++)
		{
			height = WTile->height;
			col = height;

			for (x = (j * scale); x < (j * scale) + scale; x++)
			{
				for (y = (i * scale); y < (i * scale) + scale; y++)
				{
					imageData[3 * ((offY2 + y) * BACKDROP_HACK_WIDTH + (x + offX2))] = col;
					imageData[3 * ((offY2 + y) * BACKDROP_HACK_WIDTH + (x + offX2)) + 1] = col;
					imageData[3 * ((offY2 + y) * BACKDROP_HACK_WIDTH + (x + offX2)) + 2] = col;
				}
			}
			WTile += 1;
		}
		psTile += mapWidth;
	}
	plotStructurePreview16(imageData, scale, offX2, offY2);
	screen_Upload(imageData);
	free(imageData);

	hideTime = gameTime;
	mapShutdown();
}

#ifdef REMOVE_WHEN_NOBODY_FINDS_THIS_INTERESTING_ANYMORE
// leave alone for now please -Q
// I know this don't belong here, but I am using this for testing.
static void Show_Map(char *imagedata)
{
	GLuint Tex;
//		SDL_GL_SwapBuffers();
	pie_image image;
	image_init(&image);
//	imagetest=malloc((sizeof(char)*512*512*512));
	image_load_from_jpg(&image, "texpages/bdrops/test1.jpg");
	glGenTextures(1, &Tex);
	pie_SetTexturePage(-1);
	glBindTexture(GL_TEXTURE_2D, Tex);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB,
		    512,512,//backDropWidth, backDropHeight,
			0, GL_RGB, GL_UNSIGNED_BYTE, imagedata);//image.data);
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	image_delete(&image);
//	free(image);
	glDisable(GL_DEPTH_TEST);
	glDepthMask(GL_FALSE);
	pie_SetTexturePage(-1);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, Tex);
	glColor3f(1, 1, 1);
	glPushMatrix();
	glLoadIdentity();
//	glTranslatef(0,0,-13);
//	glBegin(GL_QUADS);
//		glVertex3f(-1.0f, 1.0f, 0.0f);				// Top Left
//		glVertex3f( 1.0f, 1.0f, 0.0f);				// Top Right
//	glVertex3f( 1.0f,-1.0f, 0.0f);				// Bottom Right
//		glVertex3f(-1.0f,-1.0f, 0.0f);				// Bottom Left
//		glEnd();
glBegin(GL_TRIANGLE_FAN);
//glVertex3f(10, -12, 0);
//glVertex3f(10, 12, 0);
//glVertex3f(-10, 12, 0);
//glVertex3f(-10, -12, 0);
//glEnd();
	glTexCoord2f(0, 0);
	glVertex2f(0, 0);
	glTexCoord2f(512, 0);
	glVertex2f(screenWidth*2, 0);
	glTexCoord2f(0, 512);
	glVertex2f(0, screenHeight*2);
	glTexCoord2f(512, 512);
	glVertex2f(screenWidth*2, screenHeight*2);
	glEnd();
	glPopMatrix();
	glFlush();
//	glDeleteTextures(1,Tex);
	return;
}

/*		//leave alone -Q
void displayMapPreview()
{
	UDWORD i,j,height;
	UBYTE col,coltab[16];
	MAPTILE *psTile,*WTile;
	FEATURE *psFeat;

//	build col table;
	for (col=0; col<16; col+=1)
	{
		coltab[col] = pal_GetNearestColour( col*16,col*16, col*16);
	}

	//	for each tile, calc the height and draw a grayscale value 0-16
	psTile = psMapTiles;
	for (i=0; i<mapHeight; i+=2)
	{
		WTile = psTile;
		for (j=0; j<mapWidth; j+=2)	//Wtile is tile considering at i,j.
		{

			height = WTile->height;

			col = coltab[height/16];
//				pal_GetNearestColour( height, height, height);

			pie_BoxFillIndex(j,i,j+2,i+2,col);


			WTile+=2;
		}
		psTile += mapWidth*2;
	}
}
*/
#endif

// ////////////////////////////////////////////////////////////////////////////
// helper func

//sets sWRFILE form game.map
static void decideWRF(void)
{
	// try and load it from the maps directory first,
	strcpy(pLevelName, MultiCustomMapsPath);
	strcat(pLevelName, game.map);
	strcat(pLevelName, ".wrf");
	debug(LOG_WZ, "decideWRF: %s", pLevelName);
	//if the file exists in the downloaded maps dir then use that one instead.
	// FIXME: Try to incorporate this into physfs setup somehow for sane paths
	if ( !PHYSFS_exists(pLevelName) )
	{
		strcpy(pLevelName, game.map);		// doesn't exist, must be a predefined one.
	}
}


// ////////////////////////////////////////////////////////////////////////////
// Connection Options Screen.

static BOOL OptionsInet(UDWORD parentID)			//internet options
{
	W_EDBINIT		sEdInit;
	W_FORMINIT		sFormInit;
	W_LABINIT		sLabInit;

	if(ingame.bHostSetup)
	{
		SettingsUp = -1;
		return TRUE;
	}

	widgCreateScreen(&psConScreen);
	widgSetTipFont(psConScreen,WFont);

	memset(&sFormInit, 0, sizeof(W_FORMINIT));		//Connection Settings
	sFormInit.formID = 0;
	sFormInit.id = CON_SETTINGS;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.x = CON_SETTINGSX;
	sFormInit.y = CON_SETTINGSY;
	sFormInit.width = CON_SETTINGSWIDTH;
	sFormInit.height = CON_SETTINGSHEIGHT;
	sFormInit.pDisplay = intDisplayFeBox;
	widgAddForm(psConScreen, &sFormInit);

	addMultiBut(psConScreen, CON_SETTINGS,CON_OK,CON_OKX,CON_OKY,MULTIOP_OKW,MULTIOP_OKH,
				STR_MUL_OK,IMAGE_OK,IMAGE_OK,TRUE);

	//label.
	memset(&sLabInit, 0, sizeof(W_LABINIT));
	sLabInit.formID = CON_SETTINGS;
	sLabInit.id		= CON_SETTINGS_LABEL;
	sLabInit.style	= WLAB_ALIGNCENTRE;
	sLabInit.x		= 0;
	sLabInit.y		= 10;
	sLabInit.width	= CON_SETTINGSWIDTH;
	sLabInit.height = 20;
	sLabInit.pText	= strresGetString(psStringRes, STR_MUL_IPADDR);
	sLabInit.FontID = WFont;
	widgAddLabel(psConScreen, &sLabInit);


	memset(&sEdInit, 0, sizeof(W_EDBINIT));			// address
	sEdInit.formID = CON_SETTINGS;
	sEdInit.id = CON_IP;
	sEdInit.style = WEDB_PLAIN;
	sEdInit.x = CON_IPX;
	sEdInit.y = CON_IPY;
	sEdInit.width = CON_NAMEBOXWIDTH;
	sEdInit.height = CON_NAMEBOXHEIGHT;
	sEdInit.pText = "";									//strresGetString(psStringRes, STR_MUL_IPADDR);
	sEdInit.FontID = WFont;
//	sEdInit.pUserData = (void*)PACKDWORD_TRI(0,IMAGE_DES_EDITBOXLEFTH , IMAGE_DES_EDITBOXLEFT);
//	sEdInit.pBoxDisplay = intDisplayButtonHilight;
	sEdInit.pBoxDisplay = intDisplayEditBox;
	if (!widgAddEditBox(psConScreen, &sEdInit))
	{
		return FALSE;
	}
	SettingsUp = 1;
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////////
// Draw the connections screen.
BOOL startConnectionScreen(void)
{
	addBackdrop();										//background
	addTopForm();										// logo
	addBottomForm();

	SettingsUp		= 0;
	InitialProto	= 0;
	safeSearch		= FALSE;

	NETuseNetwork(TRUE);								// don't pretend!!

	addSideText(FRONTEND_SIDETEXT,  FRONTEND_SIDEX, FRONTEND_SIDEY,strresGetString(psStringRes,STR_MUL_SIDECONNECTION));

	addMultiBut(psWScreen,FRONTEND_BOTFORM,CON_CANCEL,10,10,MULTIOP_OKW,MULTIOP_OKH,
		STR_MUL_CANCEL,IMAGE_RETURN,IMAGE_RETURN_HI,TRUE);	// goback buttpn levels

	addConnections(0);

	return TRUE;
}

// add connections
static void addConnections(UDWORD begin)
{
	UDWORD	pos = 50;
	addTextButton(CON_TYPESID_START+begin,FRONTEND_POS1X,pos, strresGetString(psStringRes,STR_CON_INTERNET),FALSE,FALSE);
}

void runConnectionScreen(void )
{
	UDWORD id;
	static UDWORD chosenproto;
	static char addr[128];
	void * finalconnection;

	processFrontendSnap(TRUE);

	if(SettingsUp ==1)
	{
		id = widgRunScreen(psConScreen);				// Run the current set of widgets
	}
	else
	{
		id = widgRunScreen(psWScreen);					// Run the current set of widgets
	}


	if(id == CON_CANCEL)								//cancel
	{
		changeTitleMode(MULTI);
		bMultiPlayer = FALSE;
	}

	if(id == CON_TYPESID_MORE)
	{
		widgDelete(psWScreen,FRONTEND_BOTFORM);

		SettingsUp = 0;
		InitialProto +=5;

		addBottomForm();
		addMultiBut(psWScreen,FRONTEND_BOTFORM,CON_CANCEL,10,10,MULTIOP_OKW,MULTIOP_OKH,
		STR_MUL_CANCEL,IMAGE_RETURN,IMAGE_RETURN_HI,TRUE);	// goback buttpn levels

		addConnections(InitialProto);
	}


	if(  SettingsUp==0 &&  (id >= CON_TYPESID_START) && (id<=CON_TYPESID_END) )
	{
			chosenproto = 2;
			OptionsInet(id);
	}

	switch(id)												// settings buttons
	{
	case CON_IP:											// ip entered
		strcpy(addr,widgGetString(psConScreen, CON_IP));
		break;
	default:
		break;
	}

	if(id==CON_OK || SettingsUp == -1)
	{
		if(SettingsUp == 1)
		{
			widgReleaseScreen(psConScreen);
			SettingsUp =0;
		}

		switch(chosenproto)
		{
		case 2:
			game.bytesPerSec			= INETBYTESPERSEC;
			game.packetsPerSec			= INETPACKETS;
			NETsetupTCPIP(&finalconnection, addr);			//inet
			break;
		default:
			game.bytesPerSec			= DEFAULTBYTESPERSEC;// possibly a lobby, so default.
			game.packetsPerSec			= DEFAULTPACKETS;
// swap comments below to allow other providers.
			// RODZ finalconnection = NetPlay.protocols[id-CON_TYPESID_START].connection;
			break;
//			return;	//dont work on anything else!
		}

		if(ingame.bHostSetup)
		{
			changeTitleMode(MULTIOPTION);
		}
		else
		{
			changeTitleMode(GAMEFIND);
		}
	}

	StartCursorSnap(&InterfaceSnap);

	widgDisplayScreen(psWScreen);							// show the widgets currently running
	if(SettingsUp == 1)
	{
		widgDisplayScreen(psConScreen);						// show the widgets currently running
	}
}


// ////////////////////////////////////////////////////////////////////////////
// Game Chooser Screen.

static void addGames(void)
{
	UDWORD i,gcount=0;
	W_BUTINIT	sButInit;

	//count games to see if need two columns.
	for(i=0;i<MaxGames;i++)							// draw games
	{
		if( NetPlay.games[i].desc.dwSize !=0)
		{
			gcount++;
		}
	}

	memset(&sButInit, 0, sizeof(W_BUTINIT));
	sButInit.formID = FRONTEND_BOTFORM;
	sButInit.style = WBUT_PLAIN;
	sButInit.width = GAMES_GAMEWIDTH;
	sButInit.height = GAMES_GAMEHEIGHT;
	sButInit.FontID = WFont;
	sButInit.pDisplay = displayRemoteGame;

	for(i=0;i<MaxGames;i++)							// draw games
	{
		widgDelete(psWScreen, GAMES_GAMESTART+i);	// remove old icon.
		if( NetPlay.games[i].desc.dwSize !=0)
		{

			sButInit.id = GAMES_GAMESTART+i;

			if(gcount < 6)							// only center column needed.
			{
				sButInit.x = 125;
				sButInit.y = (UWORD)(30+((5+GAMES_GAMEHEIGHT)*i) );
			}
			else
			{
				if(i<6)		//column 1
				{
					sButInit.x = 10;
					sButInit.y = (UWORD)(30+((5+GAMES_GAMEHEIGHT)*i) );
				}
				else		//column 2
				{
					sButInit.x = 20+GAMES_GAMEWIDTH;
					sButInit.y = (UWORD)(30+((5+GAMES_GAMEHEIGHT)*(i-6) ) );
				}
			}

			sButInit.pTip = NetPlay.games[i].name;

			sButInit.pUserData = (void *)i;
			widgAddButton(psWScreen, &sButInit);
		}
	}

}

void runGameFind(void )
{
	UDWORD id;
	static UDWORD lastupdate=0;

	if(lastupdate> gameTime)lastupdate = 0;
	if(gameTime-lastupdate >6000)
	{
		lastupdate = gameTime;
		if(safeSearch)
		{
			NETfindGame(TRUE);						// find games asynchronously
		}
		addGames();									//redraw list
	}

	processFrontendSnap(FALSE);

	id = widgRunScreen(psWScreen);						// Run the current set of widgets

	if(id == CON_CANCEL)								// ok
	{
		changeTitleMode(PROTOCOL);
	}

	if(id == MULTIOP_REFRESH)
	{
		NETfindGame(TRUE);								// find games asynchronously
		addGames();										//redraw list.
	}


	if(id >= GAMES_GAMESTART && id<=GAMES_GAMEEND)
	{
		gameNumber = id-GAMES_GAMESTART;

		if( ( NetPlay.games[gameNumber].desc.dwCurrentPlayers < NetPlay.games[gameNumber].desc.dwMaxPlayers)
			&& !(NetPlay.games[gameNumber].desc.dwFlags & SESSION_JOINDISABLED) ) // if still joinable
		{

			// if skirmish, check it wont take the last slot
			if( NETgetGameFlagsUnjoined(gameNumber,1) == SKIRMISH
			  && (NetPlay.games[gameNumber].desc.dwCurrentPlayers >= NetPlay.games[gameNumber].desc.dwMaxPlayers -1)
			  )
			{
				goto FAIL;
			}


			ingame.localOptionsReceived = FALSE;			// note we are awaiting options
			strcpy(game.name, NetPlay.games[gameNumber].name);		// store name

//			strcpy(sPlayer,"LastUsed");
//			loadMultiStats(sPlayer,&nullStats);
//			if(NETgetGameFlagsUnjoined(gameNumber,1) == DMATCH)
//			{
//				joinArena(gameNumber,(char*)sPlayer);
//			}
//			else
//			{
				joinCampaign(gameNumber,(char*)sPlayer);
//			}

			changeTitleMode(MULTIOPTION);
		}

	}

FAIL:

	StartCursorSnap(&InterfaceSnap);

	widgDisplayScreen(psWScreen);								// show the widgets currently running
	if(safeSearch)
	{
		iV_SetFont(FEFont);
		pie_DrawText(strresGetString(psStringRes, STR_MUL_SEARCHING), D_W+260, D_H+460);
	}
}


void startGameFind(void)
{
	addBackdrop();										//background
	addTopForm();										// logo
	addBottomForm();

	addSideText(FRONTEND_SIDETEXT,  FRONTEND_SIDEX, FRONTEND_SIDEY,strresGetString(psStringRes,STR_GAMES_GAMES));

	// cancel
	addMultiBut(psWScreen,FRONTEND_BOTFORM,CON_CANCEL,10,5,MULTIOP_OKW,MULTIOP_OKH,STR_MUL_CANCEL,
		IMAGE_RETURN,IMAGE_RETURN_HI,TRUE);

	if(!safeSearch)
	{
		//refresh
		addMultiBut(psWScreen,FRONTEND_BOTFORM,MULTIOP_REFRESH ,480-MULTIOP_OKW-5 ,5,MULTIOP_OKW,MULTIOP_OKH,
					STR_MUL_REFRESH,IMAGE_REFRESH,IMAGE_REFRESH,FALSE);			// Find Games button
	}

	NETfindGame(TRUE);
	addGames();	// now add games.
}


// ////////////////////////////////////////////////////////////////////////////
// Game Options Screen.

// ////////////////////////////////////////////////////////////////////////////

static void addBlueForm(UDWORD parent,UDWORD id,char *txt,UDWORD x,UDWORD y,UDWORD w,UDWORD h)
{
	W_FORMINIT	sFormInit;
	W_LABINIT	sLabInit;

	memset(&sFormInit, 0, sizeof(W_FORMINIT));				// draw options box.
	sFormInit.formID= parent;
	sFormInit.id	= id;
	sFormInit.x		=(UWORD) x;
	sFormInit.y		=(UWORD) y;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.width = (UWORD)w;//190;
	sFormInit.height= (UWORD)h;//27;
	sFormInit.pDisplay =  intDisplayFeBox;
	widgAddForm(psWScreen, &sFormInit);

	if(strlen(txt)>0)
	{
		memset(&sLabInit, 0, sizeof(W_LABINIT));
		sLabInit.formID = id;
		sLabInit.id		= id+1;
		sLabInit.style	= WLAB_PLAIN;
		sLabInit.x		= 3;
		sLabInit.y		= 4;
		sLabInit.width	= 80;
		sLabInit.height = 20;
		sLabInit.pText	= txt;
//		sLabInit.pDisplay = displayFeText;
		sLabInit.FontID = WFont;
		widgAddLabel(psWScreen, &sLabInit);
	}
	return;
}



static void addGameOptions(BOOL bRedo)
{
	W_FORMINIT		sFormInit;

	if(bRedo)
	{
		// stop any editing going on
		screenClearFocus(psWScreen);
	}

	widgDelete(psWScreen,MULTIOP_OPTIONS);  				// clear options list
	widgDelete(psWScreen,FRONTEND_SIDETEXT3);				// del text..

	iV_SetFont(WFont);

	memset(&sFormInit, 0, sizeof(W_FORMINIT));				// draw options box.
	sFormInit.formID = FRONTEND_BACKDROP;
	sFormInit.id = MULTIOP_OPTIONS;
	sFormInit.x = MULTIOP_OPTIONSX;
	sFormInit.y = MULTIOP_OPTIONSY;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.width = MULTIOP_OPTIONSW;
	sFormInit.height = MULTIOP_OPTIONSH;
	sFormInit.pDisplay = intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT3, MULTIOP_OPTIONSX-3 , MULTIOP_OPTIONSY,strresGetString(psStringRes,STR_MUL_SIDEOPTIONS));

	addMultiEditBox(MULTIOP_OPTIONS,MULTIOP_GNAME,MCOL0,MROW2,STR_MUL_GAMEIC, game.name ,IMAGE_EDIT_GAME,MULTIOP_GNAME_ICON);
	addMultiEditBox(MULTIOP_OPTIONS,MULTIOP_MAP  ,MCOL0,MROW3,STR_MUL_MAPIC, game.map ,IMAGE_EDIT_MAP,MULTIOP_MAP_ICON);


	// buttons.

	// game type
	addBlueForm(MULTIOP_OPTIONS,MULTIOP_GAMETYPE,strresGetString(psStringRes,STR_LABEL_TYPE),MCOL0,MROW5,MULTIOP_BLUEFORMW,27);
//	addMultiBut(psWScreen,MULTIOP_GAMETYPE,MULTIOP_ARENA,	MCOL1, 2 , MULTIOP_BUTW,MULTIOP_BUTH,
//				STR_MUL_ARENA,   IMAGE_ARENA,   IMAGE_ARENA_HI,TRUE);		//arena
	addMultiBut(psWScreen,MULTIOP_GAMETYPE,MULTIOP_CAMPAIGN,MCOL1, 2 , MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_CAMPAIGN,IMAGE_CAMPAIGN,IMAGE_CAMPAIGN_HI,TRUE);	//camp

	addMultiBut(psWScreen,MULTIOP_GAMETYPE,MULTIOP_TEAMPLAY,MCOL2, 2 , MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_TEAMPLAY,IMAGE_TEAM,IMAGE_TEAM_HI,TRUE);			//teamplay

	addMultiBut(psWScreen,MULTIOP_GAMETYPE,MULTIOP_SKIRMISH,MCOL3, 2 , MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_SKIRMISH,IMAGE_SKIRMISH,IMAGE_SKIRMISH_HI,TRUE);	//skirmish

//	widgSetButtonState(psWScreen, MULTIOP_ARENA,	0);
	widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN,	0);
	widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY,	0);
	widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,	0);

	switch(game.type)
	{
//	case DMATCH:
//		widgSetButtonState(psWScreen,MULTIOP_ARENA,WBUT_LOCK);
//		break;
	case CAMPAIGN:
		widgSetButtonState(psWScreen,MULTIOP_CAMPAIGN,WBUT_LOCK);
		break;
	case TEAMPLAY:
		widgSetButtonState(psWScreen,MULTIOP_TEAMPLAY,WBUT_LOCK);
		break;
	case SKIRMISH:
		widgSetButtonState(psWScreen,MULTIOP_SKIRMISH,WBUT_LOCK);
		break;
	}

	if(!NetPlay.bComms)
	{
		widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN, WBUT_DISABLE);
//		widgSetButtonState(psWScreen, MULTIOP_ARENA,	WBUT_DISABLE);
		widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY, WBUT_DISABLE);
	}

	//just display the game options.
	addMultiEditBox(MULTIOP_OPTIONS,MULTIOP_PNAME,MCOL0,MROW1, STR_MUL_PLAYERIC,(char*) sPlayer,IMAGE_EDIT_PLAYER,MULTIOP_PNAME_ICON);
	addMultiEditBox(MULTIOP_OPTIONS,MULTIOP_FNAME,MCOL0,MROW4, STR_MUL_FORCEIC, sForceName,IMAGE_EDIT_FORCE,MULTIOP_FNAME_ICON);


	// Fog type
	addBlueForm(MULTIOP_OPTIONS,MULTIOP_FOG,strresGetString(psStringRes,STR_LABEL_FOG),MCOL0,MROW6,MULTIOP_BLUEFORMW,27);

	addMultiBut(psWScreen,MULTIOP_FOG,MULTIOP_FOG_ON ,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,STR_MUL_FOG_ON, IMAGE_FOG_OFF, IMAGE_FOG_OFF_HI,TRUE);//black stuff
	addMultiBut(psWScreen,MULTIOP_FOG,MULTIOP_FOG_OFF,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,STR_MUL_FOG_OFF,IMAGE_FOG_ON,IMAGE_FOG_ON_HI,TRUE);
	if(game.fog)
	{
		widgSetButtonState(psWScreen, MULTIOP_FOG_ON,WBUT_LOCK);
	}
	else
	{
		widgSetButtonState(psWScreen, MULTIOP_FOG_OFF,WBUT_LOCK);
	}


	if(game.type != TEAMPLAY)
	{
		// alliances
//		if(game.type == DMATCH)
//		{
//			addBlueForm(MULTIOP_OPTIONS,MULTIOP_ALLIANCES,strresGetString(psStringRes,STR_LABEL_ALLI),MCOL0,MROW7,MULTIOP_BLUEFORMW,27);
//		}
//		else
//		{
			addBlueForm(MULTIOP_OPTIONS,MULTIOP_ALLIANCES,strresGetString(psStringRes,STR_LABEL_ALLI),MCOL0,MROW8,MULTIOP_BLUEFORMW,27);
//		}

		addMultiBut(psWScreen,MULTIOP_ALLIANCES,MULTIOP_ALLIANCE_N,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_ALLIANCEN,IMAGE_NOALLI,IMAGE_NOALLI_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_ALLIANCES,MULTIOP_ALLIANCE_Y,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_ALLIANCEY,IMAGE_ALLI,IMAGE_ALLI_HI,TRUE);

		//add 'Locked Teams' button
		addMultiBut(psWScreen,MULTIOP_ALLIANCES,MULTIOP_ALLIANCE_TEAMS,MCOL3,2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_ALLIANCE_TEAMS,IMAGE_ALLI_TEAMS,IMAGE_ALLI_TEAMS_HI,TRUE);		//FIXME: fix hover image

		widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_N,0);				//hilight correct entry
		widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_Y,0);
		widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,0);

		//can't have ALLIANCES_TEAMS in campaign mode
		if(game.type == CAMPAIGN)
		{
			if(game.alliance == ALLIANCES_TEAMS)
				game.alliance = NO_ALLIANCES;

			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS, WBUT_DISABLE);		//disable
		}

		switch(game.alliance)
		{
		case NO_ALLIANCES:
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_N,WBUT_LOCK);
			break;
		case ALLIANCES:
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_Y,WBUT_LOCK);
			break;
		case ALLIANCES_TEAMS:
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,WBUT_LOCK);
			break;
		}
	}

/*	if(game.type == DMATCH)
	{

		// limit options
		addBlueForm(MULTIOP_OPTIONS,MULTIOP_LIMIT,strresGetString(psStringRes,STR_LABEL_LIMIT) ,MCOL0,MROW8,MULTIOP_BLUEFORMW,27);
		addMultiBut(psWScreen,MULTIOP_LIMIT,MULTIOP_NOLIMIT,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_NOLIM,IMAGE_NOLIMIT,IMAGE_NOLIMIT_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_LIMIT,MULTIOP_FRAGLIMIT,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_FRAGLIM,IMAGE_FRAGLIMIT,IMAGE_FRAGLIMIT_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_LIMIT,MULTIOP_TIMELIMIT, MCOL3, 2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_TIMELIM,IMAGE_TIMELIMIT,IMAGE_TIMELIMIT_HI,TRUE);
		widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,0);		//hilight correct entry
		widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,0);
		widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT ,0);
		switch(game.limit)
		{
		case NOLIMIT:
			widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,WBUT_LOCK);
			break;
		case FRAGLIMIT:
			widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,WBUT_LOCK);
			break;
		case TIMELIMIT:
			widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT,WBUT_LOCK);
			break;
		}
	}
*/
	if  (game.type == SKIRMISH || game.base != CAMP_WALLS )
//	  ||(game.type != DMATCH && game.base != CAMP_WALLS))
	{
		widgSetButtonState(psWScreen, MULTIOP_FNAME,WEDBS_DISABLE);		// disable force buts
		widgSetButtonState(psWScreen, MULTIOP_FNAME_ICON,WBUT_DISABLE);
	}

	if(game.type == CAMPAIGN || game.type == SKIRMISH || game.type == TEAMPLAY)
	{

		// pow levels
		addBlueForm(MULTIOP_OPTIONS,MULTIOP_POWER,strresGetString(psStringRes,STR_INT_POWER),MCOL0,MROW9,MULTIOP_BLUEFORMW,27);
		addMultiBut(psWScreen,MULTIOP_POWER,MULTIOP_POWLEV_LOW,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_POWLO,IMAGE_POWLO,IMAGE_POWLO_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_POWER,MULTIOP_POWLEV_MED,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_POWMED,IMAGE_POWMED,IMAGE_POWMED_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_POWER,MULTIOP_POWLEV_HI, MCOL3, 2,MULTIOP_BUTW,MULTIOP_BUTH,
			STR_MUL_POWHI,IMAGE_POWHI,IMAGE_POWHI_HI,TRUE);
		widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,0);		//hilight correct entry
		widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,0);
		widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI ,0);
		switch(game.power)
		{
		case LEV_LOW:
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,WBUT_LOCK);
			break;
		case LEV_MED:
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,WBUT_LOCK);
			break;
		case LEV_HI:
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI,WBUT_LOCK);
			break;
		}

		//type clean/base/defence
		addBlueForm(MULTIOP_OPTIONS,MULTIOP_BASETYPE,strresGetString(psStringRes,STR_LABEL_BASE),MCOL0,MROW7,MULTIOP_BLUEFORMW,27);
		addMultiBut(psWScreen,MULTIOP_BASETYPE,MULTIOP_CLEAN,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_CAMPCLEAN,IMAGE_NOBASE,IMAGE_NOBASE_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_BASETYPE,MULTIOP_BASE,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_CAMPBASE,IMAGE_SBASE,IMAGE_SBASE_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_BASETYPE,MULTIOP_DEFENCE,MCOL3,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_CAMPDEFENCE,IMAGE_LBASE,IMAGE_LBASE_HI,TRUE);
		widgSetButtonState(psWScreen, MULTIOP_CLEAN,0);						//hilight correct entry
		widgSetButtonState(psWScreen, MULTIOP_BASE,0);
		widgSetButtonState(psWScreen, MULTIOP_DEFENCE,0);
		switch(game.base)
		{
		case 0:
			widgSetButtonState(psWScreen, MULTIOP_CLEAN,WBUT_LOCK);
			break;
		case 1:
			widgSetButtonState(psWScreen, MULTIOP_BASE,WBUT_LOCK);
			break;
		case 2:
			widgSetButtonState(psWScreen, MULTIOP_DEFENCE,WBUT_LOCK);
			break;
		}
	}

	if(game.type == CAMPAIGN  || game.type == TEAMPLAY)
	{

		//type opposition/no computer opposition
		if(game.type == CAMPAIGN)
		{
			addBlueForm(MULTIOP_OPTIONS,MULTIOP_COMPUTER,strresGetString(psStringRes,STR_MUL_COMP),MCOL0,MROW10,MULTIOP_BLUEFORMW,27);
		}
		else
		{
			addBlueForm(MULTIOP_OPTIONS,MULTIOP_COMPUTER,strresGetString(psStringRes,STR_MUL_COMP),MCOL0,MROW8,MULTIOP_BLUEFORMW,27);
		}

		addMultiBut(psWScreen,MULTIOP_COMPUTER,MULTIOP_COMPUTER_Y,MCOL1,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_COMP_Y,IMAGE_COMPUTER_Y,IMAGE_COMPUTER_Y_HI,TRUE);
		addMultiBut(psWScreen,MULTIOP_COMPUTER,MULTIOP_COMPUTER_N,MCOL2,2,MULTIOP_BUTW,MULTIOP_BUTH,
				STR_MUL_COMP_N,IMAGE_COMPUTER_N,IMAGE_COMPUTER_N_HI,TRUE);
		widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,0);						//hilight correct entry
		widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,0);
		switch(game.bComputerPlayers)
		{
		case FALSE:
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,WBUT_LOCK);
			break;
		case TRUE:
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,WBUT_LOCK);
			break;
		}

		//remove PC players for 7/8 player maps.
		if(game.maxPlayers >6)
		{
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,WBUT_DISABLE);
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,WBUT_DISABLE);
		}

	}

	// cancel
	addMultiBut(psWScreen,MULTIOP_OPTIONS,CON_CANCEL,
		MULTIOP_CANCELX,MULTIOP_CANCELY,
		iV_GetImageWidth(FrontImages,IMAGE_RETURN),
		iV_GetImageHeight(FrontImages,IMAGE_RETURN),
		STR_MUL_CANCEL,IMAGE_RETURN,IMAGE_RETURN_HI,TRUE);


	// host Games button
	if(ingame.bHostSetup && !bHosted)
	{
		addMultiBut(psWScreen,MULTIOP_OPTIONS,MULTIOP_HOST,MULTIOP_HOSTX,MULTIOP_HOSTY,35,28,
					STR_MUL_HOST,IMAGE_HOST,IMAGE_HOST,FALSE);
	}

	// hosted or hosting.
	// limits button.
	if(ingame.bHostSetup )//&& (game.type != DMATCH))
	{
		addMultiBut(psWScreen,MULTIOP_OPTIONS,MULTIOP_STRUCTLIMITS,MULTIOP_STRUCTLIMITSX,MULTIOP_STRUCTLIMITSY,
					35,28, STR_MUL_STRLIM,IMAGE_SLIM,IMAGE_SLIM_HI,FALSE);
	}


	// disable buttons not available in lobby games
	if(NetPlay.bLobbyLaunched)
	{
		widgSetButtonState(psWScreen, MULTIOP_GNAME,WEDBS_DISABLE);
		widgSetButtonState(psWScreen, MULTIOP_GNAME_ICON,WBUT_DISABLE);

		widgSetButtonState(psWScreen, MULTIOP_PNAME,WEDBS_DISABLE);
		widgSetButtonState(psWScreen, MULTIOP_PNAME_ICON,WBUT_DISABLE);

	}

	//disable demo options
#ifdef MULTIDEMO
//	widgSetButtonState(psWScreen, MULTIOP_MAP,WEDBS_DISABLE);
//	widgSetButtonState(psWScreen, MULTIOP_MAP_ICON,WBUT_DISABLE);

	widgSetButtonState(psWScreen, MULTIOP_DEFENCE,WBUT_DISABLE);
//	widgSetButtonState(psWScreen, MULTIOP_ARENA,WBUT_DISABLE);
	widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,WBUT_DISABLE);
#endif

		return;
}

// ////////////////////////////////////////////////////////////////////////////
// Colour functions

static BOOL safeToUseColour(UDWORD player,UDWORD col)
{
	UDWORD i;

	// if already using it.
	if( col == getPlayerColour(player) )
	{
		return TRUE;						// already using it.
	}

	// player wants to be colour. check no other player to see if it is using that colour.....
	for(i=0;i<MAX_PLAYERS;i++)
	{
		// if no human (except us) is using it
		if( (i!=player) && isHumanPlayer(i) && (getPlayerColour(i) == col) )
		{
			return FALSE;
		}
	}

	// no computer player is using it
/*	if(game.type == SKIRMISH)
	{
		if( (col<game.maxPlayers) )
		{
			for(i=0;i<game.maxPlayers;i++)
			{
				// check no player is using it...
				if( (i!=player) && !isHumanPlayer(i) && (getPlayerColour(i) == col) )
				{
					return FALSE;
				}
			}
		}
	}
	*/
	return TRUE;
}

BOOL chooseColour(UDWORD player)
{
	UDWORD col;

	for(col =0;col<MAX_PLAYERS;col++)
	{
		if(safeToUseColour(player,col))
		{
			setPlayerColour(player,col);
			return TRUE;
		}
	}
	return TRUE;
}

/*
 * Checks if a team chooser dialog is up
 * returns index of the player who was choosing a team or -1 if none open
 */
static SDWORD teamChooserUp(void)
{
	UDWORD i;
	for(i=0;i<MAX_PLAYERS;i++)
	{
		if(bTeamChooserUp[i])
			return i;
	}

	return -1;		//none
}
static void addColourChooser(UDWORD player)
{
	UDWORD i;//,j;

	// delete that players box,
	widgDelete(psWScreen,MULTIOP_PLAYER_START+player);

	// detele team chooser botton
	widgDelete(psWScreen,MULTIOP_TEAMS_START+player);

	// add form.
	addBlueForm(MULTIOP_PLAYERS,MULTIOP_COLCHOOSER_FORM,"",
				10,
				((MULTIOP_PLAYERHEIGHT+5)*player)+4,
				MULTIOP_ROW_WIDTH,MULTIOP_PLAYERHEIGHT);

	// add the flags
	for(i=0;i<MAX_PLAYERS;i++)		//game.maxPlayers;i++)
	{
		addMultiBut(psWScreen,MULTIOP_COLCHOOSER_FORM, MULTIOP_COLCHOOSER+i,
			(i*(iV_GetImageWidth(FrontImages,IMAGE_PLAYER0) +5)+7) ,//x
			4,/*9,*/													  //y
			iV_GetImageWidth(FrontImages,IMAGE_PLAYER0),		  //w
			iV_GetImageHeight(FrontImages,IMAGE_PLAYER0),		  //h
			0, IMAGE_PLAYER0+i, IMAGE_PLAYER0+i,FALSE);

			if( !safeToUseColour(selectedPlayer,i))
			{
				widgSetButtonState(psWScreen,MULTIOP_COLCHOOSER+i ,WBUT_DISABLE);
			}
	}

	//add the position chooser.
	for(i=0;i<game.maxPlayers;i++)
	{

		addMultiBut(psWScreen,MULTIOP_COLCHOOSER_FORM, MULTIOP_PLAYCHOOSER+i,
			(i*(iV_GetImageWidth(FrontImages,IMAGE_PLAYER0) +5)+7),//x
			23,													  //y
			iV_GetImageWidth(FrontImages,IMAGE_WEE_GUY)+7,		  //w
			iV_GetImageHeight(FrontImages,IMAGE_WEE_GUY),		  //h
			0,IMAGE_WEE_GUY, IMAGE_WEE_GUY,10+i);

			if(isHumanPlayer(i) && i!=selectedPlayer)
			{
				widgSetButtonState(psWScreen,MULTIOP_PLAYCHOOSER+i ,WBUT_DISABLE);
			}
	}



	bColourChooserUp = TRUE;
}

static void closeColourChooser(void)
{
	bColourChooserUp = FALSE;

	widgDelete(psWScreen,MULTIOP_COLCHOOSER_FORM);
}

static BOOL SendTeamRequest(UDWORD player, UBYTE chosenTeam)
{
	NETMSG m;

	m.body[0] =	(UBYTE)player;
	m.body[1] = (UBYTE)chosenTeam;
	m.type    = NET_TEAMREQUEST;
	m.size    = 2;

	if(NetPlay.bHost)			// do or request the change.
	{
		recvTeamRequest(&m);	// do the change, remember only the host can do this to avoid confusion.
		return TRUE;
	}
	else
	{
		return NETbcast(&m,TRUE);
	}
}

BOOL recvTeamRequest(NETMSG *pMsg)
{
	UDWORD	player,team;

	if(!NetPlay.bHost)							//only host should act.
	{
		return TRUE;
	}

	player			= (UDWORD) pMsg->body[0];
	team			= (UDWORD) pMsg->body[1];

	playerTeamGUI[player] = team;

	debug(LOG_WZ, "set %d as new team for player %d", team, player);

	sendOptions(player2dpid[player],player);	// tell everyone && update requesting player.

	return TRUE;
}

static BOOL SendColourRequest(UDWORD player, UBYTE col,UBYTE chosenPlayer)
{
	NETMSG m;

	m.body[0] =	(UBYTE)player;
	m.body[1] = (UBYTE)col;
	m.body[2] = (UBYTE)chosenPlayer;
	m.type    = NET_COLOURREQUEST;
	m.size    = 3;

	if(NetPlay.bHost)			// do or request the change.
	{
		recvColourRequest(&m);	// do the change, remember only the host can do this to avoid confusion.
		return TRUE;
	}
	else
	{
		return NETbcast(&m,TRUE);
	}
}


BOOL recvColourRequest(NETMSG *pMsg)
{
	UDWORD	player,col,oldcol;
	UBYTE	chosenPlayer;
	UDWORD	dpid;

	if(!NetPlay.bHost)							//only host should act.
	{
		return TRUE;
	}

	player			= (UDWORD) pMsg->body[0];
	col				= (UDWORD) pMsg->body[1];
	chosenPlayer	= (UDWORD) pMsg->body[2];

	if(chosenPlayer == UBYTE_MAX)
	{	// colour change.
		if(!safeToUseColour(player,col))
		{
			return FALSE;
		}

		setPlayerColour(player,col);
		sendOptions(player2dpid[player],player);	// tell everyone && update requesting player.
	}
	else // base change.
	{
		if(isHumanPlayer(chosenPlayer))
		{
			return FALSE;
		}

		dpid = player2dpid[player];
		player2dpid[player] = 0;					// remove player,
		ingame.JoiningInProgress[player] = FALSE;	// if they never joined, reset the flag

		oldcol = getPlayerColour(chosenPlayer);
		setPlayerColour	(chosenPlayer,getPlayerColour(player));// retain player colour
		setPlayerColour	(player,oldcol);			// reset old colour
//		chooseColour	(chosenPlayer);				// pick an unused colour.
		setupNewPlayer	(dpid,chosenPlayer);		// setup all the guff for that player.
		sendOptions		(dpid,chosenPlayer);

		NETplayerInfo();						// bring netplay up to date with changes.

		if(player == selectedPlayer)// if host changing
		{
			selectedPlayer = chosenPlayer;
		}
	}
	return TRUE;
}


/*
 * Opens a menu for a player to choose a team
 * 'player' is a player id of the player who will get a new team assigned
 */
static void addTeamChooser(UDWORD player)
{
	UDWORD i;

	debug(LOG_WZ, "opened team chooser for %d, current team: %d", player, playerTeamGUI[player]);

	// delete team chooser botton
	widgDelete(psWScreen,MULTIOP_TEAMS_START+player);

	// delete that players box
	widgDelete(psWScreen,MULTIOP_PLAYER_START+player);

	// add form.
	addBlueForm(MULTIOP_PLAYERS,MULTIOP_TEAMCHOOSER_FORM,"",
				10,
				((MULTIOP_TEAMSHEIGHT+5)*player)+4,
				MULTIOP_ROW_WIDTH,MULTIOP_TEAMSHEIGHT);

	// add the teams
	for(i=0;i<MAX_PLAYERS;i++)		//game.maxPlayers;i++)
	{
		W_BUTINIT		sButInit;

		memset(&sButInit, 0, sizeof(W_BUTINIT));
		sButInit.formID = MULTIOP_TEAMCHOOSER_FORM;
		sButInit.id = MULTIOP_TEAMCHOOSER+i;
		sButInit.style = WFORM_PLAIN;
		sButInit.x = (short) (i*(iV_GetImageWidth(FrontImages,IMAGE_TEAM0) + 3)+3);
		sButInit.y = (short) 6;
		sButInit.width = (unsigned short) iV_GetImageWidth(FrontImages,IMAGE_TEAM0);
		sButInit.height= (unsigned short) iV_GetImageHeight(FrontImages,IMAGE_TEAM0);

		sButInit.pTip = "Team";

		sButInit.FontID = WFont;
		sButInit.pDisplay = displayMultiBut;
		sButInit.pUserData = (void*)PACKDWORD_TRI(FALSE,IMAGE_TEAM0+i , IMAGE_TEAM0+i);
		sButInit.pText = "Team0";

		if (!widgAddButton(psWScreen, &sButInit))
		{
			ASSERT(FALSE,"addTeamChooser: widgAddButton() failed");
		}
	}

	bTeamChooserUp[player] = TRUE;
}

/*
 * Closes Team Chooser dialog box, if there was any open
 */
static void closeTeamChooser(void)
{
	UDWORD i;
	for(i=0; i<MAX_PLAYERS;i++)
	{
		if(bTeamChooserUp[i])
		{
			debug(LOG_WZ, "closed team chooser for %d, current team: %d", i, playerTeamGUI[i]);

			bTeamChooserUp[i] = FALSE;

			ASSERT(teamChooserUp() < 0, "closeTeamChooser: more than one team chooser open (%d)", teamChooserUp());
		}
	}

	widgDelete(psWScreen,MULTIOP_TEAMCHOOSER_FORM);	//only once!
}

// ////////////////////////////////////////////////////////////////////////////
// box for players.

UDWORD addPlayerBox(BOOL players)
{
	W_FORMINIT		sFormInit;
	W_BUTINIT		sButInit;
	UDWORD			i=0;

	// if background isn't there, then return since were not ready to draw the box yet!
	if(widgGetFromID(psWScreen,FRONTEND_BACKDROP) == NULL)
	{
		return 0;
	}

	if(bHosted || ingame.localJoiningInProgress)
	{
		NETplayerInfo();
	}
	else
	{
		NETplayerInfo();			// get player info.
	}

	widgDelete(psWScreen,MULTIOP_PLAYERS);		// del player window
	widgDelete(psWScreen,FRONTEND_SIDETEXT2);	// del text too,

	memset(&sFormInit, 0, sizeof(W_FORMINIT));	// draw player window
	sFormInit.formID = FRONTEND_BACKDROP;
	sFormInit.id = MULTIOP_PLAYERS;
	sFormInit.x = MULTIOP_PLAYERSX;
	sFormInit.y = MULTIOP_PLAYERSY;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.width = MULTIOP_PLAYERSW;
	sFormInit.height = MULTIOP_PLAYERSH;
	sFormInit.pDisplay = intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT2, MULTIOP_PLAYERSX-3, MULTIOP_PLAYERSY,strresGetString(psStringRes,STR_MUL_SIDEPLAYERS));

	if(players)
	{
		for(i=0;i<game.maxPlayers;i++)
		{
			if(ingame.localOptionsReceived && game.type == SKIRMISH)	// skirmish only
			{
				//add team chooser
				memset(&sButInit, 0, sizeof(W_BUTINIT));
				sButInit.formID = MULTIOP_PLAYERS;
				sButInit.id = MULTIOP_TEAMS_START+i;
				sButInit.style = WBUT_PLAIN;
				sButInit.x = 10;
				sButInit.y = (UWORD)(( (MULTIOP_TEAMSHEIGHT+5)*i)+4);
				sButInit.width = MULTIOP_TEAMSWIDTH;
				sButInit.height = MULTIOP_TEAMSHEIGHT;
				sButInit.pTip = "Choose team";	//Players[i].name;
				sButInit.FontID = WFont;
				sButInit.pDisplay = displayTeamChooser;//intDisplayButtonHilight;
				sButInit.pUserData = (void*) i;

				if(bTeamChooserUp[i] && !bColourChooserUp )
				{
					addTeamChooser(i);
				}
				else if(game.skDiff[i])	//only if not disabled
				{
					widgAddButton(psWScreen, &sButInit);
				}
			}

			if(ingame.localOptionsReceived && NetPlay.players[i].dpid)					// only draw if real player!
			{
				memset(&sButInit, 0, sizeof(W_BUTINIT));
				sButInit.formID = MULTIOP_PLAYERS;
				sButInit.id = MULTIOP_PLAYER_START+i;
				sButInit.style = WBUT_PLAIN;
				sButInit.x = 10 + MULTIOP_TEAMSWIDTH;
				sButInit.y = (UWORD)(( (MULTIOP_PLAYERHEIGHT+5)*i)+4);
				sButInit.width = MULTIOP_PLAYERWIDTH - MULTIOP_TEAMSWIDTH;
				sButInit.height = MULTIOP_PLAYERHEIGHT;
				sButInit.pTip = NULL;//Players[i].name;
				sButInit.FontID = WFont;
				sButInit.pDisplay = displayPlayer;//intDisplayButtonHilight;
				sButInit.pUserData = (void*) i;

				if(bColourChooserUp && (teamChooserUp() < 0)
					&& NetPlay.players[i].dpid == player2dpid[selectedPlayer])
				{
					addColourChooser(i);
				}
				else if(i != teamChooserUp())	//display player number/color only if not selecting team for this player
				{
					widgAddButton(psWScreen, &sButInit);
				}

			}
			else if(game.type == SKIRMISH)	// skirmish player
			{
				memset(&sFormInit, 0, sizeof(W_BUTINIT));
				sFormInit.formID = MULTIOP_PLAYERS;
				sFormInit.id = MULTIOP_PLAYER_START+i;
				sFormInit.style = WBUT_PLAIN;
				sFormInit.x = 10;
				sFormInit.y = (UWORD)(( (MULTIOP_PLAYERHEIGHT+5)*i)+4);
				sFormInit.width = MULTIOP_ROW_WIDTH;
				sFormInit.height = MULTIOP_PLAYERHEIGHT;
				sFormInit.pTip = NULL;//Players[i].name;
				sFormInit.pDisplay = displayPlayer;//intDisplayButtonHilight;
				sFormInit.pUserData = (void*) i;
				widgAddForm(psWScreen, &sFormInit);
				addFESlider(MULTIOP_SKSLIDE+i,sFormInit.id, 43,9,	  20,game.skDiff[i], 0);
			}
		}
	}

	if(ingame.bHostSetup) // if hosting.
	{
		sliderEnableDrag(TRUE);
	}else{
		sliderEnableDrag(FALSE);
	}

	return i;
}

/*
 * Notify all players of host launching the game
 */
static void SendFireUp(void)
{
	NETMSG m;

	m.type  = NET_FIREUP;
	m.size  = 1;
	NETbcast(&m,TRUE);
}

// host kick a player from a game.
void kickPlayer(UDWORD dpid)
{
	NETMSG m;
	// send a kick msg
	m.type  = NET_KICK;
	m.size  = 4;
	NetAdd(m,0,dpid);
	NETbcast(&m,TRUE);
	return;
}


static void addOkBut(void)
{
	addMultiBut(psWScreen, MULTIOP_OPTIONS,CON_OK,
		MULTIOP_OKX,MULTIOP_OKY,
		iV_GetImageWidth(FrontImages,IMAGE_BIGOK),
		iV_GetImageHeight(FrontImages,IMAGE_BIGOK),
		STR_MUL_OK,IMAGE_BIGOK,IMAGE_BIGOK,FALSE);
}

static void addChatBox(void)
{
	W_FORMINIT		sFormInit;
	W_EDBINIT		sEdInit;

	if(widgGetFromID(psWScreen,FRONTEND_TOPFORM))
	{
		widgDelete(psWScreen,FRONTEND_TOPFORM);
	}

	if(widgGetFromID(psWScreen,MULTIOP_CHATBOX))
	{
		return;
	}

	memset(&sFormInit, 0, sizeof(W_FORMINIT));

	sFormInit.formID = FRONTEND_BACKDROP;							// add the form
	sFormInit.id = MULTIOP_CHATBOX;
	sFormInit.x = MULTIOP_CHATBOXX;
	sFormInit.y = MULTIOP_CHATBOXY;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.width = MULTIOP_CHATBOXW;
	sFormInit.height = MULTIOP_CHATBOXH;
	sFormInit.disableChildren = TRUE;								// wait till open!
	sFormInit.pDisplay = intOpenPlainForm;//intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT4,MULTIOP_CHATBOXX-3,MULTIOP_CHATBOXY,strresGetString(psStringRes, STR_MUL_CHAT));

	flushConsoleMessages();											// add the chatbox.
	initConsoleMessages();
	enableConsoleDisplay(TRUE);
	setConsoleBackdropStatus(FALSE);
	setDefaultConsoleJust(LEFT_JUSTIFY);
	setConsoleSizePos(MULTIOP_CHATBOXX+4+D_W, MULTIOP_CHATBOXY+10+D_H, MULTIOP_CHATBOXW-4);
	setConsolePermanence(TRUE,TRUE);
	setConsoleLineInfo(5);											// use x lines on chat window

	memset(&sEdInit, 0, sizeof(W_EDBINIT));							// add the edit box
	sEdInit.formID = MULTIOP_CHATBOX;
	sEdInit.id = MULTIOP_CHATEDIT;
	sEdInit.x = MULTIOP_CHATEDITX;
	sEdInit.y = MULTIOP_CHATEDITY;
	sEdInit.style = WEDB_PLAIN;
	sEdInit.width = MULTIOP_CHATEDITW;
	sEdInit.height = MULTIOP_CHATEDITH;
	sEdInit.FontID = WFont;

	sEdInit.pUserData = NULL;
	sEdInit.pBoxDisplay = displayChatEdit;

	widgAddEditBox(psWScreen, &sEdInit);

	return;
}

// ////////////////////////////////////////////////////////////////////////////
static void disableMultiButs(void)
{

	// edit box icons.
	widgSetButtonState(psWScreen,MULTIOP_GNAME_ICON,WBUT_DISABLE);
	widgSetButtonState(psWScreen,MULTIOP_MAP_ICON ,WBUT_DISABLE);

	// edit boxes
	widgSetButtonState(psWScreen,MULTIOP_GNAME,WEDBS_DISABLE);
	widgSetButtonState(psWScreen,MULTIOP_MAP,WEDBS_DISABLE);

	// force choice.
	if  (game.type == SKIRMISH || game.base != CAMP_WALLS )
//	if  (game.type == SKIRMISH ||(game.type != DMATCH && game.base != CAMP_WALLS ))
	{
		widgSetButtonState(psWScreen,MULTIOP_FNAME_ICON,WBUT_DISABLE);
		widgSetButtonState(psWScreen,MULTIOP_FNAME,WEDBS_DISABLE);
	}

	if(game.type != CAMPAIGN)	widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN, WBUT_DISABLE);
//	if(game.type != DMATCH)		widgSetButtonState(psWScreen, MULTIOP_ARENA,	WBUT_DISABLE);
	if(game.type != SKIRMISH)	widgSetButtonState(psWScreen, MULTIOP_SKIRMISH, WBUT_DISABLE);
	if(game.type != TEAMPLAY)	widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY, WBUT_DISABLE);

	if(! NetPlay.bHost)
	{
		if( game.fog) widgSetButtonState(psWScreen,MULTIOP_FOG_OFF ,WBUT_DISABLE);		//fog
		if(!game.fog) widgSetButtonState(psWScreen,MULTIOP_FOG_ON ,WBUT_DISABLE);

//		if(  game.type == DMATCH )
//		{
//			if(game.limit != NOLIMIT)	widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,WBUT_DISABLE);	// limit levels
//			if(game.limit != FRAGLIMIT)	widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,WBUT_DISABLE);
//			if(game.limit != TIMELIMIT)	widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT,WBUT_DISABLE);
//		}

		if( game.type == CAMPAIGN)
		{
			if(game.bComputerPlayers)	widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,WBUT_DISABLE);		// pow levels
			if(!game.bComputerPlayers)	widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,WBUT_DISABLE);
		}

		if(	game.type == CAMPAIGN || game.type == TEAMPLAY || game.type == SKIRMISH)
		{
			if(game.base != CAMP_CLEAN)	widgSetButtonState(psWScreen,MULTIOP_CLEAN ,WBUT_DISABLE);	// camapign subtype.
			if(game.base != CAMP_BASE)	widgSetButtonState(psWScreen,MULTIOP_BASE ,WBUT_DISABLE);
			if(game.base != CAMP_WALLS)	widgSetButtonState(psWScreen,MULTIOP_DEFENCE,WBUT_DISABLE);

			if(game.power != LEV_LOW)	widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,WBUT_DISABLE);		// pow levels
			if(game.power != LEV_MED)	widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,WBUT_DISABLE);
			if(game.power != LEV_HI )	widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI,WBUT_DISABLE);
		}


		if( /*game.type == DMATCH||*/ game.type == CAMPAIGN  || game.type == SKIRMISH)
		{
			if(game.alliance != NO_ALLIANCES)	widgSetButtonState(psWScreen,MULTIOP_ALLIANCE_N ,WBUT_DISABLE);	//alliance settings.
			if(game.alliance != ALLIANCES)	widgSetButtonState(psWScreen,MULTIOP_ALLIANCE_Y ,WBUT_DISABLE);
			if(game.alliance != ALLIANCES_TEAMS)	widgSetButtonState(psWScreen,MULTIOP_ALLIANCE_TEAMS ,WBUT_DISABLE);
		}

	}
}


////////////////////////////////////////////////////////////////////////////
static void stopJoining(void)
{
	dwSelectedGame	 = 0;
	saveConfig();

	if(bWhiteBoardUp)
	{
		removeWhiteBoard();
	}

	if(NetPlay.bLobbyLaunched)
	{
		changeTitleMode(QUIT);
		return;
	}
	else
	{
		if(bHosted)											// cancel a hosted game.
		{
			sendLeavingMsg();								// say goodbye
			NETclose();										// quit running game.
			bHosted = FALSE;								// stop host mode.
			widgDelete(psWScreen,FRONTEND_BACKDROP);		// refresh options screen.
//			startMultiOptions(FALSE);
			startMultiOptions(TRUE);
			ingame.localJoiningInProgress = FALSE;
			return;
		}
		else if(ingame.localJoiningInProgress)				// cancel a joined game.
		{
			sendLeavingMsg();								// say goodbye
			NETclose();										// quit running game.

			ingame.localJoiningInProgress = FALSE;			// reset local flags
			ingame.localOptionsReceived = FALSE;
			if(!ingame.bHostSetup && NetPlay.bHost)			// joining and host was transfered.
			{
				NetPlay.bHost = FALSE;
			}

			if(NetPlay.bComms)	// not even connected.
			{
				changeTitleMode(GAMEFIND);
				selectedPlayer =0;
			}
			else
			{
				changeTitleMode(MULTI);
				selectedPlayer =0;
			}
			return;
		}
		if(NetPlay.bComms)	// not even connected.
		{
			changeTitleMode(PROTOCOL);
			selectedPlayer =0;
		}
		else
		{
			changeTitleMode(MULTI);
			selectedPlayer =0;
		}

		if (ingame.bHostSetup) {
#ifdef COVERMOUNT
				pie_LoadBackDrop(SCREEN_COVERMOUNT);
#else
				pie_LoadBackDrop(SCREEN_RANDOMBDROP);
#endif
		}
	}
}

////////////////////////////////////////////////////////////////////////////
static void chooseSkirmishColours(void)
{
	UDWORD col,i,k;
	BOOL taken;

	col=0;
	for(i=0;i<MAX_PLAYERS;i++)	// assign each pc player a colour.
	{
		if(!isHumanPlayer(i))	// pick a colour for this player.
		{
			taken = TRUE;		// go to next unused colour.
			while(taken)
			{
				taken = FALSE;
				for(k=0;k<MAX_PLAYERS;k++)
				{
					if(isHumanPlayer(k) && getPlayerColour(k) == col)
					{
						taken = TRUE;// already taken.
					}
				}
				if(taken)
				{
					col++;
				}
			}
			setPlayerColour(i,col);
			col++;
		}
	}
}

/*
 * Process click events on the multiplayer/skirmish options screen
 * 'id' is id of the button that was pressed
 */
static void processMultiopWidgets(UDWORD id)
{
	PLAYERSTATS playerStats;
	UDWORD i;
	char	tmp[255];

	// host, who is setting up the game
	if((ingame.bHostSetup && !bHosted))
	{
		switch(id)												// Options buttons
		{

		case MULTIOP_GNAME:										// we get this when nec.
			strcpy(game.name,widgGetString(psWScreen, MULTIOP_GNAME));
			break;

		case MULTIOP_MAP:
			widgSetString(psWScreen, MULTIOP_MAP,game.map);
//			strcpy(game.map,widgGetString(psWScreen, MULTIOP_MAP));
			break;

		case MULTIOP_GNAME_ICON:
			break;

		case MULTIOP_MAP_ICON:
			widgDelete(psWScreen,MULTIOP_PLAYERS);
			widgDelete(psWScreen,FRONTEND_SIDETEXT2);					// del text too,

			strcpy(tmp,MultiCustomMapsPath);
			strcat(tmp,"*.wrf");
			debug(LOG_WZ, "processMultiopWidgets[MULTIOP_MAP_ICON]: %s", tmp);
			addMultiRequest(tmp,MULTIOP_MAP,1, 2);
			break;

//		case MULTIOP_ARENA:										// turn on arena game
//			widgSetButtonState(psWScreen, MULTIOP_ARENA, WBUT_LOCK);
//			widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN, 0);
//			widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,0);
//			widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY,0);
//			game.type = DMATCH;
//
//			widgSetString(psWScreen, MULTIOP_MAP, "DeadValley");
//			strcpy(game.map,widgGetString(psWScreen, MULTIOP_MAP));
//			game.maxPlayers =8;
//
//			addGameOptions();
//			widgSetButtonState(psWScreen, MULTIOP_FNAME,WEDBS_FIXED);
//			widgSetButtonState(psWScreen, MULTIOP_FNAME_ICON,0);
//			break;

		case MULTIOP_CAMPAIGN:									// turn on campaign game
//			widgSetButtonState(psWScreen, MULTIOP_ARENA, 0);
			widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN, WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,0);
			widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY,0);
			game.type = CAMPAIGN;
			widgSetString(psWScreen, MULTIOP_MAP, DEFAULTCAMPAIGNMAP);
			strcpy(game.map,widgGetString(psWScreen, MULTIOP_MAP));
			game.maxPlayers = 4;

			addGameOptions(FALSE);
			break;

		case MULTIOP_SKIRMISH:
//			widgSetButtonState(psWScreen, MULTIOP_ARENA, 0);
			widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN,0 );
			widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY,0);
			game.type = SKIRMISH;

			widgSetString(psWScreen, MULTIOP_MAP, DEFAULTSKIRMISHMAP);
			strcpy(game.map,widgGetString(psWScreen, MULTIOP_MAP));
			game.maxPlayers = 4;

			addGameOptions(FALSE);
			break;

		case MULTIOP_TEAMPLAY:
//			widgSetButtonState(psWScreen, MULTIOP_ARENA, 0);
			widgSetButtonState(psWScreen, MULTIOP_CAMPAIGN,0 );
			widgSetButtonState(psWScreen, MULTIOP_SKIRMISH,0);
			widgSetButtonState(psWScreen, MULTIOP_TEAMPLAY,WBUT_LOCK);

			widgSetString(psWScreen, MULTIOP_MAP, DEFAULTCAMPAIGNMAP);
			strcpy(game.map,widgGetString(psWScreen, MULTIOP_MAP));

			game.type		= TEAMPLAY;
			game.maxPlayers = 4;
			game.alliance	= ALLIANCES;

			addGameOptions(FALSE);
			break;

		}
	}

	// host who is setting up or has hosted
	if(ingame.bHostSetup)// || NetPlay.bHost) // FIXME Was: if(ingame.bHostSetup);{} ??? Note the ; !
	{
		switch(id)
		{
		case MULTIOP_COMPUTER_Y:
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,0);
			game.bComputerPlayers = TRUE;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_COMPUTER_N:
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_Y,0);
			widgSetButtonState(psWScreen, MULTIOP_COMPUTER_N,WBUT_LOCK);
			game.bComputerPlayers = FALSE;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_FOG_ON:
			widgSetButtonState(psWScreen, MULTIOP_FOG_ON,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_FOG_OFF,0);
			game.fog = TRUE;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_FOG_OFF:
			widgSetButtonState(psWScreen, MULTIOP_FOG_ON,0);
			widgSetButtonState(psWScreen, MULTIOP_FOG_OFF,WBUT_LOCK);
			game.fog = FALSE;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

/*		case MULTIOP_TECH_LOW:
			widgSetButtonState(psWScreen, MULTIOP_TECH_LOW,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_TECH_MED,0);
			widgSetButtonState(psWScreen, MULTIOP_TECH_HI ,0);
			game.techLevel = 1;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_TECH_MED:
			widgSetButtonState(psWScreen, MULTIOP_TECH_LOW,0);
			widgSetButtonState(psWScreen, MULTIOP_TECH_MED,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_TECH_HI ,0);
			game.techLevel = 2;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_TECH_HI:
			widgSetButtonState(psWScreen, MULTIOP_TECH_LOW,0);
			widgSetButtonState(psWScreen, MULTIOP_TECH_MED,0);
			widgSetButtonState(psWScreen, MULTIOP_TECH_HI ,WBUT_LOCK);
			game.techLevel = 3;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;
*/
		case MULTIOP_CLEAN:
			game.base = CAMP_CLEAN;
			addGameOptions(FALSE);
			if(bHosted)
			{
				sendOptions(0,0);
				disableMultiButs();
				addOkBut();
			}
			break;

		case MULTIOP_BASE:
			game.base = CAMP_BASE;
			addGameOptions(FALSE);
			if(bHosted)
			{
				disableMultiButs();
				sendOptions(0,0);
				addOkBut();
			}
			break;

		case MULTIOP_DEFENCE:
			game.base = CAMP_WALLS;
			addGameOptions(FALSE);
			if(bHosted)
			{
				sendOptions(0,0);
				disableMultiButs();
				addOkBut();
			}
			break;

		case MULTIOP_ALLIANCE_N:
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_N,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_Y,0);

			if(game.type != CAMPAIGN)
				widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,0);
			else
				widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,WBUT_DISABLE);

			game.alliance = NO_ALLIANCES;	//0;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_ALLIANCE_Y:
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_N,0);
			widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,0);

			if(game.type != CAMPAIGN)
				widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_Y,WBUT_LOCK);
			else
				widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,WBUT_DISABLE);

			game.alliance = ALLIANCES;	//1;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_ALLIANCE_TEAMS:	//locked teams
			setLockedTeamsMode();
			break;

		case MULTIOP_POWLEV_LOW:								// set power level to low
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,0);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI ,0);
			game.power = LEV_LOW;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_POWLEV_MED:								// set power to med
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,0);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI ,0);
			game.power = LEV_MED;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_POWLEV_HI:									// set power to hi
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_LOW,0);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_MED,0);
			widgSetButtonState(psWScreen, MULTIOP_POWLEV_HI ,WBUT_LOCK);
			game.power = LEV_HI;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_NOLIMIT:								// set power level to low
			widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,0);
			widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT ,0);
			game.limit = NOLIMIT;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_FRAGLIMIT:								// set power to med
			widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,0);
			widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,WBUT_LOCK);
			widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT ,0);
			game.limit = FRAGLIMIT;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;

		case MULTIOP_TIMELIMIT:									// set power to hi
			widgSetButtonState(psWScreen, MULTIOP_NOLIMIT,0);
			widgSetButtonState(psWScreen, MULTIOP_FRAGLIMIT,0);
			widgSetButtonState(psWScreen, MULTIOP_TIMELIMIT,WBUT_LOCK);
			game.limit = TIMELIMIT;
			if(bHosted)
			{
				sendOptions(0,0);
			}
			break;
		}
	}

	// these work all the time.
	switch(id)
	{

	case MULTIOP_WHITEBOARD:
		if(bWhiteBoardUp)
		{
			removeWhiteBoard();
		}
		else
		{
			addWhiteBoard();
		}
		break;

	case MULTIOP_STRUCTLIMITS:
		changeTitleMode(MULTILIMIT);
		break;

	case MULTIOP_FNAME:
		strcpy(sForceName,widgGetString(psWScreen, MULTIOP_FNAME));
		removeWildcards(sForceName);
		break;

	case MULTIOP_FNAME_ICON:
		widgDelete(psWScreen,MULTIOP_PLAYERS);
		widgDelete(psWScreen,FRONTEND_SIDETEXT2);					// del text too,
		strcpy(tmp,MultiForcesPath);
		strcat(tmp,"*.for");
		addMultiRequest(tmp,MULTIOP_FNAME,0,0);
		break;

	case MULTIOP_PNAME:
		strcpy(sPlayer,widgGetString(psWScreen, MULTIOP_PNAME));

		// chop to 15 chars..
		while(strlen(sPlayer) > 15)	// clip name.
		{
			sPlayer[strlen(sPlayer)-1]='\0';
		}

		// update string.
		widgSetString(psWScreen, MULTIOP_PNAME,sPlayer);


		removeWildcards((char*)sPlayer);

		sprintf(tmp,"-> %s",sPlayer);
		sendTextMessage(tmp,TRUE);

		NETchangePlayerName(NetPlay.dpidPlayer, (char*)sPlayer);			// update if joined.
		loadMultiStats((char*)sPlayer,&playerStats);
		setMultiStats(NetPlay.dpidPlayer,playerStats,FALSE);
		setMultiStats(NetPlay.dpidPlayer,playerStats,TRUE);
		break;

	case MULTIOP_PNAME_ICON:
		widgDelete(psWScreen,MULTIOP_PLAYERS);
		widgDelete(psWScreen,FRONTEND_SIDETEXT2);					// del text too,

		strcpy(tmp,MultiPlayersPath);
		strcat(tmp,"*.sta");
		addMultiRequest(tmp,MULTIOP_PNAME,0,0);
		break;

	case MULTIOP_HOST:
		if(game.type != SKIRMISH)
		{
			strcpy(sForceName,widgGetString(psWScreen, MULTIOP_FNAME));
		}
		strcpy((char*)game.name,widgGetString(psWScreen, MULTIOP_GNAME));	// game name
		strcpy((char*)sPlayer,widgGetString(psWScreen, MULTIOP_PNAME));	// pname
		strcpy((char*)game.map,widgGetString(psWScreen, MULTIOP_MAP));		// add the name

		removeWildcards((char*)sPlayer);
		removeWildcards(sForceName);

//		if (game.type == DMATCH)
//		{
//			hostArena((char*)game.name,(char*)sPlayer);
//			bHosted = TRUE;
//		}
//		else
//		{
			hostCampaign((char*)game.name,(char*)sPlayer);
			bHosted = TRUE;
//		}

	// wait for players, when happy, send options.
		if(NetPlay.bLobbyLaunched)
		{
			for(i=0;i<MAX_PLAYERS;i++)	// send options to everyone.
			{
				if(isHumanPlayer(i))
				{
					sendOptions(player2dpid[i],i);
				}
			}
		}

		widgDelete(psWScreen,MULTIOP_REFRESH);
		widgDelete(psWScreen,MULTIOP_HOST);

		ingame.localOptionsReceived = TRUE;

		addGameOptions(FALSE);									// update game options box.
		addChatBox();
		addOkBut();

		disableMultiButs();

		addPlayerBox(!ingame.bHostSetup || bHosted);	//to make sure host can't skip player selection menu (sets game.skdiff to UBYTE_MAX for humans)
		break;

	case MULTIOP_CHATEDIT:

		// don't send empty lines to other players in the lobby
		if(!strcmp(widgGetString(psWScreen, MULTIOP_CHATEDIT), ""))
			break;

		sendTextMessage(widgGetString(psWScreen, MULTIOP_CHATEDIT),TRUE);					//send
		widgSetString(psWScreen, MULTIOP_CHATEDIT, "");										// clear box
		break;

	case CON_OK:
		decideWRF();										// set up swrf & game.map
		strcpy(sForceName,widgGetString(psWScreen, MULTIOP_FNAME));
		removeWildcards(sForceName);
		bMultiPlayer = TRUE;

		if(NetPlay.bHost)
		{

			///////////
			//
			if(game.type == SKIRMISH)
			{
				chooseSkirmishColours();
				sendOptions(0,0);
			}
			// end of skirmish col choose
			////////

			resetCheatHash();

			NEThaltJoining();							// stop new players entering.
			SendFireUp();								//bcast a fireup message
		}

		// set the fog correctly..
		setRevealStatus(game.fog);

	if(bWhiteBoardUp)
	{
		removeWhiteBoard();
	}

		changeTitleMode(STARTGAME);

		bHosted = FALSE;

		if(NetPlay.bHost)
		{
			sendTextMessage(strresGetString(psStringRes, STR_MUL_STARTING),TRUE);
		}
		return;
		break;

	case CON_CANCEL:
		stopJoining();
		break;

	default:
		break;
	}

	//pop-up team chooser
	if((id >= MULTIOP_TEAMS_START) && (id <= MULTIOP_TEAMS_END))	// clicked on a team chooser
	{
		UDWORD clickedMenuID = id-MULTIOP_TEAMS_START,playerNetID;
		BOOL	bClickedOnMe = (NetPlay.players[clickedMenuID].dpid == player2dpid[selectedPlayer]);

		//find net player id
		for(playerNetID=0;(playerNetID <= MAX_PLAYERS) && (player2dpid[selectedPlayer] != NetPlay.players[playerNetID].dpid);playerNetID++);

		ASSERT(playerNetID < MAX_PLAYERS, "processMultiopWidgets: failed to find playerNetID for player %d", selectedPlayer);

		//make sure team chooser is not up before adding new one for another player
		if(teamChooserUp() < 0 && !bColourChooserUp)
		{
			if(bClickedOnMe		//player selecting team for himself	(clicked on his menu)
				|| (NetPlay.bHost							//local player=host
				&& !((NetPlay.players[clickedMenuID].dpid > 0) && !bClickedOnMe)	//human and not me
				))
				addTeamChooser(clickedMenuID);
		}
	}

	//clicked on a team
	if((id >= MULTIOP_TEAMCHOOSER) && (id <= MULTIOP_TEAMCHOOSER_END))
	{
		char msg[255];

		ASSERT(teamChooserUp() >= 0, "processMultiopWidgets: teamChooserUp() < 0");
		ASSERT((id - MULTIOP_TEAMCHOOSER) >= 0
			&& (id - MULTIOP_TEAMCHOOSER) < MAX_PLAYERS, "processMultiopWidgets: wrong id - MULTIOP_TEAMCHOOSER value (%d)", id - MULTIOP_TEAMCHOOSER);

		SendTeamRequest(teamChooserUp(),(UBYTE)id-MULTIOP_TEAMCHOOSER);

		//playerTeamGUI[teamChooserUp()] = id - MULTIOP_TEAMCHOOSER;

		debug(LOG_WZ, "Changed team for player %d to %d", teamChooserUp(), playerTeamGUI[teamChooserUp()]);

		closeTeamChooser();
		addPlayerBox(  !ingame.bHostSetup || bHosted);	//restore initial options screen

		//enable locked teams mode
		//-----------------------------
		if(game.alliance != ALLIANCES_TEAMS && bHosted)		//only if host
		{
			setLockedTeamsMode();		//update GUI

			sprintf( msg,"'%s' mode enabled", strresGetString(psStringRes, STR_MUL_ALLIANCE_TEAMS) );
			addConsoleMessage(msg,DEFAULT_JUSTIFY);
		}
	}

	if((id >= MULTIOP_PLAYER_START) && (id <= MULTIOP_PLAYER_END))	// clicked on a player
	{
		if(NetPlay.players[id-MULTIOP_PLAYER_START].dpid == player2dpid[selectedPlayer] )
		{
			if(teamChooserUp() < 0)		//not choosing team already
				addColourChooser(id-MULTIOP_PLAYER_START);
		}
		else // options for kick out, etc..
		{
			if(NetPlay.bHost)
			{
				if(mouseDown(MOUSE_RMB)) // both buttons....
				{
					kickPlayer(NetPlay.players[id-MULTIOP_PLAYER_START].dpid);	// kick out that player.
				}
			}
		}
	}

	if((id >= MULTIOP_SKSLIDE) && (id <=MULTIOP_SKSLIDE_END)) // choseskirmish difficulty.
	{
		UDWORD newValue, oldValue;

		if(		(id-MULTIOP_SKSLIDE == game.maxPlayers-1)
			//&& 	(game.skDiff[id-MULTIOP_SKSLIDE] == 0)
			&& (widgGetSliderPos(psWScreen,id) == 0)
			)
		{
			game.skDiff[id-MULTIOP_SKSLIDE] = 1;
			widgSetSliderPos(psWScreen,id,1);
		}

		newValue = widgGetSliderPos(psWScreen,id);
		oldValue = (UDWORD)game.skDiff[id-MULTIOP_SKSLIDE];

		game.skDiff[id-MULTIOP_SKSLIDE] = (UBYTE)newValue;

		//Show/hide team chooser if player was enabled/disabled
		if((oldValue == 0 && newValue > 0) || (oldValue > 0 && newValue == 0) )
		{
			closeTeamChooser();
			addPlayerBox(  !ingame.bHostSetup || bHosted);	//restore initial options screen
		}

		sendOptions(0,0);
	}

	// don't kill last player
	if((id >= MULTIOP_COLCHOOSER) && (id <= MULTIOP_COLCHOOSER_END)) // chose a new colour.
	{
		SendColourRequest(selectedPlayer,id-MULTIOP_COLCHOOSER,UBYTE_MAX);
		closeColourChooser();
		addPlayerBox(  !ingame.bHostSetup || bHosted);
	}

	// request a player number.
	if((id >= MULTIOP_PLAYCHOOSER) && (id <= MULTIOP_PLAYCHOOSER_END)) // chose a new colour.
	{
		SendColourRequest(selectedPlayer,UBYTE_MAX,id-MULTIOP_PLAYCHOOSER);
		closeColourChooser();
		addPlayerBox(  !ingame.bHostSetup || bHosted);
	}

}

// ////////////////////////////////////////////////////////////////////////////
// Net message handling

void frontendMultiMessages(void)
{
	NETMSG			msg;			// a blank msg.
	UDWORD			i;
	UDWORD			dp;
	UBYTE			bTemp;

	while(NETrecv(&msg))
	{
		switch(msg.type)
		{
		case AUDIOMSG:
			recvAudioMsg(&msg);
			break;

		case NET_REQUESTMAP:
			recvMapFileRequested(&msg);
			break;
		case FILEMSG:
			recvMapFileData(&msg);
			break;


		case NET_OPTIONS:					// incoming options file.
			recvOptions(&msg);
			ingame.localOptionsReceived = TRUE;

			if(titleMode == MULTIOPTION)
			{
				addGameOptions(TRUE);
				disableMultiButs();
				addChatBox();
			}
			break;

		case NET_WHITEBOARD:
			if(!bWhiteBoardUp)
			{
				addWhiteBoard();
			}
			memcpy(&whiteBoard[(int)msg.body[0]], &msg.body[1], NUMWHITE*2);

			break;

		case NET_ALLIANCE:
			recvAlliance(&msg,FALSE);
			break;

		case NET_COLOURREQUEST:
			recvColourRequest(&msg);
			break;

		case NET_TEAMREQUEST:
			recvTeamRequest(&msg);
			break;

		case NET_PING:						// diagnostic ping msg.
			recvPing(&msg);
			break;

		case NET_LEAVING:					// remote player leaving.
			NetGet((&msg),0,dp);
			NetGet((&msg),4,bTemp);
			MultiPlayerLeave(dp);
			if(bTemp)					// host has quit, need to quit too.
			{
				stopJoining();
			}
			break;

		case NET_PLAYERRESPONDING:			// remote player is now playing.
			NetGet((&msg),0,i);
			ingame.JoiningInProgress[i] = FALSE;
			break;

		case NET_FIREUP:					// campaign game started.. can fire the whole shebang up...
			if(ingame.localOptionsReceived)
			{
				resetCheatHash();

				decideWRF();

				if(game.type != SKIRMISH)	// force stuff.
				{
					strcpy(sForceName,widgGetString(psWScreen, MULTIOP_FNAME));
					removeWildcards(sForceName);
				}


				// set the fog correctly..
				setRevealStatus(game.fog);

				bMultiPlayer = TRUE;
				if(bWhiteBoardUp)
				{
				removeWhiteBoard();
				}
				changeTitleMode(STARTGAME);
				bHosted = FALSE;
				break;
			}

		case NET_KICK:						// player is forcing someone to leave
			NetGet((&msg),0,dp);
			if(NetPlay.dpidPlayer == dp)	// we've been told to leave.
			{
				stopJoining();
			}
			break;

		case NET_TEXTMSG:					// Chat message
			if(ingame.localOptionsReceived)
			{
				recvTextMessage(&msg);
			}
			break;
		}
	}
}

void runMultiOptions(void)
{
	static UDWORD	lastrefresh=0;
	UDWORD			id,value;//,i;
	char			sTemp[128];
	PLAYERSTATS		playerStats;
	W_CONTEXT		context;

	KEY_CODE		k;
	char			str[3];

	processFrontendSnap(FALSE);

	frontendMultiMessages();


	// keep sending the map if required.
	if(bSendingMap)
	{
		sendMap();
	}

	// update boxes?
	if(lastrefresh > gameTime)lastrefresh= 0;
	if ((gameTime - lastrefresh) >2000)
	{
		lastrefresh= gameTime;
		if( !multiRequestUp
			&& (bHosted
				|| (ingame.localJoiningInProgress && !NetPlay.bLobbyLaunched)
				|| (NetPlay.bLobbyLaunched && ingame.localOptionsReceived)
				)
			)
		{

			// store the slider settings if they are up,
			for(id=0;id<MAX_PLAYERS;id++)
			{
				if(widgGetFromID(psWScreen,MULTIOP_SKSLIDE+id))
				{
					value = widgGetSliderPos(psWScreen,MULTIOP_SKSLIDE+id);
					if(value != game.skDiff[id])
					{

						if(value == 0 && (id == game.maxPlayers-1)  )
						{
							game.skDiff[id] = 1;
							widgSetSliderPos(psWScreen,id+MULTIOP_SKSLIDE,1);
						}
						else
						{
							game.skDiff[id] = value;
						}


						if(NetPlay.bHost)
						{
							sendOptions(0,0);
						}
					}
				}
			}


			addPlayerBox(TRUE);				// update the player box.
		}
	}


	// update scores and pings if far enough into the game
	if(ingame.localOptionsReceived && ingame.localJoiningInProgress)
	{
		sendScoreCheck();
		sendPing();
	}

	// check for being able to go!
//	if(ingame.localJoiningInProgress
//		&& (widgGetFromID(psWScreen,CON_OK) == NULL)	// oks not yet there.
//		&& ingame.localOptionsReceived					// weve got the options
//		&& (game.type == DMATCH))						// it's a dmatch game
//	{
//		for(i=0;i<MAX_PLAYERS;i++)
//		{
//			if( isHumanPlayer(i) && !ingame.JoiningInProgress[i])			// only go when someones ready..
//			{
//				addOkBut();
//				break;
//			}
//		}
//	}

	// if typing and not in an edit box then jump to chat box.
	k = getQwertyKey();
	if(	k && psWScreen->psFocus == NULL)
	{
		context.psScreen	= psWScreen;
		context.psForm		= (W_FORM *)psWScreen->psForm;
		context.xOffset		= 0;
		context.yOffset		= 0;
		context.mx			= mouseX();
		context.my			= mouseY();

		keyScanToString(k,(char*)&str,3);
		if(widgGetFromID(psWScreen,MULTIOP_CHATEDIT))
		{
			widgSetString(psWScreen, MULTIOP_CHATEDIT, (char*)&str);	// start it up!
			editBoxClicked((W_EDITBOX*)widgGetFromID(psWScreen,MULTIOP_CHATEDIT), &context);
		}
	}

	// chat box handling
	if(widgGetFromID(psWScreen,MULTIOP_CHATBOX))
	{
		while(getNumberConsoleMessages() >getConsoleLineInfo())
		{
			removeTopConsoleMessage();
		}
		updateConsoleMessages();								// run the chatbox
	}

	// widget handling

	if(multiRequestUp)
	{
		id = widgRunScreen(psRScreen);						// a requester box is up.
		if( runMultiRequester(id,&id,(char*)&sTemp,&value))
		{
			switch(id)
			{
			case MULTIOP_PNAME:
				strcpy((char*)sPlayer,sTemp);
				widgSetString(psWScreen,MULTIOP_PNAME,sTemp);

				sprintf(sTemp," -> %s",sPlayer);
				sendTextMessage(sTemp,TRUE);

				NETchangePlayerName(NetPlay.dpidPlayer, (char*)sPlayer);
				loadMultiStats((char*)sPlayer,&playerStats);
				setMultiStats(NetPlay.dpidPlayer,playerStats,FALSE);
				setMultiStats(NetPlay.dpidPlayer,playerStats,TRUE);
				break;
			case MULTIOP_FNAME:
				strcpy(sForceName,sTemp);
				widgSetString(psWScreen,MULTIOP_FNAME,sTemp);
				break;
			case MULTIOP_MAP:
				strcpy(game.map,sTemp);
				game.maxPlayers =(UBYTE) value;
				loadMapPreview();

				widgSetString(psWScreen,MULTIOP_MAP,sTemp);
				addGameOptions(FALSE);
				break;
			default:
				break;
			}
			addPlayerBox( !ingame.bHostSetup );
		}
	}
	else
	{

		if(hideTime != 0)
		{
			if(gameTime-hideTime <1500)
			{
				return;
			}
			hideTime = 0;
		}

		id = widgRunScreen(psWScreen);								// run the widgets.
		processMultiopWidgets(id);
	}

	StartCursorSnap(&InterfaceSnap);

	widgDisplayScreen(psWScreen);									// show the widgets currently running

	if(multiRequestUp)
	{
		widgDisplayScreen(psRScreen);								// show the Requester running
	}

	if(widgGetFromID(psWScreen,MULTIOP_CHATBOX))
	{
		iV_SetFont(WFont);											// switch to small font.
		displayConsoleMessages();									// draw the chatbox
	}

	runWhiteBoard();
}

// ////////////////////////////////////////////////////////////////////////////
BOOL startMultiOptions(BOOL bReenter)
{
	PLAYERSTATS		nullStats;
	UBYTE i;

	addBackdrop();
	addTopForm();

	if(!bReenter)
	{
		initPlayerColours();			 // force a colour clearout.

		for(i=0;i<MAX_PLAYERS;i++)
		{
//			game.skirmishPlayers[i] = 1; // clear out skirmish setting
//			game.skDiff[i] = (rand()%19)+1;	//1-20
			game.skDiff[i] = 10;
		}

/*		//set defaults for game.
		game.power					= LEV_MED;
		if(NetPlay.bComms)
		{
			game.type				= CAMPAIGN;
			strcpy(game.map, DEFAULTCAMPAIGNMAP);
		}
		else
		{
			game.type				= SKIRMISH;
			strcpy(game.map, DEFAULTSKIRMISHMAP);
		}
		game.techLevel				= 1;
		game.base					= CAMP_BASE;
//		game.bHaveServer			= FALSE;
		game.limit					= NOLIMIT;
//		game.packetsPerSec			= 6;
		game.maxPlayers				= 4;
		game.bComputerPlayers		= FALSE;
		strcpy(sForceName,	"Default");
*/

		// set the encrypt key.
		if(NetPlay.bHost)
		{
			game.encryptKey = 0;
		}

		if(!NetPlay.bComms)			// firce skirmish if no comms.
		{
			game.type				= SKIRMISH;
			strcpy(game.map, DEFAULTSKIRMISHMAP);
			game.maxPlayers = 4;
		}

		strncpy(game.version, buildTime, 8);		// note buildtime.

		ingame.localOptionsReceived = FALSE;
		if(ingame.numStructureLimits)
		{
			ingame.numStructureLimits = 0;
			FREE(ingame.pStructureLimits);
		}

		// check the registry for setup entries and set game options.
//#ifndef NOREGCHECK
//		NETcheckRegistryEntries("Warzone2100",S_WARZONEGUID);		// check for registry entries.. warn if not ok...
//#endif

		if(NetPlay.bLobbyLaunched)
		{
			game.bytesPerSec	= INETBYTESPERSEC;					// maximum bitrate achieved before dropping checks.
			game.packetsPerSec	= INETPACKETS;
		}

		loadMultiStats((char*)sPlayer,&nullStats);

	}

	addPlayerBox(FALSE);								// Players
	addGameOptions(FALSE);

	if(NetPlay.bLobbyLaunched)
	{
		if(!NetPlay.bHost)
		{
			ingame.localJoiningInProgress = TRUE;
			widgDelete(psWScreen,MULTIOP_REFRESH);
		}
		ingame.localOptionsReceived = FALSE;
	}

	addChatBox();

	addMultiBut(psWScreen,
		FRONTEND_BACKDROP,
		MULTIOP_WHITEBOARD,
		MULTIOP_CHATBOXX-15,MULTIOP_CHATBOXY+MULTIOP_CHATBOXH-15 ,9,9,
		0,IMAGE_PENCIL,IMAGE_PENCIL,TRUE);	//whiteboard icon

	// going back to multiop after setting limits up..
	if(bReenter && bHosted)
	{
		disableMultiButs();
		addOkBut();
	}

	return TRUE;
}


// ////////////////////////////////////////////////////////////////////////////
// ////////////////////////////////////////////////////////////////////////////
// Force Select Screen.

// ////////////////////////////////////////////////////////////////////////////


static void CurrentForce(void)
{
	W_FORMINIT		sFormInit;
	UDWORD			numButtons, butPerForm;
	UDWORD i;
	FORCE_MEMBER	*pF;

	W_FORMINIT		sButInit;
	char			aButText[6]; //???
	SDWORD			BufferID;

	widgDelete(psWScreen,FORCE_CURRENT);
	widgDelete(psWScreen,FRONTEND_SIDETEXT3);

	ClearObjectBuffers();

	/* Count the number of minor tabs needed for the template form
	 * Also need one for the new template button */

	numButtons = 0;
	for(pF = Force.pMembers; pF; pF= pF->psNext)
	{
		numButtons++;
	}

	/* Calculate how many buttons will go on a single form */
	butPerForm = ((FORCE_CURRENTWIDTH - 0 - 2) /
						(OBJ_BUTWIDTH +2)) *
				 ((FORCE_CURRENTHEIGHT - 0- 2) /
						(OBJ_BUTHEIGHT+ 2));

	/* add a form to place the tabbed form on */
	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID = FRONTEND_BACKDROP;
	sFormInit.id = FORCE_CURRENT;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.x = FORCE_CURRENTX;
	sFormInit.y = FORCE_CURRENTY;
	sFormInit.width = FORCE_CURRENTWIDTH;
	sFormInit.height = FORCE_CURRENTHEIGHT+4;
	sFormInit.pDisplay = intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT3,FORCE_CURRENTX-1,FORCE_CURRENTY,strresGetString(psStringRes,STR_MUL_SIDEFORCE));

	/* Add the design templates form */
	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID = FORCE_CURRENT;
	sFormInit.id = FORCE_CURRENTFORM;
	sFormInit.style = WFORM_TABBED;
	sFormInit.x = 2;
	sFormInit.y = 2;
	sFormInit.width = FORCE_CURRENTWIDTH;
	sFormInit.height = FORCE_CURRENTHEIGHT;
	sFormInit.numMajor = numForms(numButtons, butPerForm);
	sFormInit.majorPos = WFORM_TABTOP;
	sFormInit.minorPos = WFORM_TABNONE;
	sFormInit.majorSize = OBJ_TABWIDTH;
	sFormInit.majorOffset = OBJ_TABOFFSET;
	sFormInit.tabVertOffset = (OBJ_TABHEIGHT/2);			//(DES_TAB_HEIGHT/2)+2;
	sFormInit.tabMajorThickness = OBJ_TABHEIGHT;
	sFormInit.pFormDisplay = intDisplayObjectForm;
	sFormInit.pUserData = (void*)&StandardTab;
	sFormInit.pTabDisplay = intDisplayTab;
	for (i=0; i< sFormInit.numMajor; i++)
	{
		sFormInit.aNumMinors[i] = 1;
	}
	widgAddForm(psWScreen, &sFormInit);

	/* Put the buttons on it */


	memset(aButText, 0, 6);  //??
	memset(&sButInit, 0, sizeof(W_BUTINIT));

	/* Set up the button struct */
	sButInit.formID = FORCE_CURRENTFORM;
	sButInit.id		= FORCE_FORCE;
	sButInit.style	= WFORM_CLICKABLE;
	sButInit.x		= 2;
	sButInit.y		= 2;
	sButInit.width	= OBJ_BUTWIDTH;
	sButInit.height = OBJ_BUTHEIGHT;

	for(pF = Force.pMembers;pF; pF=pF->psNext)
	{
		/* Set the tip and add the button */
		strncpy(aButText, getTemplateName(pF->pTempl), 5);
		sButInit.pTip = getTemplateName(pF->pTempl);

		BufferID = GetObjectBuffer();
		ASSERT( BufferID >= 0,"Unable to aquire Obj Buffer." );
		RENDERBUTTON_INUSE(&ObjectBuffers[BufferID]);
		ObjectBuffers[BufferID].Data = (void*)pF->pTempl;
		sButInit.pUserData = (void*)&ObjectBuffers[BufferID];
		sButInit.pDisplay = intDisplayTemplateButton;

		widgAddForm(psWScreen, &sButInit);

		/* Update the init struct for the next button */
		sButInit.id += 1;
		sButInit.x = (SWORD)(sButInit.x + OBJ_BUTWIDTH + 2);
		if (sButInit.x + OBJ_BUTWIDTH + 2 > FORCE_CURRENTWIDTH)
		{
			sButInit.x = 2;
			sButInit.y = (SWORD)(sButInit.y + OBJ_BUTHEIGHT + 2);
		}
		if (sButInit.y + OBJ_BUTHEIGHT + 2 > FORCE_CURRENTHEIGHT)
		{
			sButInit.y = 2;
			sButInit.majorID += 1;
		}
	}
}


//////////////////////////////////
static void AvailableForces(void)
{
	W_FORMINIT		sFormInit;
	DROID_TEMPLATE	*psTempl;
	UDWORD			numButtons, butPerForm;
	UDWORD i;

	/* init template list */
	memset( apsTemplateList, 0, sizeof(DROID_TEMPLATE*) * MAXTEMPLATES );
	numButtons = 0;
	psTempl = apsDroidTemplates[FORCEEDITPLAYER];

	while ((psTempl != NULL) && (numButtons < MAXTEMPLATES))
	{
		apsTemplateList[numButtons] = psTempl;

		/* Count the number of minor tabs needed for the template form */
		numButtons++;

		/* next template */
		psTempl = psTempl->psNext;
	}

	widgDelete(psWScreen,IDDES_TEMPLBASE);
	widgDelete(psWScreen,FRONTEND_SIDETEXT1);
	ClearStatBuffers();


	/* Calculate how many buttons will go on a single form */
	butPerForm = ((FORCE_AVAILABLEWIDTH - 0 - 2) /
						(OBJ_BUTWIDTH +2)) *
				 ((FORCE_AVAILABLEHEIGHT - 0- 2) /
						(OBJ_BUTHEIGHT+ 2));

	/* add a form to place the tabbed form on */
	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID = FRONTEND_BACKDROP;
	sFormInit.id = IDDES_TEMPLBASE;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.x = FORCE_AVAILABLEX;
	sFormInit.y = FORCE_AVAILABLEY;
	sFormInit.width = FORCE_AVAILABLEWIDTH;
	sFormInit.height = FORCE_AVAILABLEHEIGHT+4;

	sFormInit.pDisplay = intDisplayPlainForm;

	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT1,FORCE_AVAILABLEX-1,FORCE_AVAILABLEY,strresGetString(psStringRes, STR_MUL_SIDETEMPLATES));

	/* Add the design templates form */
	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID = IDDES_TEMPLBASE;
	sFormInit.id = IDDES_TEMPLFORM;
	sFormInit.style = WFORM_TABBED;
	sFormInit.x = 2;
	sFormInit.y = 2;
	sFormInit.width = FORCE_AVAILABLEWIDTH;
	sFormInit.height = FORCE_AVAILABLEHEIGHT;
	sFormInit.numMajor = numForms(numButtons, butPerForm);
	sFormInit.majorPos = WFORM_TABTOP;
	sFormInit.minorPos = WFORM_TABNONE;
	sFormInit.majorSize = OBJ_TABWIDTH;
	sFormInit.majorOffset = OBJ_TABOFFSET;
	sFormInit.tabVertOffset = (OBJ_TABHEIGHT/2);
	sFormInit.tabMajorThickness = OBJ_TABHEIGHT;
	sFormInit.pFormDisplay = intDisplayObjectForm;
	sFormInit.pUserData = (void*)&StandardTab;
	sFormInit.pTabDisplay = intDisplayTab;
	for (i=0; i< sFormInit.numMajor; i++)
	{
		sFormInit.aNumMinors[i] = 1;
	}
	widgAddForm(psWScreen, &sFormInit);

	/* Put the buttons on it */
	intAddTemplateButtons(IDDES_TEMPLFORM, FORCE_AVAILABLEWIDTH ,
							   FORCE_AVAILABLEHEIGHT,
							   OBJ_BUTWIDTH,OBJ_BUTHEIGHT,2,
							   NULL);


}


// ////////////////////////////////////////////////////////////////////////////
void runForceSelect(void)
{
	UDWORD			currID,id,i;
	DROID_TEMPLATE	*psTempl;
	UDWORD			posx,posy;
	FORCE_MEMBER	*pF;
	char  dir[256];

	posx = 5;
	posy = 5;

	processFrontendSnap(FALSE);

	if(bLoadSaveUp)
	{
		if(runLoadSave(FALSE))// check for file name.
		{
			if(strlen(sRequestResult))
			{
				debug( LOG_NEVER, "Returned %s", sRequestResult );
				if(bRequestLoad)
				{
					loadForce(sRequestResult);
					AvailableForces();									// update force screen..
					CurrentForce();
				}
				else
				{
					saveForce(sRequestResult,&Force);
					AvailableForces();									// update force screen..
					CurrentForce();
				}
			}
		}
	}
	else
	{
		// MouseOver stuff.
		id = widgGetMouseOver(psWScreen);
		if (id >=IDDES_TEMPLSTART  && id <= IDDES_TEMPLEND)
		{
			currID = IDDES_TEMPLSTART;
			for(psTempl = apsDroidTemplates[selectedPlayer]; psTempl;psTempl = psTempl->psNext)
			{
				if (currID == id)
				{
					break;
				}
				currID ++;
			}
			intSetShadowPower(calcTemplatePower(psTempl));	//update the power bars
		}
		else
		{
			intSetShadowPower(0);
		}


		id = widgRunScreen(psWScreen);								// run the widgets.

		if (id >= IDDES_TEMPLSTART && id <= IDDES_TEMPLEND)
		{
			currID = IDDES_TEMPLSTART;
			for(psTempl = apsDroidTemplates[selectedPlayer]; psTempl;psTempl = psTempl->psNext)
			{
				if (currID == id)
				{
					break;
				}
				currID ++;
			}

			if(!addToForce(psTempl))								// store this droid in force list.
			{
				widgDisplayScreen(psWScreen);
				return;
			}

			CurrentForce();
		}

		if((id >= FORCE_FORCE)&&(id <= FORCE_FORCEEND))				// FORCE droid selected
		{
			pF = Force.pMembers;									// dont delete if it's frozen
			for(i=0;i<(id-FORCE_FORCE);i++)pF=pF->psNext;			// goto that force element;
			removeFromForce(id-FORCE_FORCE);						//delete it from force.
			CurrentForce();

		}

		switch(id)
		{

		case CON_CANCEL:										// dont continue, return to options
			while(Force.pMembers)
			{
				removeFromForce(0);								// delete each force member.
			}
			selectedPlayer = 0;									// just in case.
			changeTitleMode(MULTI);

 	  		eventReset();
//			resReleaseBlockData(500);
			resReleaseBlockData(501);
			resReleaseBlockData(502);
			bForceEditorLoaded = FALSE;

			break;

		case FORCE_PRESETCLEAR:									// clear force
			while(Force.pMembers)
			{
				removeFromForce(0);								// delete each force member.
			}
			AvailableForces();									// update force screen
			CurrentForce();
			break;

		case FORCE_PRESETDEFAULT:
			strcpy(dir,"multiplay/forces/default.for");
			loadForce(dir);
			AvailableForces();										// update force screen
			CurrentForce();
			break;

		case FORCE_LOAD:
			addLoadSave(LOAD_FORCE,MultiForcesPath,"for",strresGetString(psStringRes,STR_MUL_LOAD) );
			break;

		case FORCE_SAVE:
			addLoadSave(SAVE_FORCE,MultiForcesPath,"for",strresGetString(psStringRes,STR_MUL_SAVE) );
			break;

		default:
			break;
		}
	}

	StartCursorSnap(&InterfaceSnap);

	widgDisplayScreen(psWScreen);							// show the widgets currently running
	if(bLoadSaveUp)
	{
		displayLoadSave();
	}

	return;
}


// ////////////////////////////////////////////////////////////////////////////
BOOL startForceSelect(void)
{
	W_FORMINIT		sFormInit;
	W_BARINIT		sBarInit;
	char			dir[256];
	DROID_TEMPLATE	*psTempl;

	copyTemplateSet(DEATHMATCHTEMPLATES,FORCEEDITPLAYER);

	initTemplatePoints();

	selectedPlayer = FORCEEDITPLAYER;
	setPower(selectedPlayer,FORCEEDIT_POWER);

	strcpy(dir,"multiplay/forces/default.for");		// start with default force.
	if(!loadForce(dir))
	{
		debug( LOG_NEVER, "Error Loading Force" );
	}

	addBackdrop();
	addTopForm();

	// init power profiles.
	for(psTempl = apsDroidTemplates[FORCEEDITPLAYER]; psTempl; psTempl = psTempl->psNext)
	{
		psTempl->powerPoints = calcTemplatePower(psTempl);
	}


	AvailableForces();										// available forces
	CurrentForce();											// current force

	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID = FRONTEND_BACKDROP;							// stats box
	sFormInit.id = FORCE_STATS;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.x = FORCE_STATSX;
	sFormInit.y = FORCE_STATSY;
	sFormInit.width = FORCE_STATSWIDTH;
	sFormInit.height = FORCE_STATSHEIGHT;
	sFormInit.pDisplay = intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);

	addSideText(FRONTEND_SIDETEXT2,FORCE_STATSX-1,FORCE_STATSY,strresGetString(psStringRes, STR_MUL_SIDEINFO));

	sFormInit.formID		= FORCE_STATS;							// stats box
	sFormInit.id			= FORCE_DROID;
	sFormInit.style			= WFORM_PLAIN;
	sFormInit.x				= FORCE_DROIDX;
	sFormInit.y				= FORCE_DROIDY;
	sFormInit.width			= 100;
	sFormInit.height		= 100;
	sFormInit.pDisplay		= displayForceDroid;
//	sFormInit.pUserData		= (void*)Force->pTempl;
	widgAddForm(psWScreen, &sFormInit);

// name/techlevel/powerrequired/powerleft

	memset(&sBarInit, 0, sizeof(W_BARINIT));		//power bar
	sBarInit.formID = FRONTEND_BACKDROP;	//IDPOW_FORM;
	sBarInit.id = IDPOW_POWERBAR_T;
	sBarInit.style = WBAR_TROUGH;
	sBarInit.orientation = WBAR_LEFT;
	sBarInit.x = (SWORD)FORCE_POWERX;
	sBarInit.y = (SWORD)FORCE_POWERY;
	sBarInit.width = POW_BARWIDTH;
	sBarInit.height = iV_GetImageHeight(IntImages,IMAGE_PBAR_EMPTY);
	sBarInit.sCol.red = POW_CLICKBARMAJORRED;
	sBarInit.sCol.green = POW_CLICKBARMAJORGREEN;
	sBarInit.sCol.blue = POW_CLICKBARMAJORBLUE;
	sBarInit.pDisplay = intDisplayPowerBar;
	sBarInit.iRange = POWERBAR_SCALE;
 	widgAddBarGraph(psWScreen, &sBarInit);

	addMultiBut(psWScreen,FORCE_STATS,CON_CANCEL,FORCE_OKX,FORCE_OKY,MULTIOP_OKW,MULTIOP_OKH,
				STR_MUL_CANCEL,IMAGE_RETURN,IMAGE_RETURN_HI,FALSE);			// cancel

	addMultiBut(psWScreen,FORCE_STATS,FORCE_PRESETCLEAR,FORCE_PRESETCLEARX,FORCE_PRESETCLEARY,FORCE_BUTW,FORCE_BUTH,
		STR_MUL_CLEAR,IMAGE_CLEARFORCE,IMAGE_CLEARFORCE,TRUE);			// clear.

	addMultiBut(psWScreen,FORCE_STATS,FORCE_PRESETDEFAULT,FORCE_PRESETDEFAULTX,FORCE_PRESETDEFAULTY,FORCE_BUTW,FORCE_BUTH,
		STR_MUL_DEFAULT,IMAGE_DEFAULTFORCE,IMAGE_DEFAULTFORCE,TRUE);	// default.

	addMultiBut(psWScreen,FORCE_STATS,FORCE_LOAD,FORCE_LOADX,FORCE_LOADY,FORCE_BUTW,FORCE_BUTH,
		STR_MUL_LOAD,IMAGE_LOADFORCE,IMAGE_LOADFORCE,TRUE);				// load

	addMultiBut(psWScreen,FORCE_STATS,FORCE_SAVE,FORCE_SAVEX,FORCE_SAVEY,FORCE_BUTW,FORCE_BUTH,
		STR_MUL_SAVE,IMAGE_SAVEFORCE,IMAGE_SAVEFORCE,TRUE);				// save

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
// whiteboard functions


// removewhiteboard
static BOOL removeWhiteBoard(void)
{
	bWhiteBoardUp = FALSE;
	widgReleaseScreen(psWhiteScreen);
	return TRUE;
}


// runwhiteboard
static BOOL runWhiteBoard(void)
{
	NETMSG m;
	static UDWORD lastSent=0;
	UDWORD	x,y,mx,my;

	if(!bWhiteBoardUp)
	{
		return TRUE;
	}
	mx = mouseX();
	my = mouseY();

	widgDisplayScreen(psWhiteScreen);

	// if mouse over && mouse down.
	if(	   mx	> WHITEX
		&& mx < (WHITEX+WHITEW)
		&& my > WHITEY
		&& my < (WHITEY+WHITEH)
		&& (mouseDown(MOUSE_LMB)||mouseReleased(MOUSE_LMB))
	  )
	{
		if(mouseDown(MOUSE_LMB))
		{
			x = mx-WHITEX;			//add a point
			y = my-WHITEY;

			whiteBoard[selectedPlayer][curWhite] = (UWORD) (((y-1)*WHITEW) + x);
		}

		if(mouseReleased(MOUSE_LMB))
		{
			whiteBoard[selectedPlayer][curWhite] = 0;	//add a non-point
		}

		curWhite++;
		if(curWhite == NUMWHITE)
		{
			curWhite = 0;
		}

		whiteBoard[selectedPlayer][curWhite] = 1;		//set wrap point
	}

	// possibly send our bit.

	if(ingame.localOptionsReceived || bHosted)
	if(lastSent>gameTime)
	{
		lastSent =0;
	}
	if((gameTime - lastSent) >2000)
	{
		lastSent = gameTime;

		m.type = NET_WHITEBOARD;
		m.body[0] = (UBYTE)selectedPlayer;
		memcpy( &(m.body[1]), &whiteBoard[selectedPlayer],(NUMWHITE*2));
		m.size = (2*NUMWHITE)+1;
		NETbcast(&m,FALSE);
	}

	return TRUE;
}

void displayWhiteBoard(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD	x = D_W+xOffset+psWidget->x;
	UDWORD	y = D_H+yOffset+psWidget->y;
//	UDWORD	w = psWidget->width;
//	UDWORD	h = psWidget->height;
	UDWORD	i;
	div_t	d;
	UBYTE	j,col;
	UWORD	oldPoint,newPoint,oldx,oldy,newx,newy;

	// white poly
//	pie_BoxFillIndex(x,y,x+w,y+h,COL_WHITE);

	// each line.
	for(i = 0;i<MAX_PLAYERS;i++)
	{
		if(isHumanPlayer(i))
		{
			switch(getPlayerColour(i))
			{
			case 0:
				col = COL_GREEN;
				break;
			case 1:
				col =COL_YELLOW;
				break;
			case 2:
				col =COL_GREY;
				break;
			case 3:
				col =COL_BLACK;
				break;
			case 4:
				col =COL_LIGHTRED;
				break;
			case 5:
				col =COL_LIGHTBLUE;
				break;
			case 6:
				col =COL_LIGHTMAGENTA;
				break;
			default:
			case 7:
				col =COL_LIGHTCYAN;
				break;
 			}

			oldPoint =whiteBoard[i][0];
			d = div(oldPoint,WHITEW);
			oldy = (UWORD)d.quot;
			oldx = (UWORD)d.rem;

			for(j=1;j<NUMWHITE;j++)
			{
				newPoint = whiteBoard[i][j];

				d = div(newPoint,WHITEW);
				newy = (UWORD)d.quot;
				newx = (UWORD)d.rem;
				if( newPoint>1 && oldPoint>1 )
				{
					iV_Line(x+oldx,y+oldy,x+newx,y+newy,col);	// draw line!
				}
				oldPoint = newPoint;
				oldx = newx;
				oldy = newy;
			}
		}
	}

	// overlay close widget.
	iV_DrawTransImage(FrontImages,IMAGE_NOPENCIL,MULTIOP_CHATBOXX-15+D_W,MULTIOP_CHATBOXY+D_H+MULTIOP_CHATBOXH-15);
}

// add whiteboard
static BOOL addWhiteBoard(void)
{
	W_FORMINIT		sFormInit;

	// clear whiteboard
	memset(&whiteBoard,0,sizeof(whiteBoard));
	widgCreateScreen(&psWhiteScreen);
	widgSetTipFont(psWhiteScreen,WFont);
	memset(&sFormInit, 0, sizeof(W_FORMINIT));		//Connection Settings
	sFormInit.formID = 0;
	sFormInit.id = 999;
	sFormInit.style = WFORM_PLAIN;
	sFormInit.x =(SWORD)( WHITEX-D_W);
	sFormInit.y =(SWORD)( WHITEY-D_H);
	sFormInit.width = WHITEW;
	sFormInit.height = WHITEH;
	sFormInit.pDisplay = displayWhiteBoard;
	widgAddForm(psWhiteScreen, &sFormInit);

	bWhiteBoardUp = TRUE;
	return TRUE;
}
/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
// Drawing functions

void displayChatEdit(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD x = xOffset+psWidget->x;
	UDWORD y = yOffset+psWidget->y -4;			// 4 is the magic number.
	iV_Line(x, y, x+psWidget->width , y, iV_PaletteNearestColour(100,100,160) );

	AddCursorSnap(&InterfaceSnap,
				(SWORD)(x+(psWidget->width/2)) ,
				(SWORD)(y+(psWidget->height/2)) ,psWidget->formID,psWidget->id,NULL);

	return;
}



// ////////////////////////////////////////////////////////////////////////////
void displayRemoteGame(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD x = xOffset+psWidget->x;
	UDWORD y = yOffset+psWidget->y;
	BOOL Hilight = FALSE;
	BOOL Down = FALSE;
	UDWORD	i;
	char	tmp[8];
	UDWORD png;

	i = (int)psWidget->pUserData;

	// collate info
	if( ((W_BUTTON*)psWidget)->state & (WBUTS_HILITE))
	{
		Hilight = TRUE;
	}
	if( ((W_BUTTON*)psWidget)->state & (WBUT_LOCK |WBUT_CLICKLOCK)) //LOCK WCLICK_DOWN | WCLICK_LOCKED | WCLICK_CLICKLOCK))
	{
		Down = TRUE;
	}

	// Draw blue boxes.
	drawBlueBox(x,y,psWidget->width,psWidget->height);
	drawBlueBox(x,y,94,psWidget->height);
	drawBlueBox(x,y,55,psWidget->height);

	//draw game info
	iV_SetFont(WFont);													// font
	iV_SetTextColour(-1);												//colour

	//draw type overlay.
//	if(NETgetGameFlagsUnjoined(i,1) == DMATCH)
//	{
//		iV_DrawTransImage(FrontImages,IMAGE_ARENA_OVER,x+59,y+3);
//	}
//	else
	if( NETgetGameFlagsUnjoined(i,1) == CAMPAIGN)
	{
		iV_DrawTransImage(FrontImages,IMAGE_CAMPAIGN_OVER,x+59,y+3);
	}
	else if( NETgetGameFlagsUnjoined(i,1) == TEAMPLAY)
	{
		iV_DrawTransImage(FrontImages,IMAGE_TEAM_OVER,x+62,y+3);
	}
	else
	{
		iV_DrawTransImage(FrontImages,IMAGE_SKIRMISH_OVER,x+62,y+3);	// SKIRMISH
	}

	// ping rating
	png = NETgetGameFlagsUnjoined(i,2);
	if(png >= PING_LO && png < PING_MED)
	{
		iV_DrawTransImage(FrontImages,IMAGE_LAMP_GREEN,x+70,y+26);
	}
	else if(png >= PING_MED && png < PING_HI)
	{
		iV_DrawTransImage(FrontImages,IMAGE_LAMP_AMBER,x+70,y+26);
	}else
	{
		iV_DrawTransImage(FrontImages,IMAGE_LAMP_RED,x+70,y+26);
	}

	//draw game name
	while(iV_GetTextWidth(NetPlay.games[i].name) > (psWidget->width-110) )
	{
		NetPlay.games[i].name[strlen(NetPlay.games[i].name)-1]='\0';
	}
	iV_DrawText(NetPlay.games[i].name, x + 100, y + 24);	// name

	// get game info.
	if(	   (NetPlay.games[i].desc.dwFlags & SESSION_JOINDISABLED)
		|| (NetPlay.games[i].desc.dwCurrentPlayers >= NetPlay.games[i].desc.dwMaxPlayers) 	// if not joinable

		||(   (NETgetGameFlagsUnjoined(gameNumber,1)== SKIRMISH)	// the LAST bug...
			&&(NetPlay.games[gameNumber].desc.dwCurrentPlayers >= NetPlay.games[gameNumber].desc.dwMaxPlayers -1)
		  )
	   )

	{
		// need some sort of closed thing here!
		iV_DrawTransImage(FrontImages,IMAGE_NOJOIN,x+18,y+11);
	}
	else
	{
		iV_DrawText(strresGetString(psStringRes, STR_MUL_PLAYERS), x + 5, y + 18);
		sprintf(tmp,"%d/%d",NetPlay.games[i].desc.dwCurrentPlayers,NetPlay.games[i].desc.dwMaxPlayers);
		iV_DrawText(tmp, x + 17, y + 33);
	}

	AddCursorSnap(&InterfaceSnap,
				(SWORD)(x+(psWidget->width/2)),
				(SWORD)(y+(psWidget->height/2)),psWidget->formID,psWidget->id,NULL);
}


// ////////////////////////////////////////////////////////////////////////////
static UDWORD bestPlayer(UDWORD player)
{

	UDWORD i, myscore,  score, count=1;

	myscore =  getMultiStats(player,FALSE).totalScore;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		if(isHumanPlayer(i) && i!=player )
		{
			score = getMultiStats(i, FALSE).totalScore;

			if(score >= myscore)
			{
				count++;
			}
		}
	}

	return count;
}

void displayTeamChooser(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD		x = xOffset+psWidget->x;
	UDWORD		y = yOffset+psWidget->y;
	BOOL		Hilight = FALSE;
	UDWORD		i;

	if( ((W_BUTTON*)psWidget)->state & (WBUTS_HILITE| WCLICK_DOWN | WCLICK_LOCKED | WCLICK_CLICKLOCK))
	{
		Hilight = TRUE;
	}
	i = (int)psWidget->pUserData;

	ASSERT(playerTeamGUI[i] >= 0 && playerTeamGUI[i] < MAX_PLAYERS,
		"displayTeamChooser: playerTeamGUI out of bounds" );

	//bluboxes.
	drawBlueBox(x,y,psWidget->width,psWidget->height);							// right

	iV_DrawTransImage(FrontImages,IMAGE_TEAM0 + playerTeamGUI[i],x+3,y+6);
}

// ////////////////////////////////////////////////////////////////////////////
void displayPlayer(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD		x = xOffset+psWidget->x;
	UDWORD		y = yOffset+psWidget->y;
	BOOL		Hilight = FALSE;
	UDWORD		j;
	UDWORD		i,eval;
	PLAYERSTATS stat;

	if( ((W_BUTTON*)psWidget)->state & (WBUTS_HILITE| WCLICK_DOWN | WCLICK_LOCKED | WCLICK_CLICKLOCK))
	{
		Hilight = TRUE;
	}
	i = (int)psWidget->pUserData;

	//bluboxes.
	drawBlueBox(x,y,psWidget->width,psWidget->height);							// right

	if(ingame.localOptionsReceived && NetPlay.players[i].dpid)					// only draw if real player!
	{
		//bluboxes.
		drawBlueBox(x,y,psWidget->width,psWidget->height);							// right
		drawBlueBox(x,y,60,psWidget->height);
		drawBlueBox(x,y,31,psWidget->height);										// left.

		for(j=0; player2dpid[j] != NetPlay.players[i].dpid && j<MAX_PLAYERS; j++);// get the in game playernumber.

		iV_SetFont(WFont);														// font
		iV_SetTextColour(-1);													// colour

		// name
		while(iV_GetTextWidth(NetPlay.players[i].name) > psWidget->width -68)	// clip name.
		{
			NetPlay.players[i].name[strlen(NetPlay.players[i].name)-1]='\0';
		}
		iV_DrawText(NetPlay.players[i].name, x + 65, y + 22);

		// ping rating
		if(ingame.PingTimes[j] >= PING_LO && ingame.PingTimes[j] < PING_MED)
		{
			iV_DrawTransImage(FrontImages,IMAGE_LAMP_GREEN,x,y);
		}else
		if(ingame.PingTimes[j] >= PING_MED && ingame.PingTimes[j] < PING_HI)
		{
			iV_DrawTransImage(FrontImages,IMAGE_LAMP_AMBER,x,y);
		}else
		{
			iV_DrawTransImage(FrontImages,IMAGE_LAMP_RED,x,y);
		}


		// player number
//		iV_DrawTransImage(FrontImages,IMAGE_WEE_GUY,x,y+23);
		switch(j)
		{
		case 0:
			iV_DrawTransImage(IntImages,IMAGE_GN_0,x+4,y+29);
			break;
		case 1:
			iV_DrawTransImage(IntImages,IMAGE_GN_1,x+5,y+29);
			break;
		case 2:
			iV_DrawTransImage(IntImages,IMAGE_GN_2,x+4,y+29);
			break;
		case 3:
			iV_DrawTransImage(IntImages,IMAGE_GN_3,x+4,y+29);
			break;
		case 4:
			iV_DrawTransImage(IntImages,IMAGE_GN_4,x+4,y+29);
			break;
		case 5:
			iV_DrawTransImage(IntImages,IMAGE_GN_5,x+4,y+29);
			break;
		case 6:
			iV_DrawTransImage(IntImages,IMAGE_GN_6,x+4,y+29);
			break;
		case 7:
			iV_DrawTransImage(IntImages,IMAGE_GN_7,x+4,y+29);
			break;
		default:
			break;
		}

		// ranking against other players.
		eval = bestPlayer(j);
		switch (eval)
		{
		case 1:
			iV_DrawTransImage(IntImages,IMAGE_GN_1,x+5,y+3);
			break;
		case 2:
			iV_DrawTransImage(IntImages,IMAGE_GN_2,x+4,y+3);
			break;
		case 3:
			iV_DrawTransImage(IntImages,IMAGE_GN_3,x+4,y+3);
			break;
		case 4:
			iV_DrawTransImage(IntImages,IMAGE_GN_4,x+4,y+3);
			break;
		case 5:
			iV_DrawTransImage(IntImages,IMAGE_GN_5,x+4,y+3);
			break;
		case 6:
			iV_DrawTransImage(IntImages,IMAGE_GN_6,x+4,y+3);
			break;
		case 7:
			iV_DrawTransImage(IntImages,IMAGE_GN_7,x+4,y+3);
			break;
		default:
			break;
		}

		if(getMultiStats(j,FALSE).played < 5)
		{
			iV_DrawTransImage(FrontImages,IMAGE_MEDAL_DUMMY,x+37,y+13);
		}
		else
		{
			stat = getMultiStats(j,FALSE);

			// star 1 total droid kills
			eval = stat.totalKills;
			if(eval >600)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK1,x+37,y+3);
			}
			else if(eval >300)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK2,x+37,y+3);
			}
			else if(eval >150)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK3,x+37,y+3);
			}


			// star 2 games played
			eval = stat.played;
			if(eval >200)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK1,x+37,y+13);
			}
			else if(eval >100)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK2,x+37,y+13);
			}
			else if(eval >50)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK3,x+37,y+13);
			}


			// star 3 games won.
			eval = stat.wins;
			if(eval >80)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK1,x+37,y+23);
			}
			else if(eval >40)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK2,x+37,y+23);
			}
			else if(eval >10)
			{
				iV_DrawTransImage(FrontImages,IMAGE_MULTIRANK3,x+37,y+23);
			}

			// medals.
			if((stat.loses>2)&&(stat.wins>2)&&(stat.wins>(2*stat.loses)) )// bronze requirement.
			{
				if(stat.wins>(4*stat.loses))								// silver requirement.
				{
					if(stat.wins>(8*stat.loses))							// gold requirement
					{
						iV_DrawTransImage(FrontImages,IMAGE_MEDAL_GOLD,x+49,y+11);
					}
					else
					{
						iV_DrawTransImage(FrontImages,IMAGE_MEDAL_SILVER,x+49,y+11);
					}
				}
				else
				{
					iV_DrawTransImage(FrontImages,IMAGE_MEDAL_BRONZE,x+49,y+11);
				}
			}
		}

		switch(getPlayerColour(j))		//flag icon
		{
		case 0:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER0,x+7,y+9);
			break;
		case 1:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER1,x+7,y+9);
			break;
		case 2:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER2,x+7,y+9);
			break;
		case 3:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER3,x+7,y+9);
			break;
		case 4:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER4,x+7,y+9);
			break;
		case 5:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER5,x+7,y+9);
			break;
		case 6:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER6,x+7,y+9);
			break;
		case 7:
			iV_DrawTransImage(FrontImages,IMAGE_PLAYER7,x+7,y+9);
			break;
		default:
			break;
		}
		//unknown bugfix
//		game.skirmishPlayers[i] =TRUE;	// don't clear this one!
		game.skDiff[i]=UBYTE_MAX;	// don't clear this one!
	}
	else
	{
		//bluboxes.
		drawBlueBox(x,y,psWidget->width,psWidget->height);							// right
		//drawBlueBox(x,y,31,psWidget->height);										// left.

		//if(game.type == SKIRMISH && game.skDiff[i])
		//{
		//	iV_DrawTransImage(FrontImages,IMAGE_PLAYER_PC,x+2,y+9);
		//}
	}
	AddCursorSnap(&InterfaceSnap,
				(SWORD)(x+(psWidget->width/2)),
				(SWORD)(y+(psWidget->height/2)),psWidget->formID,psWidget->id,NULL);

}



// ////////////////////////////////////////////////////////////////////////////
// Display blue box
void intDisplayFeBox(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD	x = xOffset+psWidget->x;
	UDWORD	y = yOffset+psWidget->y;
	UDWORD	w = psWidget->width;
	UDWORD	h = psWidget->height;

	drawBlueBox(x,y,w,h);

}

// ////////////////////////////////////////////////////////////////////////////
// Display edit box
void displayMultiEditBox(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD	x = xOffset+psWidget->x;
	UDWORD	y = yOffset+psWidget->y;
	UWORD	im = (UWORD)((UDWORD)psWidget->pUserData);

	drawBlueBox(x,y,psWidget->width,psWidget->height);
	drawBlueBox(x+psWidget->width,y,psWidget->height,psWidget->height);	// box on end.

	iV_DrawTransImage(FrontImages,im,x+psWidget->width+2,y+4);			//icon descriptor.

	if( ((W_EDITBOX*)psWidget)->state & WEDBS_DISABLE)					// disabled
	{
		pie_UniTransBoxFill(x,y, x+psWidget->width+psWidget->height ,y+psWidget->height,(FILLRED<<16) | (FILLGREEN<<8) | FILLBLUE, FILLTRANS);
	}

	AddCursorSnap(&InterfaceSnap,
					(SWORD)(x+(psWidget->width/2)),
					(SWORD)(y+(psWidget->height/2)),psWidget->formID,psWidget->id,NULL);
}

// ////////////////////////////////////////////////////////////////////////////
// Display one of two images depending on if the widget is hilighted by the mouse.
void displayMultiBut(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD	x = xOffset+psWidget->x;
	UDWORD	y = yOffset+psWidget->y;
	BOOL	Hilight = FALSE;
	UDWORD	Down = 0;
	UWORD	hiToUse = 0;
	UDWORD	Grey = 0;
	UWORD	im = (UWORD)UNPACKDWORD_TRI_B((UDWORD)psWidget->pUserData);
	UWORD	im2= (UWORD)(UNPACKDWORD_TRI_C((UDWORD)psWidget->pUserData));
	BOOL	usehl = ((UWORD)(UNPACKDWORD_TRI_A((UDWORD)psWidget->pUserData)));
//	BOOL	snap = 1;

	//evaluate
	if( (usehl==1) && ((W_BUTTON*)psWidget)->state & WBUTS_HILITE)
	{
		Hilight = TRUE;
		switch(iV_GetImageWidth(FrontImages,im))			//pick a hilight.
		{
		case 30:
			hiToUse = IMAGE_HI34;
			break;
		case 60:
			hiToUse = IMAGE_HI64;
			break;
		case 19:
			hiToUse = IMAGE_HI23;
			break;
		case 27:
			hiToUse = IMAGE_HI31;
			break;
		case 35:
			hiToUse = IMAGE_HI39;
			break;
		case 37:
			hiToUse = IMAGE_HI41;
			break;
		case 56:
			hiToUse = IMAGE_HI56;
			break;
		default:
			hiToUse = 0;
//			DBPRINTF(("no multibut highlight for width = %d",iV_GetImageWidth(FrontImages,im)));
			break;
		}

		if(im == IMAGE_RETURN)
		{
			hiToUse = IMAGE_RETURN_HI;
		}

	}

	if( ((W_BUTTON*)psWidget)->state & (WCLICK_DOWN | WCLICK_LOCKED | WCLICK_CLICKLOCK))
	{
		Down = 1;
	}

	if( ((W_BUTTON*)psWidget)->state & WBUTS_GREY)
	{
		Grey = 1;
	}


	// now display
	iV_DrawTransImage(FrontImages,im,x,y);

	// hilight with a number...just for player selector
	if(usehl >=10)
	{
		iV_DrawTransImage(IntImages,IMAGE_ASCII48-10+usehl,x+11,y+8);
	}

	// hilights etc..
	if(Hilight && !Grey)
	{
		if(Down)
		{
			iV_DrawTransImage(FrontImages,im2,x,y);
		}

		if(hiToUse)
		{
			iV_DrawTransImage(FrontImages,hiToUse,x,y);
		}

	}
	else if(Down)
	{
		iV_DrawTransImage(FrontImages,im2,x,y);
	}


	if (Grey) {
		// disabled, render something over it!
		iV_TransBoxFill(x,y,x+psWidget->width,y+psWidget->height);
	} else {
		// add a snap.
		AddCursorSnap(&InterfaceSnap,
						(SWORD)(x+(psWidget->width/2)),
						(SWORD)(y+(psWidget->height/2)),psWidget->formID,psWidget->id,NULL);
	}
}



/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
// common widgets

BOOL addMultiEditBox(UDWORD formid,UDWORD id,UDWORD x, UDWORD y,UDWORD tip, char tipres[128],UDWORD icon,UDWORD iconid)
{
	W_EDBINIT		sEdInit;

	memset(&sEdInit, 0, sizeof(W_EDBINIT));			// editbox
	sEdInit.formID = formid;
	sEdInit.id = id;
	sEdInit.style = WEDB_PLAIN;
	sEdInit.x = (short)x;
	sEdInit.y = (short)y;
	sEdInit.width = MULTIOP_EDITBOXW;
	sEdInit.height = MULTIOP_EDITBOXH;
	sEdInit.pText = tipres;
	sEdInit.FontID = WFont;
	sEdInit.pUserData = (void*)icon;
	sEdInit.pBoxDisplay = displayMultiEditBox;
	if (!widgAddEditBox(psWScreen, &sEdInit))
	{
		return FALSE;
	}

	// note drawing is done by the editbox draw tho...

	addMultiBut(psWScreen,MULTIOP_OPTIONS ,iconid,(x+MULTIOP_EDITBOXW+2),(y+2) ,
				 MULTIOP_EDITBOXH, MULTIOP_EDITBOXH, tip,
				 icon,icon,FALSE);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////////////////

BOOL addMultiBut(W_SCREEN *screen,UDWORD formid,UDWORD id,UDWORD x, UDWORD y,
				 UDWORD width, UDWORD height, UDWORD tipres,UDWORD norm,UDWORD hi,BOOL hiIt)
{
	W_BUTINIT		sButInit;

	memset(&sButInit, 0, sizeof(W_BUTINIT));
	sButInit.formID = formid;
	sButInit.id = id;
	sButInit.style = WFORM_PLAIN;
	sButInit.x = (short) x;
	sButInit.y = (short) y;
	sButInit.width = (unsigned short) width;
	sButInit.height= (unsigned short) height;
	if(tipres)
	{
		sButInit.pTip = strresGetString(psStringRes,tipres);
	}
	sButInit.FontID = WFont;
	sButInit.pDisplay = displayMultiBut;
/*
	if (hiIt == 1)
	{
		sButInit.pUserData = (void*)PACKDWORD_TRI(0,norm , hi);
	}
	else if (hiIt == 0)
	{
		sButInit.pUserData = (void*)PACKDWORD_TRI(1,norm , hi);
	}
	else
	{}
	*/
		sButInit.pUserData = (void*)PACKDWORD_TRI(hiIt,norm , hi);


	if (!widgAddButton(screen, &sButInit))
	{
		return FALSE;
	}
	return TRUE;
}


void displayForceDroid(WIDGET *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	iVector			Rotation,Position;
	UDWORD			x			= psWidget->x+xOffset;
	UDWORD			y			= psWidget->y+yOffset;
	UDWORD tlx,tly,brx,bry;

	tlx = x-58;
	tly = y-80;
	brx = x+58;
	bry = y+23;

	pie_BoxFill(tlx,	tly,	brx,	bry, 0x006067a0);
	pie_BoxFill(tlx+1,	tly+1,	brx-1,	bry-1,	0x002f3050);

	pie_Set2DClip((UWORD)(tlx+1),(UWORD)(tly+1),(UWORD)(brx-1),(UWORD)(bry-1));

	pie_SetGeometricOffset( x,y);
	Rotation.x = -20;
	Rotation.y = ((gameTime2/45)%360); //45
	Rotation.z = 0;
	Position.x = 0;
	Position.y = 0;			//above droid.
	Position.z = BUTTON_DEPTH;//2500;		//scale them to 200% in button render

	if(Force.pMembers)
	{
		displayComponentButtonTemplate(Force.pMembers->pTempl, &Rotation,&Position,TRUE, 2 * DROID_BUT_SCALE);
	}

	pie_Set2DClip(CLIP_BORDER,CLIP_BORDER,psRendSurface->width-CLIP_BORDER,psRendSurface->height-CLIP_BORDER);
	return;
}

/*
 * Set skirmish/multiplayer alliance mode to 'Locked teams',
 * update GUI accordingly and notify other players
 */
void setLockedTeamsMode(void)
{
	widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_N,0);
	widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_Y,0);
	widgSetButtonState(psWScreen, MULTIOP_ALLIANCE_TEAMS,WBUT_LOCK);
	game.alliance = ALLIANCES_TEAMS;		//2
	if(bHosted)
	{
		sendOptions(0,0);
	}
}
