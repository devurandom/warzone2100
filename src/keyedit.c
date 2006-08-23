/*
 * keyedit.c
 * keymap editor
 * alexl.
 */

// ////////////////////////////////////////////////////////////////////////////
// includes
#include <SDL/SDL.h>
#include <physfs.h>

#include "lib/framework/frame.h"
#include "lib/widget/widget.h"
#include "frontend.h"
#include "frend.h"
#include "text.h"
#include "lib/ivis_common/textdraw.h"
#include "hci.h"
#include "loadsave.h"
#include "keymap.h"
#include "csnap.h"
#include "intimage.h"
#include "lib/ivis_common/bitimage.h"
#include "intdisplay.h"
#include "audio_id.h"
#include "lib/ivis_common/pieblitfunc.h"
#include "multiint.h"

// ////////////////////////////////////////////////////////////////////////////
// defines

#define KM_FORM				10200
#define	KM_FORM_TABS		10201
#define KM_RETURN			10202
#define KM_DEFAULT			10203

#define	KM_START			10204
#define	KM_END				10399

#define KM_W				580
#define KM_H				440
#define KM_X				30
#define KM_Y				20

#define KM_RETURNX			(KM_W-90)
#define KM_RETURNY			(KM_H-42)

#define BUTTONSPERKEYMAPPAGE 20

#define KM_ENTRYW			480
#define KM_ENTRYH			(( (KM_H-50)/BUTTONSPERKEYMAPPAGE )-3 )


// ////////////////////////////////////////////////////////////////////////////
// variables
extern IMAGEFILE	*FrontImages;
extern CURSORSNAP	InterfaceSnap;

static KEY_MAPPING	*selectedKeyMap;
// ////////////////////////////////////////////////////////////////////////////
// protos

BOOL		runKeyMapEditor		(void);
static BOOL keyMapToString		(STRING *pStr, KEY_MAPPING *psMapping);
VOID		displayKeyMap		(struct _widget *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);
BOOL		startKeyMapEditor	(BOOL first);
BOOL		saveKeyMap		(void);
BOOL		loadKeyMap		(void);
static BOOL	pushedKeyMap		(UDWORD key);

char	keymapVersion[8] = "KM_0002";
extern char    KeyMapPath[];

// ////////////////////////////////////////////////////////////////////////////
// funcs

static BOOL pushedKeyMap(UDWORD key)
{
//	UDWORD count =0;
//	id-KM_START
//	for(selectedKeyMap = keyMappings;
//		selectedKeyMap->status != KEYMAP_ASSIGNABLE;
//		(selectedKeyMap->status= KEYMAP__DEBUG) && (selectedKeyMap->status==KEYMAP___HIDE);
//
//		selectedKeyMap = selectedKeyMap->psNext);
//
//	while(count!=key)
//	{
//		selectedKeyMap = selectedKeyMap->psNext;
//		if((selectedKeyMap->status!=KEYMAP__DEBUG)&&(selectedKeyMap->status!=KEYMAP___HIDE))		// if it's not a debug mapping..
//		if(selectedKeyMap->status == KEYMAP_ASSIGNABLE)
//		{
//			count++;
//		}
//	}
	selectedKeyMap = widgGetFromID(psWScreen,key)->pUserData;
	if(selectedKeyMap && selectedKeyMap->status != KEYMAP_ASSIGNABLE)
	{
		selectedKeyMap = NULL;
	    audio_PlayTrack( ID_SOUND_BUILD_FAIL );

	}

	return TRUE;
}


// ////////////////////////////////////////////////////////////////////////////
static BOOL pushedKeyCombo(UDWORD subkey)
{
	KEY_CODE	metakey=KEY_IGNORE;
	KEY_MAPPING	*pExist;
   	KEY_MAPPING	*psMapping;
	KEY_CODE	alt;
   //	void (*function)(void);
   //	KEY_ACTION	action;
   //	KEY_STATUS	status;
   //	STRING	name[255];

	// check for
	// alt
	alt = 0;
	if( keyDown(KEY_RALT) || keyDown(KEY_LALT) )
	{
		metakey= KEY_LALT;
		alt = KEY_RALT;
	}

	// ctrl
	else if( keyDown(KEY_RCTRL) || keyDown(KEY_LCTRL) )
	{
		metakey = KEY_LCTRL;
		alt = KEY_RCTRL;
	}

	// shift
	else if( keyDown(KEY_RSHIFT) || keyDown(KEY_LSHIFT) )
	{
		metakey = KEY_LSHIFT;
		alt = KEY_RSHIFT;
	}

	// check if bound to a fixed combo.
	pExist = keyFindMapping(  metakey,  subkey );
	if(pExist && (pExist->status == KEYMAP_ALWAYS OR pExist->status == KEYMAP_ALWAYS_PROCESS))
	{
		selectedKeyMap = NULL;	// unhighlght selected.
		return FALSE;
	}

	/* Clear down mappings using these keys... But only if it isn't unassigned */
	keyReAssignMapping( metakey, subkey, KEY_IGNORE, (KEY_CODE)KEY_MAXSCAN );

	/* Try and see if its there already - damn well should be! */
	psMapping = keyGetMappingFromFunction(selectedKeyMap->function);

	/* Cough if it's not there */
	ASSERT( psMapping!=NULL,"Trying to patch a non-existant function mapping - whoop whoop!!!" );

	/* Now alter it to the new values */
	psMapping->metaKeyCode = metakey;
	psMapping->subKeyCode = subkey;
	// was "=="
	psMapping->status = KEYMAP_ASSIGNABLE; //must be
	if(alt)
	{
		psMapping->altMetaKeyCode = alt;
	}



	/*



	// unbind old mapping with this combo.
//	function = selectedKeyMap->function;
//	action = selectedKeyMap->action;
//	status = selectedKeyMap->status;
//	strcpy(name,selectedKeyMap->pName);
//	keyRemoveMappingPt(selectedKeyMap);

	keyAddMapping(status,metakey,subkey,action,function,name);

	// add new binding.
//	keyReAssignMapping( selectedKeyMap->metaKeyCode, selectedKeyMap->subKeyCode, metakey, subkey);
  //	keyAddMapping(

	selectedKeyMap->metaKeyCode = metakey;
	selectedKeyMap->subKeyCode = subkey;

// readd the widgets.
//	widgDelete(psWScreen,FRONTEND_BACKDROP);
//	startKeyMapEditor(FALSE);

	*/
	selectedKeyMap = NULL;	// unhighlght selected .
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////////
static UDWORD scanKeyBoardForBinding()
{
	UDWORD i;
	for(i = 0; i <= KEY_MAXSCAN; i++)
	{
		if(keyPressed(i))
		{
			if(i !=	KEY_RALT			// exceptions
			&& i !=	KEY_LALT
			&& i != KEY_RCTRL
			&& i != KEY_LCTRL
			&& i != KEY_RSHIFT
			&& i != KEY_LSHIFT
			)
			{
				return(i);	// top row key pressed
			}
		}
	}
	return 0;
}

// ////////////////////////////////////////////////////////////////////////////
BOOL runKeyMapEditor(void)
{
	UDWORD id;

	id = widgRunScreen(psWScreen);						// Run the current set of widgets

	if(id == KM_RETURN)			// return
	{
		saveKeyMap();
		changeTitleMode(TITLE);
	}
	if(id == KM_DEFAULT)
	{
		keyClearMappings();
		keyInitMappings(TRUE);
		widgDelete(psWScreen,FRONTEND_BACKDROP);// readd the widgets
		startKeyMapEditor(FALSE);
	}
	else if( id>=KM_START && id<=KM_END)
	{
		 pushedKeyMap(id);
	}

	if(selectedKeyMap)
	{
		id = scanKeyBoardForBinding();
		if(id)
		{
			pushedKeyCombo(id);
		}
	}

	DrawBegin();
	StartCursorSnap(&InterfaceSnap);
	widgDisplayScreen(psWScreen);				// show the widgets currently running
	DrawEnd();

	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////////
// returns key to press given a mapping.
static BOOL keyMapToString(STRING *pStr, KEY_MAPPING *psMapping)
{
	BOOL	onlySub = TRUE;
	STRING	asciiSub[20],asciiMeta[20];

	if(psMapping->metaKeyCode!=KEY_IGNORE)
	{
		keyScanToString(psMapping->metaKeyCode,(STRING *)&asciiMeta,20);
		onlySub = FALSE;
	}
	keyScanToString(psMapping->subKeyCode,(STRING *)&asciiSub,20);

	if(onlySub)
	{
		sprintf(pStr,"%s",asciiSub);
	}
	else
	{
		sprintf(pStr,"%s - %s",asciiMeta,asciiSub);
	}
	return TRUE;
}

// ////////////////////////////////////////////////////////////////////////////
// display a keymap on the interface.
VOID displayKeyMap(struct _widget *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours)
{
	UDWORD		x = xOffset+psWidget->x;
	UDWORD		y = yOffset+psWidget->y;
	UDWORD		w = psWidget->width;
	UDWORD		h = psWidget->height;
	KEY_MAPPING *psMapping = (KEY_MAPPING*)psWidget->pUserData;
	STRING		sKey[MAX_NAME_SIZE];// was just 40

	if(psMapping == selectedKeyMap)
	{
		pie_BoxFillIndex(x,y,x+w,y+h,COL_GREEN);
	}
	else if(psMapping->status == KEYMAP_ALWAYS OR psMapping->status == KEYMAP_ALWAYS_PROCESS)
	{
		pie_BoxFillIndex(x,y,x+w,y+h,COL_RED);
	}
	else
	{
		drawBlueBox(x,y,w,h);
	}

	// draw name
	iV_SetFont(WFont);											// font
	iV_SetTextColour(-1);										//colour

	pie_DrawText(psMapping->pName, x + 2, y + (psWidget->height / 2) + 3);

	// draw binding
	keyMapToString(sKey,psMapping);
	pie_DrawText(sKey, x + 370, y + (psWidget->height / 2) + 3);

	return;
}

// ////////////////////////////////////////////////////////////////////////////
BOOL startKeyMapEditor(BOOL first)
{
	W_BUTINIT	sButInit;
	W_FORMINIT	sFormInit;
	KEY_MAPPING	*psMapping;
	UDWORD		i,mapcount =0;
	UDWORD		bubbleCount;
	BOOL		bAtEnd,bGotOne;
	KEY_MAPPING	*psPresent = NULL, *psNext;
	char		test[255];
	addBackdrop();
	addSideText	(FRONTEND_SIDETEXT ,KM_X-2,KM_Y,strresGetString(psStringRes, STR_KM_KEYMAP_SIDE));

	if (first)
	{
		loadKeyMap();									// get the current mappings.
	}
	memset(&sFormInit, 0, sizeof(W_FORMINIT));			// draw blue form...
	sFormInit.formID	= FRONTEND_BACKDROP;
	sFormInit.id		= KM_FORM;
	sFormInit.style		= WFORM_PLAIN;
	sFormInit.x			= KM_X;
	sFormInit.y			= KM_Y;
	sFormInit.width		= KM_W;
	sFormInit.height	= KM_H;
	sFormInit.pDisplay	= intDisplayPlainForm;
	widgAddForm(psWScreen, &sFormInit);


	addMultiBut(psWScreen,KM_FORM,KM_RETURN,			// return button.
					8,5,
					iV_GetImageWidth(FrontImages,IMAGE_RETURN),
					iV_GetImageHeight(FrontImages,IMAGE_RETURN),
					STR_MUL_CANCEL,IMAGE_RETURN,IMAGE_RETURN_HI,TRUE);

	addMultiBut(psWScreen,KM_FORM,KM_DEFAULT,
				11,45,
				56,38,
				STR_MUL_DEFAULT,
				IMAGE_KEYMAP_DEFAULT,IMAGE_KEYMAP_DEFAULT,TRUE);	// default.


	/* Better be none that come after this...! */
	strcpy(test,"zzzzzzzzzzzzzzzzzzzzz");
	psMapping = NULL;

	//count mappings required.
	for(psMapping = keyMappings; psMapping; psMapping = psMapping->psNext)
	{
		if( (psMapping->status!=KEYMAP__DEBUG)&&(psMapping->status!=KEYMAP___HIDE))		// if it's not a debug mapping..
		{
			mapcount++;
			if(strcmp(psMapping->pName,test) < 0)
			{
				/* Best one found so far */
				strcpy(test,psMapping->pName);
				psPresent = psMapping;
			}
		}
	}

	// add tab form..
	memset(&sFormInit, 0, sizeof(W_FORMINIT));
	sFormInit.formID		= KM_FORM;
	sFormInit.id			= KM_FORM_TABS;
	sFormInit.style			= WFORM_TABBED;
	sFormInit.x			= 50;
	sFormInit.y			= 10;
	sFormInit.width			= KM_W- 100;
	sFormInit.height		= KM_H- 4;
	sFormInit.numMajor		= numForms(mapcount, BUTTONSPERKEYMAPPAGE);
	sFormInit.majorPos		= WFORM_TABTOP;
	sFormInit.minorPos		= WFORM_TABNONE;
	sFormInit.majorSize		= OBJ_TABWIDTH+3;
	sFormInit.majorOffset		= OBJ_TABOFFSET;
	sFormInit.tabVertOffset		= (OBJ_TABHEIGHT/2);
	sFormInit.tabMajorThickness 	= OBJ_TABHEIGHT;
	sFormInit.pFormDisplay		= intDisplayObjectForm;
	sFormInit.pUserData		= (void*)&StandardTab;
	sFormInit.pTabDisplay		= intDisplayTab;
	for (i=0; i< sFormInit.numMajor; i++)
	{
		sFormInit.aNumMinors[i] = 1;
	}
	widgAddForm(psWScreen, &sFormInit);

	//Put the buttons on it
	memset(&sButInit, 0, sizeof(W_BUTINIT));
	sButInit.formID   = KM_FORM_TABS;
	sButInit.style	  = WFORM_PLAIN;
	sButInit.width    = KM_ENTRYW;
	sButInit.height	  = KM_ENTRYH;
	sButInit.pDisplay = displayKeyMap;
	sButInit.x	  = 2;
	sButInit.y	  = 16;
	sButInit.id	  = KM_START;


	/* Add our first mapping to the form */
	sButInit.pUserData= (VOID*)psPresent;
	widgAddButton(psWScreen, &sButInit);
	sButInit.id++;
	sButInit.y +=  KM_ENTRYH +3;

	/* Now add the others... */
	bubbleCount = 0;
	bAtEnd = FALSE;
	/* Stop when the right number or when aphabetically last - not sure...! */
	while(bubbleCount<mapcount-1 AND !bAtEnd)
	{
		/* Same test as before for upper limit */
	 	strcpy(test,"zzzzzzzzzzzzzzzzzzzzz");
		for(psMapping = keyMappings,psNext = NULL,bGotOne = FALSE; psMapping; psMapping = psMapping->psNext)
		{
			/* Only certain mappings get displayed */
			if( (psMapping->status!=KEYMAP__DEBUG)&&(psMapping->status!=KEYMAP___HIDE))		// if it's not a debug mapping..
			{
				/* If it's alphabetically good but better then next one */
				if(strcmp(psMapping->pName,test) < 0 AND strcmp(psMapping->pName,psPresent->pName) > 0)
				{
					/* Keep a record of it */
					strcpy(test,psMapping->pName);
				   	psNext = psMapping;
					bGotOne = TRUE;
				}
			}
		}
		/* We found one matching criteria */
		if(bGotOne)
		{
			psPresent = psNext;
			bubbleCount++;
	 		sButInit.pUserData= (VOID*)psNext;
	 		widgAddButton(psWScreen, &sButInit);
			 					// move on..
	 		sButInit.id++;
		  	/* Might be no more room on the page */
			if (  (sButInit.y + KM_ENTRYH+5 ) > (3+ (BUTTONSPERKEYMAPPAGE*(KM_ENTRYH+3))))
			{
				sButInit.y = 16;
				sButInit.majorID += 1;
			}
			else
			{
				sButInit.y +=  KM_ENTRYH +3;
			}
		}
		else
		{
			/* The previous one we found was alphabetically last - time to stop */
			bAtEnd = TRUE;
		}
	}

	/* Go home... */
	return TRUE;
}



// ////////////////////////////////////////////////////////////////////////////
// save current keymaps to registry
// FIXME: Use the endian-safe physfs functions.
BOOL saveKeyMap(void)
{
	KEY_MAPPING	*psMapping;
	SDWORD		count;
	STRING		name[128];
	PHYSFS_file *pfile;

  // KeyMapPath
	debug(LOG_WZ, "We are to write %s for keymap info", KeyMapPath);
	pfile = PHYSFS_openWrite(KeyMapPath);
	if (!pfile) {
		debug(LOG_ERROR, "saveKeyMap: %s could not be created: %s", KeyMapPath,
		      PHYSFS_getLastError());
		assert(FALSE);
		return FALSE;
	}

#define WRITE(var, size)                                               \
	if (PHYSFS_write(pfile, var, 1, size) != size) {                     \
		debug(LOG_ERROR, "saveKeyMap: could not write to %s %d bytes: %s", \
		      KeyMapPath, size, PHYSFS_getLastError());                    \
		assert(FALSE);                                                     \
		return FALSE;                                                      \
	}

	// write out number of entries.
	count = 0;
	for (psMapping = keyMappings; psMapping; psMapping = psMapping->psNext) {
		count++;
	}
	WRITE(&count, sizeof(count));
	WRITE(&keymapVersion, 8);

	for(psMapping = keyMappings; psMapping; psMapping = psMapping->psNext)
	{
		// save this map.
		// name
		strcpy(name,psMapping->pName);
		WRITE(&name, 128);

		WRITE(&psMapping->status, sizeof(KEY_STATUS));	// status
		WRITE(&psMapping->metaKeyCode, sizeof(KEY_CODE));	// metakey
		WRITE(&psMapping->subKeyCode, sizeof(KEY_CODE)); // subkey
		WRITE(&psMapping->action, sizeof(KEY_ACTION)); // action

		// function to map to!
		for(count = 0;  keyMapSaveTable[count] != NULL
					 && keyMapSaveTable[count] != psMapping->function;
			count++);
		if(keyMapSaveTable[count] == NULL)
		{
			debug( LOG_ERROR, "can't find keymapped function in the keymap save table!!" );
			abort();
		}
		WRITE(&count, sizeof(count));
	}
	if (!PHYSFS_close(pfile)) {
		debug(LOG_ERROR, "saveKeyMap: Error closing %s: %s", KeyMapPath,
		      PHYSFS_getLastError());
		assert(FALSE);
		return FALSE;
	}
	debug(LOG_WZ, "Keymap written ok to %s.", KeyMapPath);
	return TRUE;	// saved ok.
#undef WRITE
}

// ////////////////////////////////////////////////////////////////////////////
// load keymaps from registry.
BOOL loadKeyMap(void)
{
	KEY_STATUS	status;
	KEY_CODE	metaCode;
	KEY_CODE	subCode;
	KEY_ACTION	action;
	STRING		name[128];
	SDWORD		count;
	UDWORD		funcmap;
	char		ver[8];
	PHYSFS_file *pfile;
	PHYSFS_sint64 filesize;
	PHYSFS_sint64 countsize = 0;
	PHYSFS_sint64 length_read;

	// throw away any keymaps!!
	keyClearMappings();

	if (!PHYSFS_exists(KeyMapPath)) {
		debug(LOG_WZ, "loadKeyMap: %s not found", KeyMapPath);
		return FALSE;
	}
	pfile = PHYSFS_openRead(KeyMapPath);
	if (!pfile) {
		debug(LOG_ERROR, "loadKeyMap: %s could not be opened: %s", KeyMapPath,
		      PHYSFS_getLastError());
		assert(FALSE);
		return FALSE;
	}
	filesize = PHYSFS_fileLength(pfile);

#define READ(var, size)                                       \
	length_read = PHYSFS_read(pfile, var, 1, size);             \
	countsize += length_read;                                   \
	if (length_read != size) {                                  \
		debug(LOG_ERROR, "loadKeyMap: Reading %s short: %s",      \
		      KeyMapPath, PHYSFS_getLastError());                 \
		assert(FALSE);                                            \
		(void) PHYSFS_close(pfile);                               \
		return FALSE;                                             \
	}

	READ(&count, sizeof(count));
	READ(&ver, 8);	// get version number.

	if (strncmp(ver, keymapVersion, 8) != 0) {
		/* If wrong version, create a new one instead. */
		PHYSFS_close(pfile);
		return FALSE;
	}

	for(; count > 0; count--) {
		READ(&name, 128);	// name
		READ(&status, sizeof(KEY_STATUS));	// status
		READ(&metaCode, sizeof(KEY_CODE));	// metakey
		READ(&subCode, sizeof(KEY_CODE));	// subkey
		READ(&action, sizeof(KEY_ACTION));	// action
		READ(&funcmap, sizeof(funcmap));	// function

		// add mapping
		keyAddMapping( status, metaCode, subCode, action, keyMapSaveTable[funcmap],(char*)&name);
	}

	if (!PHYSFS_close(pfile)) {
		debug(LOG_ERROR, "loadKeyMap: Error closing %s: %s", KeyMapPath,
		      PHYSFS_getLastError());
		assert(FALSE);
		return FALSE;
	}
	if (countsize != filesize) {
		debug(LOG_ERROR, "loadKeyMap: File size different from bytes read!");
		assert(FALSE);
	}
	return TRUE;
}
