/*
 * MultiMenu.h
 *
 * Definition for in game,multiplayer, interface.
 */
//
#ifndef __INCLUDED_MULTIMENU__
#define __INCLUDED_MULTIMENU__

// requester
extern void		addMultiRequest(STRING *ToFind, UDWORD id,UBYTE mapCam, UBYTE numPlayers);
extern BOOL		multiRequestUp;
extern W_SCREEN *psRScreen;			// requester stuff.
extern BOOL		runMultiRequester(UDWORD id,UDWORD *contextmode, STRING *chosen,UDWORD *chosenValue);
extern void		displayRequestOption(struct _widget *psWidget, UDWORD xOffset, UDWORD yOffset, UDWORD *pColours);

// multimenu
extern void		intProcessMultiMenu		(UDWORD id);
extern BOOL		intRunMultiMenu			(void);
extern BOOL		intCloseMultiMenu		(void);
extern void		intCloseMultiMenuNoAnim	(void);
extern BOOL		intAddMultiMenu			(void);

extern BOOL		addDebugMenu			(BOOL bAdd);
extern void		intCloseDebugMenuNoAnim	(void);

extern BOOL		MultiMenuUp;
extern BOOL		ClosingMultiMenu;

extern BOOL		DebugMenuUp;

//extern void		intDisplayMiniMultiMenu		(void);

#define MULTIMENU			10600
#define MULTIMENU_FORM		MULTIMENU

#define	DEBUGMENU				106000
#define	DEBUGMENU_CLOSE	(DEBUGMENU+1)
#define	DEBUGMENU_MAX_ENTRIES	10
#define	DEBUGMENU_BUTTON				(DEBUGMENU_CLOSE + DEBUGMENU_MAX_ENTRIES)

extern char		debugMenuEntry[DEBUGMENU_MAX_ENTRIES][MAX_STR_LENGTH];

#endif
