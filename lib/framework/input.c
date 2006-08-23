/*
 * Input.c
 *
 * Processes all keyboard and mouse input.
 *
 */

#include <stdio.h>
#include <SDL/SDL.h>

/* Allow frame header files to be singly included */
#define FRAME_LIB_INCLUDE

/* The input buffer printf's */
//#define DEBUG_GROUP1
#include "types.h"
#include "debug.h"
#include "input.h"
#include "fractions.h"
#include "frame.h"
#include "frameint.h"
#include "configfile.h"

/* The possible states for keys */
typedef enum _key_state
{
	KEY_UP,
	KEY_PRESSED,
	KEY_DOWN,
	KEY_RELEASED,
	KEY_PRESSRELEASE,	// When a key goes up and down in a frame
	KEY_DOUBLECLICK,	// Only used by mouse keys
	KEY_DRAG,			// Only used by mouse keys
} KEY_STATE;


/* The current state of the keyboard */
static KEY_STATE aKeyState[KEY_MAXSCAN];

/* The current location of the mouse */
static SDWORD		mouseXPos, mouseYPos;

/* How far the mouse has to move to start a drag */
#define DRAG_THRESHOLD	5

/* Which button is being used for a drag */
static MOUSE_KEY_CODE	dragKey;

/* The start of a possible drag by the mouse */
static SDWORD			dragX, dragY;

/* The current mouse button state */
static KEY_STATE aMouseState[6];


/* The size of the input buffer */
#define INPUT_MAXSTR	512

/* The input string buffer */
static UDWORD	pInputBuffer[INPUT_MAXSTR];
static UDWORD	*pStartBuffer, *pEndBuffer;

static char	pCharInputBuffer[INPUT_MAXSTR];
static char	*pCharStartBuffer, *pCharEndBuffer;
static char	currentChar;


static KEY_CODE sdlKeyToKeyCode(SDLKey key)
{
	return (KEY_CODE)key;
}

static SDLKey keyCodeToSDLKey(KEY_CODE code)
{
	return (SDLKey)code;
}


void keyScanToString(KEY_CODE code, STRING *ascii, UDWORD maxStringSize)
{
	if(keyCodeToSDLKey(code) == KEY_MAXSCAN)
	{
		strcpy(ascii,"???");
		return;
	}
	ASSERT( (code >= 0) && (code <= KEY_MAXSCAN), "Invalid key code: %d", code );
#ifndef _MSC_VER
	snprintf(ascii, maxStringSize, "%s", SDL_GetKeyName(keyCodeToSDLKey(code)));
#else
	_snprintf(ascii, maxStringSize, "%s", SDL_GetKeyName(keyCodeToSDLKey(code)));
//	sprintf(ascii,"%s",SDL_GetKeyName(keyCodeToSDLKey(code)));	//temp
#endif //use _snprintf() in _MSC_VER	  --Q
}


/* Initialise the input module */
void inputInitialise(void)
{
	UDWORD	i;

	for(i=0; i<KEY_MAXSCAN; i++)
	{
		aKeyState[i] = KEY_UP;
	}

	for (i = 0; i < 6; i++)
	{
		aMouseState[i] = KEY_UP;
	}

	pStartBuffer = pInputBuffer;
	pEndBuffer = pInputBuffer;
	pCharStartBuffer = pCharInputBuffer;
	pCharEndBuffer = pCharInputBuffer;

	dragX = mouseXPos = screenWidth/2;
	dragY = mouseYPos = screenHeight/2;
	dragKey = MOUSE_LMB;

	SDL_EnableUNICODE(1);
}

/* add count copies of the characater code to the input buffer */
void inputAddBuffer(UDWORD code, char char_code, UDWORD count)
{
	UDWORD	*pNext;
	char	*pCharNext;

	/* Calculate what pEndBuffer will be set to next */
	pNext = pEndBuffer + 1;
	pCharNext = pCharEndBuffer + 1;
	if (pNext >= pInputBuffer + INPUT_MAXSTR)
	{
		pNext = pInputBuffer;
		pCharNext = pCharInputBuffer;
	}

	while (pNext != pStartBuffer && count > 0)
	{
		/* Store the character */
		*pEndBuffer = code;
		*pCharEndBuffer = char_code;
		pEndBuffer = pNext;
		pCharEndBuffer = pCharNext;
		count -= 1;

		/* Calculate what pEndBuffer will be set to next */
		pNext = pEndBuffer + 1;
		pCharNext = pCharEndBuffer + 1;
		if (pNext >= pInputBuffer + INPUT_MAXSTR)
		{
			pNext = pInputBuffer;
			pCharNext = pCharInputBuffer;
		}
	}
}


/* Clear the input buffer */
void inputClearBuffer(void)
{
	pStartBuffer = pInputBuffer;
	pEndBuffer = pInputBuffer;
	pCharStartBuffer = pCharInputBuffer;
	pCharEndBuffer = pCharInputBuffer;
}


/* Return the next key press or 0 if no key in the buffer.
 * The key returned will have been remaped to the correct ascii code for the
 * windows key map.
 * All key presses are buffered up (including windows auto repeat).
 */
UDWORD inputGetKey(void)
{
	UDWORD	retVal;

	if (pStartBuffer != pEndBuffer)
	{
		retVal = *pStartBuffer;
		currentChar = *pCharStartBuffer;
		pStartBuffer += 1;
		pCharStartBuffer += 1;

		if (pStartBuffer >= pInputBuffer + INPUT_MAXSTR)
		{
			pStartBuffer = pInputBuffer;
			pCharStartBuffer = pCharInputBuffer;
		}
	}
	else
	{
		retVal = 0;
	}

	return retVal;
}

char inputGetCharKey(void) {
	return currentChar;
}



/* Deal with windows messages to maintain the state of the keyboard and mouse */
void inputProcessEvent(SDL_Event *event)
{
	UDWORD	code,i, vk;
//	FRACT	divX,divY;
//	UDWORD	scrX,scrY;

	switch(event->type)
	{
		case SDL_KEYDOWN:
			//printf("keydown %s (%i)\n", SDL_GetKeyName(code), event->key.keysym.sym);
			switch (event->key.keysym.sym)
			{
				case SDLK_LEFT:
					vk = INPBUF_LEFT;
					break;
				case SDLK_RIGHT:
					vk = INPBUF_RIGHT;
					break;
				case SDLK_UP:
					vk = INPBUF_UP;
					break;
				case SDLK_DOWN:
					vk = INPBUF_DOWN;
					break;
				case SDLK_HOME:
					vk = INPBUF_HOME;
					break;
				case SDLK_END:
					vk = INPBUF_END;
					break;
				case SDLK_INSERT:
					vk = INPBUF_INS;
					break;
				case SDLK_DELETE:
					vk = INPBUF_DEL;
					break;
				case SDLK_PAGEUP:
					vk = INPBUF_PGUP;
					break;
				case SDLK_PAGEDOWN:
					vk = INPBUF_PGDN;
					break;
				default:
					vk = event->key.keysym.sym;
					break;
			}

			{

				unsigned char char_code = event->key.keysym.unicode;

//				DBP1(("Code: %x\n", vk));		//This breaks with .NET, [DBP1(("Code: %x\n", vk));]it don't want the ; at end. --Qamly



				if (char_code < 32) {
					char_code = 0;
				}
				inputAddBuffer(vk, char_code, 1);

			}

			code = sdlKeyToKeyCode(event->key.keysym.sym);
			if ((aKeyState[code] == KEY_UP) ||
				(aKeyState[code] == KEY_RELEASED) ||
				(aKeyState[code] == KEY_PRESSRELEASE))
			{
				aKeyState[code] = KEY_PRESSED;
			}
			break;
		case SDL_KEYUP:
			code = sdlKeyToKeyCode(event->key.keysym.sym);
			if (aKeyState[code] == KEY_PRESSED)
			{
				aKeyState[code] = KEY_PRESSRELEASE;
			}
			else if (aKeyState[code] == KEY_DOWN)
			{
				aKeyState[code] = KEY_RELEASED;
			}
			break;
		/* Deal with mouse messages */
		case SDL_MOUSEMOTION:
			if(!mouseDown(MOUSE_MMB))
			{
				/* store the current mouse position */
				mouseXPos = event->motion.x;
				mouseYPos = event->motion.y;
				/*
				if(mouseXPos>=screenWidth)
				{
					mouseXPos = screenWidth-1;
				}
				if(mouseYPos >= screenHeight)
				{
					mouseYPos = screenHeight-1;
				}
				*/

				/* now see if a drag has started */
				if ((aMouseState[dragKey] == KEY_PRESSED ||
					 aMouseState[dragKey] == KEY_DOWN) &&
					(ABSDIF(dragX,mouseXPos) > DRAG_THRESHOLD ||
					 ABSDIF(dragY,mouseYPos) > DRAG_THRESHOLD))
				{
		//		DBPRINTF(("dragging\n"));
					aMouseState[dragKey] = KEY_DRAG;
				}
			}
			break;
		case SDL_MOUSEBUTTONUP:
			if (aMouseState[event->button.button] == KEY_PRESSED)
				{
				aMouseState[event->button.button] = KEY_PRESSRELEASE;
				}
			else if (aMouseState[event->button.button] == KEY_DOWN
					|| aMouseState[event->button.button] == KEY_DRAG)
				{
				aMouseState[event->button.button] = KEY_RELEASED;
			}
			break;
		case SDL_MOUSEBUTTONDOWN:
			if (aMouseState[event->button.button] == KEY_UP
					|| aMouseState[event->button.button] == KEY_RELEASED
					|| aMouseState[event->button.button] == KEY_PRESSRELEASE)
			{
				aMouseState[event->button.button] = KEY_PRESSED;
				if (event->button.button < 4)
				{
					dragKey = (MOUSE_KEY_CODE)event->button.button;
					dragX = mouseXPos;
					dragY = mouseYPos;
				}
			}
			// TODO: double click
			break;
		case SDL_ACTIVEEVENT:
			/* Lost the window focus, have to take this as a global key up */
			for(i=0; i<KEY_MAXSCAN; i++)
			{
				if ((aKeyState[i] == KEY_PRESSED) ||
					(aKeyState[i] == KEY_DOWN))
				{
					aKeyState[i] = KEY_RELEASED;
				}
			}
			for (i = 0; i < 6; i++)
			{
				if ((aMouseState[i] == KEY_PRESSED) ||
					(aMouseState[i] == KEY_DOWN) ||
					(aMouseState[i] == KEY_DRAG))
				{
					aMouseState[i] = KEY_RELEASED;
				}
			}
			break;
	}
}

/* This is called once a frame so that the system can tell
 * whether a key was pressed this turn or held down from the last frame.
 */
void inputNewFrame(void)
{
	UDWORD i;


	/* Do the keyboard */
	for (i=0; i< KEY_MAXSCAN; i++)
	{
		if (aKeyState[i] == KEY_PRESSED)
		{
			aKeyState[i] = KEY_DOWN;
		}
		else if ((aKeyState[i] == KEY_RELEASED) ||
				 (aKeyState[i] == KEY_PRESSRELEASE))
		{
			aKeyState[i] = KEY_UP;
		}
	}


	/* Do the mouse */
	for (i = 0; i < 6; i++) {
		if (aMouseState[i] == KEY_PRESSED)
			aMouseState[i] = KEY_DOWN;
		else if ((aMouseState[i] == KEY_RELEASED)
				|| (aMouseState[i] == KEY_DOUBLECLICK)
				|| (aMouseState[i] == KEY_PRESSRELEASE))
			aMouseState[i] = KEY_UP;
		}
}

/* This returns true if the key is currently depressed */
BOOL keyDown(KEY_CODE code)
{
	ASSERT( (code >= 0) && (code < KEY_MAXSCAN), "Invalid key code: %d", code );
	return (aKeyState[code] != KEY_UP);
}

/* This returns true if the key went from being up to being down this frame */
BOOL keyPressed(KEY_CODE code)
{
	ASSERT( (code >= 0) && (code < KEY_MAXSCAN), "Invalid key code: %d", code );
	return ((aKeyState[code] == KEY_PRESSED) || (aKeyState[code] == KEY_PRESSRELEASE));
}

/* This returns true if the key went from being down to being up this frame */
BOOL keyReleased(KEY_CODE code)
{
	ASSERT( (code >= 0) && (code < KEY_MAXSCAN), "Invalid key code: %d", code );
	return ((aKeyState[code] == KEY_RELEASED) || (aKeyState[code] == KEY_PRESSRELEASE));
}

/* Return the X coordinate of the mouse */
SDWORD mouseX(void)
{
	return mouseXPos;
}

/* Return the Y coordinate of the mouse */
SDWORD mouseY(void)
{
	return mouseYPos;
}

/* This returns true if the mouse key is currently depressed */
BOOL mouseDown(MOUSE_KEY_CODE code)
{
	ASSERT( (code >= 0), "Invalid mouse key code: %d", code );
	return (aMouseState[code] != KEY_UP);
}

/* This returns true if the mouse key was double clicked */
BOOL mouseDClicked(MOUSE_KEY_CODE code)
{
	ASSERT( code >= 0, "Invalid mouse key code: %d", code );
	return (aMouseState[code] == KEY_DOUBLECLICK);
}

/* This returns true if the mouse key went from being up to being down this frame */
BOOL mousePressed(MOUSE_KEY_CODE code)
{
	ASSERT( (code >= 0), "Invalid mouse key code: %d", code );
	return ((aMouseState[code] == KEY_PRESSED) ||
			(aMouseState[code] == KEY_PRESSRELEASE));
}

/* This returns true if the mouse key went from being down to being up this frame */
BOOL mouseReleased(MOUSE_KEY_CODE code)
{
	ASSERT( (code >= 0), "Invalid mouse key code: %d", code );
	return ((aMouseState[code] == KEY_RELEASED) ||
			(aMouseState[code] == KEY_DOUBLECLICK) ||
			(aMouseState[code] == KEY_PRESSRELEASE));
}

/* Check for a mouse drag, return the drag start coords if dragging */
BOOL mouseDrag(MOUSE_KEY_CODE code, UDWORD *px, UDWORD *py)
{
	ASSERT( (code >= 0), "Invalid mouse key code: %d", code );
	if (aMouseState[code] == KEY_DRAG)
	{
		*px = dragX;
		*py = dragY;
		return TRUE;
	}

	return FALSE;
}

void SetMousePos(UDWORD nowt,UDWORD x,UDWORD y)
{
	static int mousewarp = -1;

	if (mousewarp == -1) {
		int val;

		mousewarp = 1;
		if (getWarzoneKeyNumeric("nomousewarp", &val)) {
			if (val) {
				mousewarp = 0;
			}
		}
	}
	if (mousewarp) SDL_WarpMouse(x, y);
}

/* Sets the state of the mouse key to down */
void setMouseDown(MOUSE_KEY_CODE code)
{
	SDL_Event event;

	event.type = SDL_MOUSEBUTTONDOWN;
	event.button.type = SDL_MOUSEBUTTONDOWN;
	event.button.button = code;
	event.button.state = SDL_PRESSED;
	event.button.x = mouseX();
	event.button.y = mouseY();
	SDL_PushEvent(&event);
}

/* Sets the state of the mouse key to up */
void setMouseUp(MOUSE_KEY_CODE code)
{
	SDL_Event event;

	event.type = SDL_MOUSEBUTTONUP;
	event.button.type = SDL_MOUSEBUTTONUP;
	event.button.button = code;
	event.button.state = SDL_RELEASED;
	event.button.x = mouseX();
	event.button.y = mouseY();
	SDL_PushEvent(&event);
}



