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
 * Event.h
 *
 * Interface to the event management system.
 *
 */
#ifndef _event_h
#define _event_h

#include "interpreter.h"

/* The number of values in a context value chunk */
#define CONTEXT_VALS 20

/* One chunk of variables for a script context */
typedef struct _val_chunk
{
	INTERP_VAL		asVals[CONTEXT_VALS];

	struct _val_chunk *psNext;
} VAL_CHUNK;


/* The number of links in a context event link chunk */
#define CONTEXT_LINKS	10
/* One chunk of event links for a script context */
typedef struct _link_chunk
{
	SWORD			aLinks[CONTEXT_LINKS];

	struct _link_chunk *psNext;
} LINK_CHUNK;

// Whether a context is released when there are no active triggers for it
typedef enum _context_release
{
	CR_RELEASE,		// release the context
	CR_NORELEASE,	// do not release the context
} CONTEXT_RELEASE;


/* The data needed within an object to run a script */
typedef struct _script_context
{
	SCRIPT_CODE		*psCode;		// The actual script to run
	VAL_CHUNK		*psGlobals;		// The objects copy of the global variables
	SDWORD			triggerCount;	// Number of currently active triggers
	CONTEXT_RELEASE		release;		// Whether to release the context when there are no triggers
	SWORD			id;

	struct _script_context *psNext;
} SCRIPT_CONTEXT;

/*
 * A currently active trigger.
 * If the type of the triggger == TR_PAUSE, the trigger number stored is the
 * index of the trigger to replace this one when the event restarts
 */
typedef struct _active_trigger
{
	UDWORD				testTime;
	SCRIPT_CONTEXT		*psContext;
	SWORD				type;			// enum - TRIGGER_TYPE
	SWORD				trigger;
	UWORD				event;
	UWORD				offset;

	struct _active_trigger *psNext;
} ACTIVE_TRIGGER;

// ID numbers for each user type
typedef enum _scr_user_types
{
	ST_INTMESSAGE = VAL_USERTYPESTART,		// Intelligence message ?? (6) - (pointer)
	ST_BASEOBJECT,							// Base object (pointer)
	ST_DROID,								// Droid object (pointer)
	ST_STRUCTURE,							// Structure object (pointer)
	ST_FEATURE,								// Feature object (pointer)
	ST_BASESTATS,							// General stats type
	ST_COMPONENT,							// General component
	ST_BODY,								// Component types (integer)
	ST_PROPULSION,					//Propulsion type (integer)
	ST_ECM,									//ECM type (integer)
	ST_SENSOR,							//Sensor type (integer)
	ST_CONSTRUCT,					//Construction type (integer)
	ST_WEAPON,							//Droid weapon (integer)
	ST_REPAIR,							//Repair component (integer)
	ST_BRAIN,
	ST_TEMPLATE,							// Template object (pointer)
	ST_STRUCTUREID,							/* A structure ID number (don't really need this since just a number?) - integer*/
	ST_STRUCTURESTAT,						// structure stat type (integer/pointer offset with asStructureStats as base)
	ST_FEATURESTAT,							// feature stat type (integer)
	ST_DROIDID,								// ID of a droid (integer)
	ST_TEXTSTRING,							// text string for display messages in tutorial (string pointer)
	ST_SOUND,								//(integer)
	ST_LEVEL,								// The name of a game level (string pointer)
	ST_GROUP,								// A group of droids
	ST_RESEARCH,							// A research topic (pointer)

	//private types for game code - not for use in script
	ST_POINTER_O,								//used so we can check for NULL objects etc
	ST_POINTER_T,								//used so we can check for NULL templates
	ST_POINTER_S,								// NULL stats

	ST_POINTER_STRUCTSTAT,						//for NULLSTRUCTURESTAT

	ST_MAXTYPE,									// maximum possible type - should always be last
} SCR_USER_TYPES;


// The list of currently active triggers
extern ACTIVE_TRIGGER	*psTrigList;

// The list of callback triggers
extern ACTIVE_TRIGGER	*psCallbackList;

// The currently allocated contexts
extern SCRIPT_CONTEXT	*psContList;


/* Initialise the event system */
extern BOOL eventInitialise(void);

// Shutdown the event system
extern void eventShutDown(void);

// add a TR_PAUSE trigger to the event system.
extern BOOL eventAddPauseTrigger(SCRIPT_CONTEXT *psContext, UDWORD event, UDWORD offset,
						  UDWORD time);

// Load a trigger into the system from a save game
extern BOOL eventLoadTrigger(UDWORD time, SCRIPT_CONTEXT *psContext,
					  SDWORD type, SDWORD trigger, UDWORD event, UDWORD offset);

//resets the event timer - updateTime
extern void eventTimeReset(UDWORD initTime);

extern const char *eventGetEventID(SCRIPT_CODE *psCode, SDWORD event);
extern const char *eventGetTriggerID(SCRIPT_CODE *psCode, SDWORD trigger);

#endif


