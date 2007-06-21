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
 * DroidDef.h
 *
 * Droid structure definitions
 */
#ifndef _droiddef_h
#define _droiddef_h

#include "lib/gamelib/animobj.h"
#include "lib/sound/audio.h"

#include "base.h"
#include "deliverance.h"
#include "movedef.h"
#include "statsdef.h"
#include "weapons.h"

/* The number of components in the asParts / asBits arrays */
#define DROID_MAXCOMP		(COMP_NUMCOMPONENTS - 1)//(COMP_NUMCOMPONENTS - 2)

/* The maximum number of droid weapons */
#define DROID_MAXWEAPS		3//Watermelon:its 3 again now
#define	DROID_DAMAGE_SCALING	400
// This should really be logarithmic
#define	CALC_DROID_SMOKE_INTERVAL(x) ((((100-x)+10)/10) * DROID_DAMAGE_SCALING)

//defines how many times to perform the iteration on looking for a blank location
#define LOOK_FOR_EMPTY_TILE		20
//used to get a location next to a droid - withinh one tile
#define LOOK_NEXT_TO_DROID		8




/* The different types of droid */
// NOTE, if you add to, or change this list then you'll need
// to update the DroidSelectionWeights lookup table in Display.c
typedef enum _droid_type
{
	DROID_WEAPON,		// Weapon droid
	DROID_SENSOR,		// Sensor droid
	DROID_ECM,			// ECM droid
	DROID_CONSTRUCT,	// Constructor droid
	DROID_PERSON,		// person
	DROID_CYBORG,		// cyborg-type thang
	DROID_TRANSPORTER,	// guess what this is!
	DROID_COMMAND,		// Command droid
	DROID_REPAIR,		// Repair droid
	DROID_DEFAULT,		// Default droid
    DROID_CYBORG_CONSTRUCT, //cyborg constructor droid - new for update 28/5/99
    DROID_CYBORG_REPAIR,    //cyborg repair droid - new for update 28/5/99
    DROID_CYBORG_SUPER,		//cyborg repair droid - new for update 7/6/99
	DROID_ANY,			// Any droid, Only used as a parameter for intGotoNextDroidType(droidtype).
} DROID_TYPE;

#define MAX_STATS (255)

typedef struct _component
{
//	UDWORD					nStat;
	UBYTE			nStat;				// Allowing a maximum of 255 stats per file
	//UDWORD					hitPoints; NOT USED?

	/* Possibly a set of function pointers here to be called when
	 * damage is done to a component, power is low ...
	 */
} COMPONENT;

// maximum number of queued orders
#define ORDER_LIST_MAX		10

typedef struct _order_list
{
	SDWORD order;
    void *psOrderTarget; //this needs to cope with objects and stats
	UWORD x, y, x2, y2;      //line build requires two sets of coords
} ORDER_LIST;

// maximum number of characters in a droid name
#define DROID_MAXNAME	MAX_NAME_SIZE

typedef struct _droid_template
{
	// On the PC the pName entry in STATS_BASE is redundant and can be assumed to be NULL,
	STATS_BASE;						/* basic stats */

	// on the PC this contains the full editable ascii name of the template
	char			aName[DROID_MAXNAME];

	UBYTE 			NameVersion;			// Version number used in name (e.g. Viper Mk "I" would be stored as 1 - Viper Mk "X" as 10)

	/* The droid components.  This array is indexed by COMPONENT_TYPE
	 * so the ECM would be accessed using asParts[COMP_ECM].
	 * COMP_BRAIN is an index into the asCommandDroids array NOT asBrainStats
	 */
	SDWORD			asParts[DROID_MAXCOMP];

	UDWORD			buildPoints;				/*total build points required to manufacture
												  the droid*/
	UDWORD			powerPoints;				/*total power points required to build/maintain
												  the droid */
	UDWORD			storeCount;					/*used to load in weaps and progs*/
	/* The weapon systems */
	UDWORD			numWeaps;					/* Number of weapons*/
	UDWORD			asWeaps[DROID_MAXWEAPS];	/* weapon indices */

	DROID_TYPE		droidType;					// The type of droid
	UDWORD			multiPlayerID;				// multiplayer unique descriptor(cant use id's for templates)
												// used for save games as well now - AB 29/10/98
	struct _droid_template	*psNext;			/* Pointer to next template*/

} DROID_TEMPLATE;


typedef struct _droid
{
	/* The common structure elements for all objects */
	BASE_ELEMENTS(struct _droid);

	//Ascii name of the droid - This is generated from the droid template and can not be changed by the game player after creation.
	char		aName[DROID_MAXNAME];

	DROID_TYPE	droidType;			// The type of droid

	/* Possibly a set of function pointers here to be called when
	 * damage is done to a component, power is low ...
	 */

//	DROID_TEMPLATE	*pTemplate;		//defines the droids components

	/* Holds the specifics for the component parts - allows damage
	 * per part to be calculated. Indexed by COMPONENT_TYPE.
	 * Weapons need to be dealt with separately.
	 * COMP_BRAIN is an index into the asCommandDroids array NOT asBrainStats
	 */
	COMPONENT		asBits[DROID_MAXCOMP];

	/* The other droid data.  These are all derived from the components
	 * but stored here for easy access
	 */
	UDWORD		weight;
	UDWORD		baseSpeed;		//the base speed dependant on propulsion type
	UDWORD		sensorRange;
	UDWORD		sensorPower;
	UDWORD		ECMMod;
	UDWORD		originalBody;		//the original body points
	UDWORD		body;				// the current body points
	//Watermleon:armour of all sides
	UDWORD		armour[NUM_HIT_SIDES][NUM_WEAPON_CLASS];
//tjc	UDWORD		imdNum;
	UWORD		numKills;
	UWORD		turretRotation[DROID_MAXWEAPS];		// Watermelon:turretRotation info for multiple turrents :)
	UWORD		turretPitch[DROID_MAXWEAPS];	//* Watermelon:turrentPitch info for multiple turrents :)
	UBYTE 		NameVersion;			// Version number used for generating on-the-fly names (e.g. Viper Mk "I" would be stored as 1 - Viper Mk "X" as 10)  - copied from droid template
	UBYTE		currRayAng;

	SWORD       resistance;             //used in Electronic Warfare

	UDWORD		numWeaps;		// Watermelon:Re-enabled this,I need this one in droid.c
	WEAPON		asWeaps[DROID_MAXWEAPS];

	// The group the droid belongs to
	struct		_droid_group	*psGroup;
	struct		_droid			*psGrpNext;
	struct      _structure      *psBaseStruct;  //a structure that this droid might be associated with
                                                    //for vtols its the rearming pad
	// queued orders
	SDWORD			listSize;
	ORDER_LIST		asOrderList[ORDER_LIST_MAX];

	/* Order data */
	SDWORD				order;
	UWORD				orderX, orderY;
	UWORD				orderX2, orderY2;

	UDWORD				lastHitWeapon;
	UDWORD				timeLastHit;
	BOOL				bTargetted;

	BASE_OBJECT			*psTarget[DROID_MAXWEAPS];	// Who to shoot at etc
	BASE_STATS			*psTarStats[DROID_MAXWEAPS];	// What to build etc; only first value ever used

	// secondary order data
	UDWORD				secondaryOrder;

	UDWORD				lastSync;			// multiplayer synchronization value.

	/* Action data */
	SDWORD				action;
	UDWORD				actionX,actionY;
	BASE_OBJECT			*psActionTarget[DROID_MAXWEAPS];	// Action target object
	UDWORD				actionStarted;		// Game time action started
	UDWORD				actionPoints;		// number of points done by action since start
	//UWORD				actionHeight;		// height to level the ground to for foundation,
											// possibly use it for other data as well? Yup! - powerAccrued!
	UWORD               powerAccrued;       // renamed the above variable since this is what its used for now!

	UBYTE				illumination;
	UBYTE				updateFlags;

	/* Movement control data */
	MOVE_CONTROL		sMove;
//	void				*lastTile;

	/* anim data */
	ANIM_OBJECT			*psCurAnim;
	SDWORD				iAudioID;
} DROID;


#endif
