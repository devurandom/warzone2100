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
 * ScriptObj.h
 *
 * Object access functions for the script library
 *
 */
#ifndef _scriptobj_h
#define _scriptobh_h


// id's for object variables
enum _objids
{
	OBJID_POSX,			// Position of a base object
	OBJID_POSY,
	OBJID_POSZ,
	OBJID_ID,			// id of a base object
	OBJID_PLAYER,		// player of a base object
	OBJID_TYPE,			// type of a base object
	OBJID_ORDER,		// current droid order
	OBJID_DROIDTYPE,	// what type of droid
	OBJID_CLUSTERID,	// which cluster the object is a member of
	OBJID_HEALTH,		// %age damage level
	OBJID_BODY,			// the body component
	OBJID_PROPULSION,	// the propulsion component
	OBJID_WEAPON,		// the weapon component
	OBJID_STRUCTSTAT,	// the stat of a structure
	OBJID_STRUCTSTATTYPE,//new
	OBJID_ORDERX,		// order coords.106
	OBJID_ORDERY,
	OBJID_ACTION,
	OBJID_SELECTED,		// if droid is selected (humans only)
	OBJID_TARGET,		// added object->psTarget
	OBJID_GROUP,		// group a droid belongs to
};

// id's for group variables
enum _groupids
{
	GROUPID_POSX,		// average x of a group
	GROUPID_POSY,		// average y of a group
	GROUPID_MEMBERS,	// number of units in a group
	GROUPID_HEALTH,		// average health of a group
	GROUPID_TYPE,		// group type, one of: GT_NORMAL, GT_COMMAND or GT_TRANSPORTER
	GROUPID_CMD,		// commander of the group if group type == GT_COMMAND
};

// Get values from a base object
extern BOOL scrBaseObjGet(UDWORD index);

// Set values from a base object
extern BOOL scrBaseObjSet(UDWORD index);

// convert a base object to a droid if it is the right type
extern BOOL scrObjToDroid(void);

// convert a base object to a structure if it is the right type
extern BOOL scrObjToStructure(void);

// convert a base object to a feature if it is the right type
extern BOOL scrObjToFeature(void);

// Get values from a group
extern BOOL scrGroupObjGet(UDWORD index);

// default value save routine
extern BOOL scrValDefSave(INTERP_VAL *psVal, char *pBuffer, UDWORD *pSize);

// default value load routine
extern BOOL scrValDefLoad(SDWORD version, INTERP_VAL *psVal, char *pBuffer, UDWORD size);


#endif

