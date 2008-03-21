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

#ifndef __INCLUDED_SRC_EFFECTS_H__
#define __INCLUDED_SRC_EFFECTS_H__

/*
	All singing, all dancing new effects code. Does all the stuff
	that explosion.c and most of particle.c used to do as well as
	all the construction dust clouds, body parts and all other
	temporary world 'effects
	Alex McLean, Pumpkin Studios, EIDOS Interactive, 1998.
*/
#include "lib/ivis_common/piedef.h"
#include "lib/ivis_common/pietypes.h"

/* All the effect groups */
typedef enum
{
EFFECT_EXPLOSION,
EFFECT_CONSTRUCTION,
EFFECT_SMOKE,
EFFECT_STRUCTURE,
EFFECT_GRAVITON,
EFFECT_WAYPOINT,
EFFECT_BLOOD,
EFFECT_DESTRUCTION,
EFFECT_SAT_LASER,
EFFECT_DUST_BALL,
EFFECT_FIRE,
EFFECT_FIREWORK
}EFFECT_GROUP;

/* Might not even need this */
typedef enum
{
EXPLOSION_TYPE_SMALL,
EXPLOSION_TYPE_VERY_SMALL,
EXPLOSION_TYPE_MEDIUM,
EXPLOSION_TYPE_LARGE,
EXPLOSION_TYPE_SPECIFIED,	// superfluous?
EXPLOSION_TYPE_NOT_FACING,
EXPLOSION_TYPE_SPECIFIED_SOLID,
EXPLOSION_TYPE_SPECIFIED_FIXME,
EXPLOSION_TYPE_FLAMETHROWER,
EXPLOSION_TYPE_LASER,
EXPLOSION_TYPE_TESLA,
EXPLOSION_TYPE_DISCOVERY,
EXPLOSION_TYPE_FLARE,
EXPLOSION_TYPE_PLASMA,
EXPLOSION_TYPE_KICKUP,
EXPLOSION_TYPE_LAND_LIGHT,
EXPLOSION_TYPE_SHOCKWAVE,

//EXPLOSION_TYPE_SKYSCRAPER,

GRAVITON_TYPE_STANDARD,
GRAVITON_TYPE_EMITTING_DR,
GRAVITON_TYPE_EMITTING_ST,
GRAVITON_TYPE_GIBLET,

SMOKE_TYPE_DRIFTING,
SMOKE_TYPE_DRIFTING_HIGH,
SMOKE_TYPE_DRIFTING_SMALL,
SMOKE_TYPE_BILLOW,
SMOKE_TYPE_STEAM,
SMOKE_TYPE_TRAIL,
FIRE_TYPE_LOCALISED,
FIRE_TYPE_SMOKY,
FIRE_TYPE_SMOKY_BLUE,
CONSTRUCTION_TYPE_DRIFTING,

BLOOD_TYPE_NORMAL,

DUST_TYPE_NORMAL,
DESTRUCTION_TYPE_DROID,
DESTRUCTION_TYPE_STRUCTURE,
DESTRUCTION_TYPE_POWER_STATION,
DESTRUCTION_TYPE_WALL_SECTION,
DESTRUCTION_TYPE_FEATURE,
DESTRUCTION_TYPE_SKYSCRAPER,

SAT_LASER_STANDARD,

WAYPOINT_TYPE,

FIREWORK_TYPE_STARBURST,
FIREWORK_TYPE_LAUNCHER,

}EFFECT_TYPE;

/* Is the slot currently being used and is it active? */
typedef enum
{
ES_INACTIVE,
ES_ACTIVE,
ES_DORMANT
}EFFECT_STATUS;

typedef enum
{
	LL_MIDDLE,
	LL_INNER,
	LL_OUTER
}LAND_LIGHT_SPEC;

#define SHOCK_WAVE_HEIGHT	(64)

typedef struct	_effect_def
{
	uint8_t           control;		// Controls the bits above - essential,flips etc
	uint8_t           group;			// what	group is it - explosion, building effect etc....
	uint8_t           type;			// what type is it within the group?
	uint8_t           frameNumber;	// what frame number is the imd on?
	uint16_t          size;			// Size in terms of percent of original imd.
	uint8_t           baseScale;		// if scaled, what's bottom line?
	uint8_t           specific;		// how many times has it bounced?
	Vector3f          position;		// world coordinates of the effect - floats on the PC.
	Vector3f          velocity;		// movement values per update
	Vector3i          rotation;		// current rotation - only for gravitons
	Vector3i          spin;			// rotation info for spinning things.
	uint32_t          birthTime;		// what time was it introduced into the world?
	uint32_t          lastFrame;		// when did we last update the frame?
	uint16_t          frameDelay;		// how many game ticks between each frame?
	uint16_t          lifeSpan;		// what is it's life expectancy?
	uint16_t          radius;			// Used for area effects
	iIMDShape  *imd;			// pointer to the imd the effect uses.
} EFFECT;

/* Maximum number of effects in the world - need to investigate what this should be */
/* EXTERNAL REFERENCES */
extern void	effectGiveAuxVar		( UDWORD var); // naughty
extern void	effectGiveAuxVarSec		( UDWORD var); // and so's this

extern void	initEffectsSystem		( void );
extern void	processEffects			( void );
extern void	addEffect				( Vector3i *pos, EFFECT_GROUP group,
										EFFECT_TYPE type, BOOL specified, iIMDShape *imd, BOOL lit );
extern void	addMultiEffect			( Vector3i *basePos, Vector3i *scatter,EFFECT_GROUP group,
									EFFECT_TYPE type,BOOL specified, iIMDShape *imd, UDWORD number, BOOL lit, UDWORD size );

extern void	renderEffect			( EFFECT *psEffect );
extern UDWORD getNumEffects			( void );
extern UDWORD	getFreeEffect		( void );
extern void	effectResetUpdates		( void );
extern UDWORD	getNumActiveEffects		( void );
extern UDWORD	getMissCount( void );
extern	UDWORD	getNumSkippedEffects(void);
extern	UDWORD	getNumEvenEffects(void);

extern	bool fireOnLocation(unsigned int x, unsigned int y);

extern UDWORD EffectGetNumFrames(EFFECT *psEffect);
extern UDWORD IMDGetNumFrames(iIMDShape *Shape);
extern UDWORD IMDGetAnimInterval(iIMDShape *Shape);
extern void	initPerimeterSmoke			( iIMDShape *pImd, UDWORD x, UDWORD y, UDWORD z);

#define SKY_MULT	1
#define SKY_SHIMMY_BASE	((DEG(1)*SKY_MULT)/2)
#define SKY_SHIMMY (SKY_SHIMMY_BASE - (rand()%(2*SKY_SHIMMY_BASE)))
extern bool readFXData(const char* fileName);
extern bool	writeFXData(const char* fileName);
extern	void	effectSetSize(UDWORD size);
extern void	effectSetLandLightSpec(LAND_LIGHT_SPEC spec);

#endif // __INCLUDED_SRC_EFFECTS_H__
