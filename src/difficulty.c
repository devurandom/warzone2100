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
// ------------------------------------------------------------------------------------
/* Simple short file - only because there was nowhere else for it logically to go */
/* Handes the difficulty level effects on gameplay */


/*
	Changed to allow seperate modifiers for enemy and player damage.
*/


// ------------------------------------------------------------------------------------
#include "lib/framework/frame.h"
#include "difficulty.h"
// ------------------------------------------------------------------------------------

static DIFFICULTY_LEVEL	presDifLevel = DL_NORMAL;
static float		fDifPlayerModifier;
static float		fDifEnemyModifier;


void setModifiers(float Player,float Enemy)
{
	fDifPlayerModifier = Player;
	fDifEnemyModifier = Enemy;
}


// ------------------------------------------------------------------------------------
/* Sets the game difficulty level */
void	setDifficultyLevel(DIFFICULTY_LEVEL lev)
{

	switch(lev)
	{
	case	DL_EASY:
		fDifPlayerModifier = 120.f / 100.f;
		fDifEnemyModifier = 100.f / 100.f;
		break;
	case	DL_NORMAL:
		fDifPlayerModifier = 100.f / 100.f;
		fDifEnemyModifier = 100.f / 100.f;
		break;
	case	DL_HARD:
		fDifPlayerModifier = 80.f / 100.f;
		fDifEnemyModifier = 100.f / 100.f;
		break;
	case	DL_KILLER:
		fDifPlayerModifier = 999.f / 100.f;	// 10 times
		fDifEnemyModifier = 1.f / 100.f;		// almost nothing
		break;
	case	DL_TOUGH:
		fDifPlayerModifier = 100.f / 100.f;
		fDifEnemyModifier = 50.f / 100.f;	// they do less damage!
		break;
	default:
		debug( LOG_ERROR, "Invalid difficulty level selected - forcing NORMAL" );
		abort();
		break;
	}

	presDifLevel = lev;
}

// ------------------------------------------------------------------------------------
/* Returns the difficulty level */
DIFFICULTY_LEVEL	getDifficultyLevel( void )
{
	return(presDifLevel);
}

// ------------------------------------------------------------------------------------
int modifyForDifficultyLevel(int basicVal, bool IsPlayer)
{
	if (IsPlayer)
		return math_round(basicVal * fDifPlayerModifier);
	else
		return math_round(basicVal * fDifEnemyModifier);
}
// ------------------------------------------------------------------------------------
