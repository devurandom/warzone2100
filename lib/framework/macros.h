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
/*! \file macros.h
 *  \brief Various macro definitions
 */
#ifndef MACROS_H
#define MACROS_H

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) < 0) ? (-(a)) : (a))

#define ABSDIF(a,b) ((a)>(b) ? (a)-(b) : (b)-(a))

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

#define CLIP(val, min, max) do                                                \
{                                                                             \
    if ((val) < (min)) (val) = (min);                                         \
    else if ((val) > (max)) (val) = (max);                                    \
} while(0)

/**
 * Returns the index of the lowest value in the array.
 */
static inline int arrayMinIndex(const int *array, const size_t n, int *value)
{
	int i, minIdx;
	
	// Find the index of the minimum value
	for (i = minIdx = 0; i < n; i++)
	{
		if (array[i] < array[minIdx])
		{
			minIdx = i;
		}
	}
	
	// If requested, store the minimum value
	if (value != NULL)
	{
		*value = array[minIdx];
	}
	
	// Return the array
	return minIdx;
}

/*
   defines for ONEINX
   Use: if (ONEINX) { code... }
*/
#define	ONEINTWO				(rand()%2==0)
#define ONEINTHREE				(rand()%3==0)
#define ONEINFOUR				(rand()%4==0)
#define ONEINFIVE				(rand()%5==0)
#define ONEINSIX				(rand()%6==0)
#define ONEINSEVEN				(rand()%7==0)
#define ONEINEIGHT				(rand()%8==0)
#define ONEINNINE				(rand()%9==0)
#define ONEINTEN				(rand()%10==0)

#define MACROS_H_STRINGIFY(x) #x
#define TOSTRING(x) MACROS_H_STRINGIFY(x)

#define AT_MACRO __FILE__ ":" TOSTRING(__LINE__)

#define MKID(a) MKID_(a, __LINE__)
#define MKID_(a, b) a ## b

#endif // MACROS_H
