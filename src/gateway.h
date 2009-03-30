/*
	This file is part of Warzone 2100.
	Copyright (C) 1999-2004  Eidos Interactive
	Copyright (C) 2005-2009  Warzone Resurrection Project

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
/** @file
 *  Interface to routing gateway code.
 */

#ifndef __INCLUDED_SRC_GATEWAY_H__
#define __INCLUDED_SRC_GATEWAY_H__

typedef struct _gateway
{
	UBYTE			x1,y1, x2,y2;
	struct _gateway		*psNext;
} GATEWAY;

/// Initialise the gateway system
BOOL gwInitialise(void);

/// Shutdown the gateway system
void gwShutDown(void);

/// Add a gateway to the system
BOOL gwNewGateway(SDWORD x1, SDWORD y1, SDWORD x2, SDWORD y2);

/// Release a gateway
void gwFreeGateway(GATEWAY *psDel);

/// Get number of gateways.
UDWORD gwNumGateways(void);

/// Get the gateway list.
GATEWAY *gwGetGateways(void);

/// Set gateway list
void gwSetGateways(GATEWAY *ps);

#endif // __INCLUDED_SRC_GATEWAY_H__
