/*
 * GatewayDef.h
 *
 * Structure definitions for routing gateways.
 *
 */
#ifndef _gatewaydef_h
#define _gatewaydef_h

typedef struct _gateway_link
{
	struct _gateway  *psGateway;
	SDWORD			 dist;
} GATEWAY_LINK;

typedef struct _gateway
{
	UBYTE		x1,y1, x2,y2;

	UBYTE		zone1;		// zone to the left/above the gateway
	UBYTE		zone2;		// zone to the right/below the gateway

	struct _gateway		*psNext;

	GATEWAY_LINK		*psLinks;	// array of links to other zones
	UBYTE				zone1Links;	// number of links
	UBYTE				zone2Links;

	// Data for the gateway router
	UBYTE	flags;		// open or closed node
	SWORD	dist, est;	// distance so far and estimate to end

	struct _gateway *psOpen;
	struct _gateway	*psRoute;	// Previous point in the route

} GATEWAY;


// types of node for the gateway router
enum _gw_node_flags
{
	GWR_OPEN		= 0x01,
	GWR_CLOSED		= 0x02,
	GWR_ZONE1		= 0x04,		// the route goes from zone1 to zone2
	GWR_ZONE2		= 0x08,		// the route goes from zone2 to zone1
	GWR_INROUTE		= 0x10,		// the gateway is part of the final route
	GWR_BLOCKED		= 0x20,		// the gateway is totally blocked
	GWR_WATERLINK	= 0x80,		// the gateway is a land/water link
};

// the flags reset by the router
#define GWR_RESET_MASK	0x7f

// the maximum width and height of the map
#define GW_MAP_MAXWIDTH		(MAP_MAXWIDTH - 1)
#define GW_MAP_MAXHEIGHT	(MAP_MAXHEIGHT - 1)



#endif

