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
 * Message.c
 *
 * Functions for the messages shown in the Intelligence Map
 *
 */
#include <string.h>

#include "lib/framework/frame.h"
#include "lib/framework/strres.h"
#include "lib/framework/frameresource.h"
#include "message.h"
#include "stats.h"
#include "text.h"
#include "console.h"
#include "lib/sound/audio.h"
#include "lib/sound/audio_id.h"
#include "hci.h"
#include "lib/ivis_common/piedef.h"
#include "objmem.h"
#include "map.h"

#include "multiplay.h"

/* Allocation sizes for the message heaps */
#define MESSAGE_INIT		20
#define MESSAGE_EXT			5
#define VIEWDATA_INIT		5		// was 2 ... but that wasn't enough
#define VIEWDATA_EXT		1

/* Allocation sizes for the proximity display heaps - this should coincide with
the number of Proximity Messages for a mission*/
#define PROXDISP_INIT		10
#define PROXDISP_EXT		5

//max number of text strings or sequences for viewdata
#define MAX_DATA		4

//VIEWDATA			*asViewData;
//UDWORD			numViewData;

//array of pointers for the view data
VIEWDATA_LIST			*apsViewData;

/* The id number for the next message allocated
 * Each message will have a unique id number irrespective of type
 */
UDWORD	msgID = 0;

static int currentNumProxDisplays;
/* The list of messages allocated */
MESSAGE		*apsMessages[MAX_PLAYERS];

/* The list of proximity displays allocated */
PROXIMITY_DISPLAY *apsProxDisp[MAX_PLAYERS];

/* The current tutorial message - there is only ever one at a time. They are displayed
when called by the script. They are not to be re-displayed*/
//MESSAGE		tutorialMessage;

/* The IMD to use for the proximity messages */
iIMDShape	*pProximityMsgIMD;

//function declarations
static void addProximityDisplay(MESSAGE *psMessage, BOOL proxPos, UDWORD player);
static void removeProxDisp(MESSAGE *psMessage, UDWORD player);
//static void checkMessages(VIEWDATA *psViewData);
static void checkMessages(MSG_VIEWDATA *psViewData);


/* Creating a new message
 * new is a pointer to a pointer to the new message
 * type is the type of the message
 */
// ajl modified for netgames
extern UDWORD selectedPlayer;

static inline MESSAGE* createMessage(MESSAGE_TYPE msgType)
{
	MESSAGE *newMsg;

	// Allocate memory for the message, and on failure return a NULL pointer
	newMsg = (MESSAGE*)malloc(sizeof(MESSAGE));
	if (newMsg == NULL)
	{
		debug(LOG_ERROR, "createMessage: out of memory\n");
		return NULL;
	}

	newMsg->type = msgType;
	newMsg->id = (msgID << 3) | selectedPlayer;
	msgID++;

	return newMsg;
}

/* Add the message to the BOTTOM of the list
 * list is a pointer to the message list
 * Order is now CAMPAIGN, MISSION, RESEARCH/PROXIMITY
 */
static inline void addMessageToList(MESSAGE *list[MAX_PLAYERS], MESSAGE *msg, UDWORD player)
{
	MESSAGE *psCurr = NULL, *psPrev = NULL;

	ASSERT( msg != NULL,
		"addMessageToList: Invalid message pointer" );

	// If there is no message list, create one
	if (list[player] == NULL)
	{
		list[player] = msg;
		msg->psNext = NULL;

		return;
	}

	switch (msg->type)
       	{
		case MSG_CAMPAIGN:
			/*add it before the first mission/research/prox message */
			for(psCurr = list[player]; psCurr != NULL; psCurr = psCurr->psNext)
			{
				if (psCurr->type == MSG_MISSION ||
				    psCurr->type == MSG_RESEARCH ||
				    psCurr->type == MSG_PROXIMITY)
					break;

				psPrev = psCurr;
			}

			if (psPrev)
			{
				psPrev->psNext = msg;
				msg->psNext = psCurr;
			}
			else
			{
				//must be top of list
				psPrev = list[player];
				list[player] = msg;
				msg->psNext = psPrev;
			}

			break;
		case MSG_MISSION:
			/*add it before the first research/prox message */
			for(psCurr = list[player]; psCurr != NULL; psCurr = psCurr->psNext)
			{
				if (psCurr->type == MSG_RESEARCH ||
				    psCurr->type == MSG_PROXIMITY)
					break;

				psPrev = psCurr;
			}

			if (psPrev)
			{
				psPrev->psNext = msg;
				msg->psNext = psCurr;
			}
			else
			{
				//must be top of list
				psPrev = list[player];
				list[player] = msg;
				msg->psNext = psPrev;
			}

			break;
		case MSG_RESEARCH:
		case MSG_PROXIMITY:
			/*add it to the bottom of the list */

			// Iterate to the last item in the list
			for(psCurr = list[player]; psCurr->psNext != NULL; psCurr = psCurr->psNext);

			// Append the new message to the end of the list
			psCurr->psNext = msg;
			msg->psNext = NULL;

			break;
		default:
			debug(LOG_ERROR, "addMessageToList: unknown message type");
			break;
	}
}



/* Remove a message from the list
 * list is a pointer to the message list
 * del is a pointer to the message to remove
*/
static inline void removeMessageFromList(MESSAGE *list[], MESSAGE *del, UDWORD player)
{
	MESSAGE *psPrev = NULL, *psCurr;

	ASSERT( del != NULL,
		"removeMessageFromList: Invalid message pointer" );

	// If the message to remove is the first one in the list then mark the next one as the first
	if (list[player] == del)
	{
		list[player] = list[player]->psNext;
		free(del);
		return;
	}

	// Iterate through the list and find the item before the message to delete
	for(psCurr = list[player]; (psCurr != del) && (psCurr != NULL);	psCurr = psCurr->psNext)
	{
		psPrev = psCurr;
	}

	ASSERT( psCurr != NULL,
		"removeMessage: message not found in list" );

	if (psCurr != NULL)
	{
		// Modify the "next" pointer of the previous item to
		// point to the "next" item of the item to delete.
		psPrev->psNext = psCurr->psNext;
		free(del);
	}
}

static inline void releaseAllMessages(MESSAGE *list[])
{
	UDWORD	i;
	MESSAGE	*psCurr, *psNext;

	// Iterate through all players' message lists
	for(i=0; i < MAX_PLAYERS; i++)
	{
		// Iterate through all messages in list
		for(psCurr = list[i]; psCurr != NULL; psCurr = psNext)
		{
	 		psNext = psCurr->psNext;
			free(psCurr);
		}
		list[i] = NULL;
	}
}

BOOL messageInitVars(void)
{
	int i;

	msgID = 0;
	currentNumProxDisplays = 0;

	for(i=0; i<MAX_PLAYERS; i++) {
		apsMessages[i] = NULL;
		apsProxDisp[i] = NULL;
	}

	pProximityMsgIMD = NULL;

	return TRUE;
}

//allocates the viewdata heap
BOOL initViewData(void)
{
	return TRUE;
}

//destroys the viewdata heap
void viewDataHeapShutDown(void)
{
}

/*Add a message to the list */
MESSAGE * addMessage(MESSAGE_TYPE msgType, BOOL proxPos, UDWORD player)
{
	//first create a message of the required type
	MESSAGE* psMsgToAdd = createMessage(msgType);

	debug(LOG_WZ, "addMessage: adding message for player %d, type is %d, proximity is %d", player, msgType, proxPos);

	if (!psMsgToAdd)
	{
		debug(LOG_ERROR, "addMessage: createMessage failed");
		return NULL;
	}
	//then add to the players' list
	addMessageToList(apsMessages, psMsgToAdd, player);

	//initialise the message data
	psMsgToAdd->player = player;
	psMsgToAdd->pViewData = NULL;
	//psMsgToAdd->frameNumber = 0;
	psMsgToAdd->read = FALSE;

	//add a proximity display
	if (msgType == MSG_PROXIMITY)
	{
		addProximityDisplay(psMsgToAdd, proxPos, player);
	}
//	else
//	{
//		//make the reticule button flash as long as not prox msg or multiplayer game.
//		if (player == selectedPlayer && !bMultiPlayer)
//		{
//			flashReticuleButton(IDRET_INTEL_MAP);
//		}
//	}

	return psMsgToAdd;
}

/* adds a proximity display - holds varaibles that enable the message to be
 displayed in the Intelligence Screen*/
void addProximityDisplay(MESSAGE *psMessage, BOOL proxPos, UDWORD player)
{
	PROXIMITY_DISPLAY *psToAdd;

	//create the proximity display
	psToAdd = (PROXIMITY_DISPLAY*)malloc(sizeof(PROXIMITY_DISPLAY));
	if (psToAdd == NULL)
	{
		debug(LOG_ERROR, "addProximityDisplay: out of memory\n");
		return;
	}


	if (proxPos)
	{
		psToAdd->type = POS_PROXOBJ;
	}
	else
	{
		psToAdd->type = POS_PROXDATA;
	}
	psToAdd->psMessage = psMessage;
	psToAdd->screenX = 0;
	psToAdd->screenY = 0;
	psToAdd->screenR = 0;
	psToAdd->radarX = 0;
	psToAdd->radarY = 0;
	psToAdd->player = player;
	psToAdd->timeLastDrawn = 0;
	psToAdd->frameNumber = 0;
	psToAdd->selected = FALSE;
	psToAdd->strobe = 0;

	//now add it to the top of the list
	psToAdd->psNext = apsProxDisp[player];
	apsProxDisp[player] = psToAdd;

	//add a button to the interface
	intAddProximityButton(psToAdd, currentNumProxDisplays);
	currentNumProxDisplays++;
}

 /*remove a message */
void removeMessage(MESSAGE *psDel, UDWORD player)
{
	debug(LOG_WZ, "removeMessage: removing message for player %d", player);

	if (psDel->type == MSG_PROXIMITY)
	{
		removeProxDisp(psDel, player);
	}
	removeMessageFromList(apsMessages, psDel, player);
}

/* remove a proximity display */
void removeProxDisp(MESSAGE *psMessage, UDWORD player)
{
	PROXIMITY_DISPLAY		*psCurr, *psPrev;

	//find the proximity display for this message
	if (apsProxDisp[player]->psMessage == psMessage)
	{
		psCurr = apsProxDisp[player];

		apsProxDisp[player] = apsProxDisp[player]->psNext;
		intRemoveProximityButton(psCurr);
		free(psCurr);
	}
	else
	{
		psPrev = apsProxDisp[player];
		for(psCurr = apsProxDisp[player]; psCurr != NULL; psCurr =
			psCurr->psNext)
		{
			//compare the pointers
			if (psCurr->psMessage == psMessage)
			{
				psPrev->psNext = psCurr->psNext;
				intRemoveProximityButton(psCurr);
				free(psCurr);
				break;
			}
			psPrev = psCurr;
		}
	}
}

/* Remove all Messages*/
void freeMessages(void)
{
	releaseAllProxDisp();
	releaseAllMessages(apsMessages);
}

/* removes all the proximity displays */
void releaseAllProxDisp(void)
{
	UDWORD				player;
	PROXIMITY_DISPLAY	*psCurr, *psNext;

	for(player=0; player<MAX_PLAYERS; player++)
	{
		for(psCurr = apsProxDisp[player]; psCurr != NULL; psCurr = psNext)
		{
	 		psNext = psCurr->psNext;
			//remove message associated with this display
			removeMessage(psCurr->psMessage, player);
			//HEAP_FREE(psProxDispHeap, psCurr);
		}
		apsProxDisp[player] = NULL;
	}
	//re-initialise variables
	currentNumProxDisplays = 0;
}

/* Initialise the message heaps */
BOOL initMessage(void)
{
#ifdef VIDEO_TEST
	MESSAGE *psMessage;
#endif

	//set up the imd used for proximity messages
	pProximityMsgIMD = (iIMDShape *)resGetData("IMD", "arrow.pie");
	if (pProximityMsgIMD == NULL)
	{
		debug( LOG_ERROR, "Unable to load Proximity Message PIE" );
		abort();
		return FALSE;
	}

	//initialise the tutorial message - only used by scripts
	/*tutorialMessage.id = msgID;
	tutorialMessage.type = MSG_TUTORIAL;
	tutorialMessage.pViewData = NULL;
	tutorialMessage.read = FALSE;
	tutorialMessage.player = MAX_PLAYERS + 1;
	tutorialMessage.psNext = NULL;*/

	//JPS add message to get on screen video
#ifdef VIDEO_TEST
    //mission
	psMessage = addMessage(MSG_MISSION, FALSE, 0);
	if (psMessage)
	{
		psMessage->pViewData = (MSG_VIEWDATA *)getViewData("MB1A_MSG");
	}
    //campaign
	psMessage = addMessage(MSG_CAMPAIGN, FALSE, 0);
	if (psMessage)
	{
		psMessage->pViewData = (MSG_VIEWDATA *)getViewData("CMB1_MSG");
	}
#endif

	return TRUE;
}

static BOOL addToViewDataList(VIEWDATA *psViewData, UBYTE numData)
{
	VIEWDATA_LIST		*psAdd;
	psAdd = (VIEWDATA_LIST*)malloc(sizeof(VIEWDATA_LIST));
	if (psAdd == NULL)
	{
		debug(LOG_ERROR, "addToViewDataList: out of memory\n");
		return FALSE;
	}

	psAdd->psViewData = psViewData;
	psAdd->numViewData = numData;
	//add to top of list
	psAdd->psNext = apsViewData;
	apsViewData = psAdd;

	return TRUE;
}

/*load the view data for the messages from the file */
VIEWDATA *loadViewData(const char *pViewMsgData, UDWORD bufferSize)
{
	UDWORD				i, id, dataInc, seqInc, numFrames, numData, count, count2;
	VIEWDATA			*psViewData, *pData;
	VIEW_RESEARCH		*psViewRes;
	VIEW_REPLAY			*psViewReplay;
	char				name[MAX_STR_LENGTH], imdName[MAX_NAME_SIZE],
						string[MAX_STR_LENGTH],
						imdName2[MAX_NAME_SIZE];
	char				audioName[MAX_STR_LENGTH];
	SDWORD				LocX,LocY,LocZ, audioID;
	PROX_TYPE	proxType;
        int cnt;
	//keep the start so we release it at the end
	//pData = pViewMsgData;

	numData = numCR(pViewMsgData, bufferSize);
	if (numData > UBYTE_MAX)
	{
		debug( LOG_ERROR, "loadViewData: Didn't expect 256 viewData messages!" );
		abort();
		return NULL;
	}

	//allocate space for the data
	psViewData = (VIEWDATA *)malloc(numData * sizeof(VIEWDATA));
	if (psViewData == NULL)
	{
		debug( LOG_ERROR, "Unable to allocate memory for viewdata" );
		abort();
		return NULL;
	}

	//add to array list
	addToViewDataList(psViewData, (UBYTE)numData);

	//psViewData = asViewData;
	//save so can pass the value back
	pData = psViewData;

	for (i=0; i < numData; i++)
	{
		UDWORD numText;

		memset(psViewData, 0, sizeof(VIEWDATA));

		name[0] = '\0';

		//read the data into the storage - the data is delimeted using comma's
		sscanf(pViewMsgData,"%[^','],%d%n",name, &numText,&cnt);
                pViewMsgData += cnt;

		//check not loading up too many text strings
		if (numText > MAX_DATA)
		{
			debug( LOG_ERROR, "loadViewData: too many text strings for %s", psViewData->pName );
			abort();
			return NULL;
		}
		psViewData->numText=(UBYTE)numText;

		//allocate storage for the name
 		psViewData->pName = (char *)malloc((strlen(name))+1);
		if (psViewData->pName == NULL)
		{
			debug( LOG_ERROR, "ViewData Name - Out of memory" );
			abort();
			return NULL;
		}
		strcpy(psViewData->pName,name);

		//allocate space for text strings
		if (psViewData->numText)
		{
			psViewData->ppTextMsg = (char **) malloc(psViewData->numText *
				sizeof(char *));
		}

		//read in the data for the text strings
		for (dataInc = 0; dataInc < psViewData->numText; dataInc++)
		{
			name[0] = '\0';
			//sscanf(pViewMsgData,"%[^',']", &name);
			sscanf(pViewMsgData,",%[^',']%n",name,&cnt);
                        pViewMsgData += cnt;


			//get the ID for the string
			if (!strresGetIDNum(psStringRes, name, &id))
			{
				debug( LOG_ERROR, "Cannot find the view data string id %s ", name );
				abort();
				return NULL;
			}
			//get the string from the id
			psViewData->ppTextMsg[dataInc] = strresGetString(psStringRes, id);

		}

		//sscanf(pViewMsgData,"%d", &psViewData->type);
		sscanf(pViewMsgData,",%d%n", (int*) &psViewData->type,&cnt);
                pViewMsgData += cnt;

		//allocate data according to type
		switch (psViewData->type)
		{
		case VIEW_RES:
			psViewData->pData = (VIEW_RESEARCH *) malloc(sizeof(VIEW_RESEARCH));
			if (psViewData->pData == NULL)
			{
				debug( LOG_ERROR, "Unable to allocate memory" );
				abort();
				return NULL;
			}
			imdName[0] = '\0';
			imdName2[0] = '\0';
			string[0] = '\0';
			audioName[0] = '\0';
			//sscanf(pViewMsgData, "%[^','],%[^','],%[^','],%[^','],%d",
			//	&imdName, &imdName2, &string, &audioName, &numFrames);
			sscanf(pViewMsgData,",%[^','],%[^','],%[^','],%[^','],%d%n",
				imdName, imdName2, string, audioName, &numFrames,&cnt);
                        pViewMsgData += cnt;
			psViewRes = (VIEW_RESEARCH *)psViewData->pData;
			psViewRes->pIMD = (iIMDShape *) resGetData("IMD", imdName);
			if (psViewRes->pIMD == NULL)
			{
				debug( LOG_ERROR, "Cannot find the PIE for message %s", name );
				abort();
				return NULL;
			}
			if (strcmp(imdName2, "0"))
			{
				psViewRes->pIMD2 = (iIMDShape *) resGetData("IMD", imdName2);
				if (psViewRes->pIMD2 == NULL)
				{
					debug( LOG_ERROR, "Cannot find the 2nd PIE for message %s", name );
					abort();
					return NULL;
				}
			}
			else
			{
				psViewRes->pIMD2 = NULL;
			}
			strcpy(psViewRes->sequenceName, string);
			//get the audio text string
			if (strcmp(audioName, "0"))
			{
				//allocate space
				psViewRes->pAudio = (char *) malloc(strlen(audioName) + 1);
				if (psViewRes->pAudio == NULL)
				{
					debug( LOG_ERROR, "loadViewData - Out of memory" );
					abort();
					return NULL;
				}
				strcpy(psViewRes->pAudio, audioName);
			}
			else
			{
				psViewRes->pAudio = NULL;
			}
			//this is for the PSX only
			psViewRes->numFrames = (UWORD)numFrames;
			break;
		case VIEW_RPL:
		case VIEW_RPLX:
			// This is now also used for the stream playing on the PSX
			// NOTE: on the psx the last entry (audioID) is used as the number of frames in the stream
			psViewData->pData = (VIEW_REPLAY *) malloc(sizeof(VIEW_REPLAY));
			if (psViewData->pData == NULL)
			{
				debug( LOG_ERROR, "Unable to allocate memory" );
				abort();
				return NULL;
			}
			psViewReplay = (VIEW_REPLAY *)psViewData->pData;

			//read in number of sequences for this message
			//sscanf(pViewMsgData, "%d", &psViewReplay->numSeq);
			sscanf(pViewMsgData, ",%d%n", &count,&cnt);
                        pViewMsgData += cnt;

			if (count > MAX_DATA)
			{
				debug( LOG_ERROR, "loadViewData: too many sequence for %s", psViewData->pName );
				abort();
				return NULL;
			}

			psViewReplay->numSeq = (UBYTE)count;

			//allocate space for the sequences
			psViewReplay->pSeqList = (SEQ_DISPLAY*) malloc(psViewReplay->numSeq *
				sizeof(SEQ_DISPLAY));

			//read in the data for the sequences
			for (dataInc = 0; dataInc < psViewReplay->numSeq; dataInc++)
			{
				name[0] = '\0';
				//load extradat for extended type only
				if (psViewData->type == VIEW_RPL)
				{
					sscanf(pViewMsgData, ",%[^','],%d%n", name, &count,&cnt);
                                        pViewMsgData += cnt;
					if (count > MAX_DATA)
					{
						debug( LOG_ERROR, "loadViewData: too many strings for %s", psViewData->pName );
						abort();
						return NULL;
					}
					psViewReplay->pSeqList[dataInc].numText = (UBYTE)count;
					//set the flag to default
					psViewReplay->pSeqList[dataInc].flag = 0;
				}
				else //extended type
				{
					sscanf(pViewMsgData, ",%[^','],%d,%d%n", name, &count,	&count2,&cnt);
                                        pViewMsgData += cnt;
					if (count > MAX_DATA)
					{
						debug( LOG_ERROR, "loadViewData: invalid video playback flag %s", psViewData->pName );
						abort();
						return NULL;
					}
					psViewReplay->pSeqList[dataInc].flag = (UBYTE)count;
					//check not loading up too many text strings
					if (count2 > MAX_DATA)
					{
						debug( LOG_ERROR, "loadViewData: too many text strings for seq for %s", psViewData->pName );
						abort();
						return NULL;
					}
					psViewReplay->pSeqList[dataInc].numText = (UBYTE)count2;
				}
				strcpy(psViewReplay->pSeqList[dataInc].sequenceName,name);

				//get the text strings for this sequence - if any
				//allocate space for text strings
				if (psViewReplay->pSeqList[dataInc].numText)
				{
					psViewReplay->pSeqList[dataInc].ppTextMsg = (char **) malloc(
						psViewReplay->pSeqList[dataInc].numText * sizeof(char *));
				}
				//read in the data for the text strings
				for (seqInc = 0; seqInc < psViewReplay->pSeqList[dataInc].numText;
					seqInc++)
				{
					name[0] = '\0';
					sscanf(pViewMsgData,",%[^',']%n", name,&cnt);
                                        pViewMsgData += cnt;
					//get the ID for the string
					if (!strresGetIDNum(psStringRes, name, &id))
					{
						debug( LOG_ERROR, "Cannot find the view data string id %s ", name );
						abort();
						return NULL;
					}

					//get the string from the id
					psViewReplay->pSeqList[dataInc].ppTextMsg[seqInc] = strresGetString(psStringRes, id);




				}
				//get the audio text string
				sscanf(pViewMsgData,",%[^','],%d%n", audioName, &count,&cnt);
                                pViewMsgData += cnt;

				ASSERT( count < UWORD_MAX, "loadViewData: numFrames too high for %s", name );

				psViewReplay->pSeqList[dataInc].numFrames = (UWORD)count;

				if (strcmp(audioName, "0"))
				{
					//allocate space
					psViewReplay->pSeqList[dataInc].pAudio = (char *) malloc(
						strlen(audioName) + 1);
					if (psViewReplay->pSeqList[dataInc].pAudio == NULL)
					{
						debug( LOG_ERROR, "loadViewData - Out of memory" );
						abort();
						return NULL;
					}
					strcpy(psViewReplay->pSeqList[dataInc].pAudio, audioName);
				}
				else
				{
					psViewReplay->pSeqList[dataInc].pAudio = NULL;
				}
			}
			psViewData->type = VIEW_RPL;//no longer need to know if it is extended type
			break;

		case VIEW_PROX:
			psViewData->pData = (VIEW_PROXIMITY *) malloc(sizeof(VIEW_PROXIMITY));
			if (psViewData->pData == NULL)
			{
				debug( LOG_ERROR, "Unable to allocate memory" );
				assert( FALSE );
				return NULL;
			} else {
				int tmp;

				audioName[0] = '\0';
				sscanf( pViewMsgData, ", %d,%d,%d,%[^','],%d%n", &LocX, &LocY, &LocZ,
				        audioName, &tmp, &cnt);
				proxType = tmp;
			}
			pViewMsgData += cnt;

			//allocate audioID
			if ( strcmp( audioName, "0" ) == 0 )
			{
				audioID = NO_SOUND;
			}
			else
			{
				if ( (audioID = audio_GetIDFromStr( audioName )) == NO_SOUND )
				{
					debug( LOG_ERROR, "loadViewData: couldn't get ID %d for weapon sound %s", audioID, audioName );
					abort();
					return FALSE;
				}

				if ((audioID < 0
				  || audioID > ID_MAX_SOUND)
				 && audioID != NO_SOUND)
				{
					debug( LOG_ERROR, "Invalid Weapon Sound ID - %d for weapon %s", audioID, audioName );
					abort();
					return FALSE;
				}
			}


			((VIEW_PROXIMITY *)psViewData->pData)->audioID = audioID;

			if (LocX < 0)
			{
				ASSERT( FALSE,
					"loadViewData: Negative X coord for prox message - %s",name );
				return NULL;
			}
			((VIEW_PROXIMITY *)psViewData->pData)->x = (UDWORD)LocX;
			if (LocY < 0)
			{
				ASSERT( FALSE,
					"loadViewData: Negative Y coord for prox message - %s",name );
				return NULL;
			}
			((VIEW_PROXIMITY *)psViewData->pData)->y = (UDWORD)LocY;
			if (LocZ < 0)
			{
				ASSERT( FALSE,
					"loadViewData: Negative Z coord for prox message - %s",name );
				return NULL;
			}
			((VIEW_PROXIMITY *)psViewData->pData)->z = (UDWORD)LocZ;

			if (proxType > PROX_TYPES)
			{
//printf("proxType %d > %d\n",proxType,PROX_TYPES);
				ASSERT( FALSE, "Invalid proximity message sub type - %s", name );
				return NULL;
			}
			((VIEW_PROXIMITY *)psViewData->pData)->proxType = proxType;
			break;
		default:
			debug( LOG_ERROR, "Unknown ViewData type" );
			abort();
			return NULL;
		}
		//increment the pointer to the start of the next record
		pViewMsgData = strchr(pViewMsgData,'\n') + 1;
		//increment the list to the start of the next storage block
		psViewData++;
	}
//	free(pData);

	//return TRUE;
	return pData;
}

/*get the view data identified by the name */
VIEWDATA * getViewData(const char *pName)
{
	VIEWDATA_LIST *psList = NULL;
	unsigned int i = 0;

	ASSERT( strlen(pName) < MAX_STR_SIZE, "getViewData: verbose message name" );

	for (psList = apsViewData; psList != NULL; psList = psList->psNext)
	{
		for (i = 0; i < psList->numViewData; i++)
		{
			//compare the strings
			if (!strcmp(psList->psViewData[i].pName, pName))
			{
				//return psViewData;
				return &psList->psViewData[i];
			}
		}
	}

	debug( LOG_ERROR, "Unable to find viewdata for message %s", pName );
	abort();
	return NULL;
}

/* Release the message heaps */
BOOL messageShutdown(void)
{
	freeMessages();

	return TRUE;
}

/* Release the viewdata memory */
void viewDataShutDown(VIEWDATA *psViewData)
{
	VIEWDATA_LIST	*psList, *psPrev;
	//VIEWDATA		*psData;// = asViewData;
	//UDWORD		inc, numData;
	UDWORD			seqInc;
	VIEW_REPLAY		*psViewReplay;
	VIEW_RESEARCH	*psViewRes;
	UBYTE			i;

	psPrev = apsViewData;

	for (psList = apsViewData; psList != NULL; psList = psList->psNext)
	{
		if (psList->psViewData == psViewData)
		{
			for (i = 0; i < psList->numViewData; i++)
			{
				psViewData = &psList->psViewData[i];

				//check for any messages using this viewdata
				checkMessages((MSG_VIEWDATA *)psViewData);

				free(psViewData->pName);
				psViewData->pName = NULL;

				//free the space allocated for the text messages
				if (psViewData->numText)
				{
					free(psViewData->ppTextMsg);
					psViewData->ppTextMsg = NULL;
				}

				//free the space allocated for multiple sequences
				if (psViewData->type == VIEW_RPL)
				{
					psViewReplay = (VIEW_REPLAY *)psViewData->pData;
					if (psViewReplay->numSeq)
					{
						for (seqInc = 0; seqInc < psViewReplay->numSeq; seqInc++)
						{
							//free the space allocated for the text messages
							if (psViewReplay->pSeqList[seqInc].numText)
							{
								free(psViewReplay->pSeqList[seqInc].ppTextMsg);
								psViewReplay->pSeqList[seqInc].ppTextMsg = NULL;
							}
							if (psViewReplay->pSeqList[seqInc].pAudio)
							{
								free(psViewReplay->pSeqList[seqInc].pAudio);
								psViewReplay->pSeqList[seqInc].pAudio = NULL;
							}
						}
						free(psViewReplay->pSeqList);
						psViewReplay->pSeqList = NULL;
					}
				}
				else if (psViewData->type == VIEW_RES)
				{
					psViewRes = (VIEW_RESEARCH *)psViewData->pData;
					if (psViewRes->pAudio)
					{
						free(psViewRes->pAudio);
						psViewRes->pAudio = NULL;
					}
				}
				free(psViewData->pData);
				psViewData->pData = NULL;
			}
			free(psList->psViewData);
			psList->psViewData = NULL;

			//remove viewData list from the heap
			if (psList == apsViewData)
			{
				apsViewData = psList->psNext;
				free(psList);
			}
			else
			{
				psPrev->psNext = psList->psNext;
				free(psList);
			}
			break;
		}
	}
	psPrev = psList;
}

/* Looks through the players list of messages to find one with the same viewData
pointer and which is the same type of message - used in scriptFuncs */
MESSAGE * findMessage(MSG_VIEWDATA *pViewData, MESSAGE_TYPE type, UDWORD player)
{
	MESSAGE					*psCurr;

	for (psCurr = apsMessages[player]; psCurr != NULL; psCurr = psCurr->psNext)
	{
		if (psCurr->type == type && psCurr->pViewData == pViewData)
		{
			return psCurr;
		}
	}

	//not found the message so return NULL
	return NULL;
}

/* 'displays' a proximity display*/
void displayProximityMessage(PROXIMITY_DISPLAY *psProxDisp)
{
	FEATURE			*psFeature;
	VIEWDATA		*psViewData;
	VIEW_PROXIMITY	*psViewProx;
	//char			msgStr[255];

	if (psProxDisp->type == POS_PROXDATA)
	{
		psViewData = (VIEWDATA *) psProxDisp->psMessage->pViewData;

		//display text - if any
		if (psViewData->ppTextMsg)
		{
			//Beacon stuff: Add player number to the text
			//if(psViewData->type == VIEW_HELP)
			//{
			//	//NOTE: this seems to cause GFX artefacts for some players
			//	sprintf(msgStr, "%s", psViewData->ppTextMsg[0]);	//temporary solution
			//	addConsoleMessage( msgStr, DEFAULT_JUSTIFY );
			//}
			//else
			//{
				if(psViewData->type != VIEW_HELP)
				{
					addConsoleMessage( psViewData->ppTextMsg[0], DEFAULT_JUSTIFY );
				}
			//}
		}

		//play message - if any
		psViewProx = (VIEW_PROXIMITY *) psViewData->pData;
		if ( psViewProx->audioID != NO_AUDIO_MSG )
		{
			audio_QueueTrackPos( psViewProx->audioID, psViewProx->x,
									psViewProx->y, psViewProx->z );
		}
	}
	else if (psProxDisp->type == POS_PROXOBJ)
	{
		ASSERT( ((BASE_OBJECT *)psProxDisp->psMessage->pViewData)->type ==
			OBJ_FEATURE, "displayProximityMessage: invalid feature" );

		psFeature = (FEATURE *)psProxDisp->psMessage->pViewData;
		if (psFeature->psStats->subType == FEAT_OIL_RESOURCE)
		{
			//play default audio message for oil resource
			audio_QueueTrackPos( ID_SOUND_RESOURCE_HERE, psFeature->x,
								 psFeature->y, psFeature->z );
		}
		else if (psFeature->psStats->subType == FEAT_GEN_ARTE)
		{
			//play default audio message for artefact
			audio_QueueTrackPos( ID_SOUND_ARTIFACT, psFeature->x,
								 psFeature->y, psFeature->z );
		}
	}

	//set the read flag
	psProxDisp->psMessage->read = TRUE;

}

/*void storeProximityScreenCoords(MESSAGE *psMessage, SDWORD x, SDWORD y)
{
	PROXIMITY_DISPLAY		*psProxDisp = NULL;

	psProxDisp = getProximityDisplay(psMessage);
	if (psProxDisp)
	{
		psProxDisp->screenX = x;
		psProxDisp->screenY = y;
	}
	else
	{
		ASSERT( FALSE, "Unable to find proximity display" );
	}
}*/

PROXIMITY_DISPLAY * getProximityDisplay(MESSAGE *psMessage)
{
	PROXIMITY_DISPLAY	*psCurr;

	if (apsProxDisp[psMessage->player]->psMessage == psMessage)
	{
		return apsProxDisp[psMessage->player];
	}
	else
	{
		for(psCurr = apsProxDisp[psMessage->player]; psCurr != NULL; psCurr = psCurr->psNext)
		{
			if (psCurr->psMessage == psMessage)
			{
				return psCurr;
			}
		}
	}
	return NULL;
}

//check for any messages using this viewdata and remove them
//void checkMessages(VIEWDATA *psViewData)
void checkMessages(MSG_VIEWDATA *psViewData)
{
	MESSAGE			*psCurr, *psNext;
	UDWORD			i;

	for (i=0; i < MAX_PLAYERS; i++)
	{
		for (psCurr = apsMessages[i]; psCurr != NULL; psCurr = psNext)
		{
			psNext = psCurr->psNext;
			if (psCurr->pViewData == psViewData)
			{
				removeMessage(psCurr, i);
			}
		}
	}
}

//add proximity messages for all untapped VISIBLE oil resources
void addOilResourceProximities(void)
{
    FEATURE     *psFeat;
    MESSAGE     *psMessage;

    //look thru the features to find oil resources
    for (psFeat = apsFeatureLists[0]; psFeat != NULL; psFeat = psFeat->psNext)
    {
        if (psFeat->psStats->subType == FEAT_OIL_RESOURCE)
        {
            //check to see if the feature is visible to the selected player
            if (psFeat->visible[selectedPlayer])
            {
                //if there isn't an oil derrick built on it
				if(!TILE_HAS_STRUCTURE(mapTile(psFeat->x >> TILE_SHIFT,
					psFeat->y >> TILE_SHIFT)))
				{
                    //add a proximity message
					psMessage = addMessage(MSG_PROXIMITY, TRUE, selectedPlayer);
					if (psMessage)
					{
						psMessage->pViewData = (MSG_VIEWDATA *)psFeat;
					}
                }
            }
        }
    }
}









