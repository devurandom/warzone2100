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
 * SeqDisp.h
 *
 * Functions for the display of the Escape Sequences
 */

#ifndef _SeqDisp_h
#define _SeqDisp_h

			// ffs js (bastard)
#include "lib/ivis_common/piemode.h"

/***************************************************************************/
/*
 *	Global Definitions
 */
/***************************************************************************/

#define  SEQUENCE_PLAY 0//play once and exit
#define  SEQUENCE_LOOP 1//loop till stopped externally
#define  SEQUENCE_PAUSE 2//pause time
#define  SEQUENCE_KILL 3//stop
#define  SEQUENCE_HOLD 4//play once and hold last frame

#define  SEQ_TEXT_POSITION  0//position text
#define  SEQ_TEXT_FOLLOW_ON 1//justify if less than 3/4 length
#define  SEQ_TEXT_JUSTIFY   2//justify if less than 520/600

/***************************************************************************/
/*
 *	Global Variables
 */
/***************************************************************************/

/***************************************************************************/
/*
 *	Global ProtoTypes
 */
/***************************************************************************/
//buffer render
extern BOOL	seq_RenderVideoToBuffer(iSurface *pSurface, char* sequenceName, int time, int seqCommand);
extern BOOL	seq_BlitBufferToScreen(char* screen, SDWORD screenStride, SDWORD xOffset, SDWORD yOffset);

//full screen render
//extern BOOL seq_PlayVideo(char* pSeq, char* pAudio);

extern BOOL seq_UpdateFullScreenVideo(int *bClear);

extern BOOL seq_StopFullScreenVideo(void);
//control
extern BOOL	seq_SetupVideoBuffers(void);
extern BOOL	seq_ReleaseVideoBuffers(void);
extern BOOL seq_GetVideoSize(SDWORD* pWidth, SDWORD* pHeight);
//text
extern BOOL seq_AddTextForVideo(char* pText, SDWORD xOffset, SDWORD yOffset, SDWORD startTime, SDWORD endTime, SDWORD bJustify);
extern BOOL seq_ClearTextForVideo(void);
//clear the sequence list
extern void seq_ClearSeqList(void);
//add a sequence to the list to be played
extern void seq_AddSeqToList(const char *pSeqName, const char *pAudioName, const char *pTextName, BOOL bLoop);
/*checks to see if there are any sequences left in the list to play*/
extern BOOL seq_AnySeqLeft(void);

//set and check subtitle mode, TRUE subtitles on
extern void seq_SetSubtitles(BOOL bNewState);
extern BOOL seq_GetSubtitles(void);




/*returns the next sequence in the list to play*/
extern void seq_StartNextFullScreenVideo(void);



#endif	//SeqDisp.h


