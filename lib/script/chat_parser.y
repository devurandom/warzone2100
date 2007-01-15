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
%{
/*
 * chat_parser.y
 *
 * yacc grammar for multiplayer chat messages
 *
 */

#include <stdio.h>
#include <string.h>

#include "lib/framework/frame.h"
#include "lib/framework/frameresource.h"

#include "lib/script/chat_processing.h"

#define MAX_CHAT_ARGUMENTS 10

/* Holds information about a processed player chat message */
CHAT_MSG chat_msg;

// The current script code
static SCRIPT_CODE *psCurrScript;

// The current script context
static SCRIPT_CONTEXT	*psCurrContext;

// Parameter variable
static INTERP_VAL scrParameter;

extern int chat_lex(void);

// Store extracted command for use in scripts
static void chat_store_command(char *command);

/* Return value of the chat parsing - to be used in scripts */
static BOOL chat_store_parameter(INTERP_VAL *parameter);

// Store players that were addressed in a command
static void chat_store_player(SDWORD cmdIndex, SDWORD playerIndex);

// Reset a command
static void chat_reset_command(SDWORD cmdIndex);

// Information extracted from message and available for scripts
//static int					numMsgParams=0;		//number of parameters currently extracted/stored
//static INTERP_VAL		msgParams[MAX_CHAT_ARGUMENTS];

/* Store command parameter extracted from the message */
static BOOL chat_store_parameter(INTERP_VAL *cmdParam)
{
	SDWORD	numCmdParams, numCommands;

	//console("chat_store_parameter: new parameter");

	/* Make sure we have no overflow */
	//if(numMsgParams >= MAX_CHAT_ARGUMENTS)
	if(chat_msg.numCommands >= MAX_CHAT_COMMANDS)
	{
		ASSERT(FALSE, "chat_store_parameter: too many commands in a message");
		return FALSE;
	}

	numCommands = chat_msg.numCommands;
	numCmdParams = chat_msg.cmdData[numCommands].numCmdParams;

	/* Make sure we still have room for more parameters */
	if(numCmdParams >= MAX_CHAT_CMD_PARAMS)
	{
		ASSERT(FALSE, "chat_store_parameter: out of parameters for command %d", numCommands);
		return FALSE;
	}

	/* Store parameter for command we are currently processing */
	//memcpy(&(msgParams[numMsgParams]), parameter, sizeof(INTERP_VAL));
	memcpy(&(chat_msg.cmdData[numCommands].parameter[numCmdParams]), cmdParam, sizeof(INTERP_VAL));

	chat_msg.cmdData[numCommands].numCmdParams++;

	return TRUE;
}

// Store extracted command for use in scripts
static void chat_store_command(char *command)
{
	SDWORD	numCmdParams, numCommands;

	//console("chat_store_command: new command: %s", command);

	/* Make sure we have no overflow */
	if(chat_msg.numCommands >= MAX_CHAT_COMMANDS)
	{
		ASSERT(FALSE, "chat_store_command: too many commands in a message");
		return;
	}

	numCommands = chat_msg.numCommands;
	numCmdParams = chat_msg.cmdData[numCommands].numCmdParams;

	/* Make sure we still have room for more parameters */
	if(numCmdParams >= MAX_CHAT_CMD_PARAMS)
	{
		ASSERT(FALSE, "chat_store_command: out of parameters for command %d", numCommands);
		return;
	}

	/* Store command */
	chat_msg.cmdData[numCommands].pCmdDescription = command;

	chat_msg.numCommands++;
}

// Store players that were addressed in a command
static void chat_store_player(SDWORD cmdIndex, SDWORD playerIndex)
{
	SDWORD i;

	//console("chat_store_player: player %d addressd in command %d", playerIndex, cmdIndex);

	/* Make sure we have no overflow */
	if(cmdIndex < 0 || cmdIndex >= MAX_CHAT_COMMANDS)
	{
		ASSERT(FALSE, "chat_store_player: command message out of bounds: %d", cmdIndex);
		return;
	}

	if(playerIndex == -1)	//no player specified means all players addressed
	{
		/* Ally players addressed */
		for(i=0; i<MAX_PLAYERS; i++)
		{
			chat_msg.cmdData[cmdIndex].bPlayerAddressed[i] = TRUE;
		}
	}
	else if(playerIndex >= 0 && playerIndex < MAX_PLAYERS)
	{
		chat_msg.cmdData[cmdIndex].bPlayerAddressed[playerIndex] = TRUE;
	}
	else	/* Wrong player index */
	{
		ASSERT(FALSE, "chat_store_player: wrong player index: %d", playerIndex);
		return;
	}
}

static void chat_reset_command(SDWORD cmdIndex)
{
	SDWORD i;

	ASSERT(cmdIndex >= 0 && cmdIndex < MAX_CHAT_COMMANDS,
		"chat_reset_command: command index out of bounds: %d", cmdIndex);

	chat_msg.cmdData[cmdIndex].numCmdParams = 0;

	for(i=0; i<MAX_PLAYERS; i++)
	{
		chat_msg.cmdData[cmdIndex].bPlayerAddressed[i] = FALSE;
	}
}

%}

%name-prefix="chat_"

%union {
	BOOL				bval;
	INTERP_TYPE		tval;
	char				*sval;
	UDWORD			vindex;
	SDWORD			ival;
}

	/* Start symbol */
%start R_PHRASE

	/* keywords */
%token BRACKET_OPEN
%token BRACKET_CLOSE
%token SQ_BRACKET_OPEN
%token SQ_BRACKET_CLOSE
%token PIPE
%token T_WORD

%token _T_QM
%token _T_EM
%token _T_FULLSTOP
%token _T_COLON
%token _T_SEMICOLON
%token _T_COMMA

%token _T_A
%token _T_AFFIRMATIVE
%token _T_AFTER
%token _T_ALLY
%token _T_AM
%token _T_AND
%token _T_ANY
%token _T_ATTACK
%token _T_ATTACKING
%token _T_BEACON
%token _T_BUILDING
%token _T_CANT
%token _T_CENTER
%token _T_DEAD
%token _T_DERRICK
%token _T_DO
%token _T_DROP
%token _T_FINE
%token _T_GET
%token _T_GETTING
%token _T_GO
%token _T_GOING
%token _T_GONNA
%token _T_GOT
%token _T_GREAT
%token _T_HAVE
%token _T_HAS
%token _T_HELP
%token _T_I
%token _T_IM
%token _T_A
%token _T_IS
%token _T_LETS
%token _T_ME
%token _T_NO
%token _T_NOW
%token _T_OFCOURSE
%token _T_OK
%token _T_PLACE
%token _T_POSSESSION
%token _T_POWER
%token _T_PUMPING
%token _T_PUT
%token _T_ROGER
%token _T_SEE
%token _T_SOME
%token _T_STATUS
%token _T_STOP
%token _T_SURE
%token _T_THANK_YOU
%token _T_THANKS
%token _T_U
%token _T_UNITS
%token _T_VTOLS
%token _T_WAIT
%token _T_WHERE
%token _T_YEA
%token _T_YEAH
%token _T_YES
%token _T_YOU
%token _T_EOF 0

/* Typed Keywords */
%token <ival> R_PLAYER
%token <ival> R_INTEGER

%%

/* The top-level rule, consists of
 * one or more sentences: commands or addressed commands
 * or unrecognized messages (handled in R_COMMAND)
 */
R_PHRASE:								R_ADDRESSED_COMMAND
									|	R_COMMAND
										{
											chat_store_player(chat_msg.numCommands - 1, -1);	/* No player list means current command is addressed to everyone */
										}
									|	R_PHRASE R_ADDRESSED_COMMAND
									|	R_PHRASE R_COMMAND
										{
											chat_store_player(chat_msg.numCommands - 1, -1);
										}
									|	error
										{
											//console("-unrecognized phrase");

											/* Reset current command */
											chat_reset_command(chat_msg.numCommands);
										}
									;

/* A command addressed to one or more players
 */
R_ADDRESSED_COMMAND:					R_PLAYER_LIST _T_COLON R_COMMAND			/* "Red, Black: attack yellow" */
											//{console("RULE: player list: command");}
									|	R_PLAYER_LIST _T_SEMICOLON R_COMMAND			/* "Red, Black; attack yellow" */
											//{console("RULE: player list; command");}
									|	R_PLAYER_LIST _T_COMMA R_COMMAND			/* "Red, Black, attack yellow" */
											//{console("RULE: player list, command");}
									|	R_PLAYER_LIST R_COMMAND						/* No delimeter: "Red, Black attack yellow" */
											//{console("RULE: player list command");}
									;


/* Any of the recognizable commands -
 * since there can't be any player list at this stage,
 * should address all players receiving this message by default:
 * EXAMPLE:
 * 1) "Help me!" - addresses all players receiving the message
 * 2) "Black and Red, help me!" - addresses players 'Black' and 'red' only
 */
R_COMMAND:						R_ALLY_OFFER					/* ally me */
									{
										chat_store_command("ally me");		/* Store current command */
									}
								|	R_ASK_READINESS				/* go? */
									{
										chat_store_command("go?");
									}
								|	R_INITIATE_ACTION			/* go ! */
									{
										chat_store_command("go!");
									}
								|	R_DEMAND_BEACON				/* drop a beacon */
									{
										chat_store_command("drop a beacon");
									}
								|	R_MEET_CENTER					/* go center */
									{
										chat_store_command("go center");
									}
								|	R_ASK_STATUS					/* status? */
									{
										chat_store_command("status?");
									}
								|	R_BUILDING_UNITS				/* pumping units */
									{
										chat_store_command("pumping units");
									}
								|	R_STOP							/* stop! */
									{
										chat_store_command("stop");
									}
								|	R_WONDER_IF_HAVE_POWER		/* Ask if player has power */
									{
										chat_store_command("got power?");
									}
								|	R_DEMAND_HELP					/* help me! */
									{
										chat_store_command("help me");
									}
								|	R_REPORT_SAFETY				/* i'm ok */
									{
										chat_store_command("i'm ok");
									}
								|	R_AFFIRMATIVE					/* roger! */
									{
										chat_store_command("roger");
									}
								|	R_ALLY_PLAYER					/* ally blue */
									{
										chat_store_command("ally player");
									}
								|	R_ATTACK_PLAYER				/* attack blue */
									{
										chat_store_command("attack player");
									}
								|	R_ATTACKING_PLAYER			/* attacking blue */
									{
										chat_store_command("attacking player?");
									}
								|	R_PLAYER_HAS_VTOLS			/* blue has VTOLS */
									{
										chat_store_command("player has vtols");
									}
								;

/* A reference to certain players: "yellow, help me"
 */
R_PLAYER_LIST:					R_PLAYER
									{
										chat_store_player(chat_msg.numCommands, $1);		/* Remember this player was addressed in current command */
									}
								|	R_PLAYER_LIST R_PLAYER			/* without any delimeters: "yellow black red" */
									{
										chat_store_player(chat_msg.numCommands, $2);
									}
								|	R_PLAYER_LIST _T_COMMA R_PLAYER			/* "yellow, black, red" */
									{
										chat_store_player(chat_msg.numCommands, $3);
									}
								|	R_PLAYER_LIST _T_AND R_PLAYER			/* "yellow, black and red" */
									{
										chat_store_player(chat_msg.numCommands, $3);
									}
								;


/* A Article or nothing
 */
R_A_OR_EMPTY:					/* Empty */
								|	_T_A
								;

/* Punctuation mark */
R_PUNCTUATION_MARK:					_T_QM
								|	_T_EM
								|	R_PUNCTUATION_MARK _T_QM	/* ?????!!!!??! */
								|	R_PUNCTUATION_MARK _T_EM	/* ?????!!!!??! */
								;

/* Full stop or end of file
 * (a 'virtual' rule)
 */
R_FULLSTOP_OR_EOF:					/* Empty */				/* Needed for other rules */
								|	_T_EOF					/* End of input */
								|	_T_FULLSTOP			/* . */
								;

/* End of a declarative sentence
 * can't be a question
 */
R_EOD:								_T_EM								/* !!!! */
								|	R_FULLSTOP_OR_EOF
								;

/* End of a question
 * can't end with an exclamation mark
 */
R_EOQ:								_T_QM
								|	R_FULLSTOP_OR_EOF
								;

/* (any possible) end of the sentence - similar
 * to "End of a declarative sentence",
 * but can additionally end with a question.
 */
R_EOS:								R_EOD				/* End of a declarative sentence */
								|	R_PUNCTUATION_MARK		/* ???!!?!?!?!? */
								;

/* Express attack intention */
R_ATTACKING:						_T_ATTACKING
								|	_T_GOING
								|	_T_GOING _T_AFTER
								;

/* Attack */
R_INITIATE_ATTACK:					_T_ATTACK
								|	_T_GET
								;

R_PUT_DOWN:						_T_PUT | _T_DROP | _T_PLACE;	/* put */

R_INCREASING_NUMBER:				_T_PUMPING						/* pumping/getting */
								|	_T_GETTING
								|	_T_BUILDING
								;

R_YOU:											_T_YOU
											|	_T_U						/* u */
											;

/* 'You' - pronoun for questions */
R_DO_YOU:										R_YOU						/* like in "you got any..." */
											|	_T_DO R_YOU				/* do you */
											;

/* Used in questions */
R_POSSESSION_Q:									_T_HAVE
											|	_T_GOT
											;

R_POSSESSES:									_T_HAS
											|	_T_GOT
											;


R_QUANTITY:										_T_ANY
											|	_T_SOME
											;

R_DO_YOU_HAVE_ANY:								R_DO_YOU R_POSSESSION_Q R_QUANTITY
											|	R_DO_YOU R_POSSESSION_Q
											|	R_POSSESSION_Q R_QUANTITY						/* got any.. */
											|	R_POSSESSION_Q									/* got <substantive>? */
											|	R_QUANTITY											/* any <substantive>? */
											;

R_YES_FORMS:									_T_YES
											|	_T_YEA
											|	_T_YEAH
											;

R_CONFIDENCE_EXPRESSION:						_T_SURE
											|	_T_OFCOURSE
											;

R_AGREEMENT_EXPRESSION:							R_YES_FORMS
											|	_T_FINE
											|	_T_OK
											;

R_AFFIRMATIVE_FORMS:							R_AGREEMENT_EXPRESSION
											|	R_CONFIDENCE_EXPRESSION
											|	_T_ROGER								/* roger */
											|	_T_AFFIRMATIVE
											;
					/*******************************************/
					/* FINAL RULES, SHOULD BE PART OF R_PHRASE */
					/*******************************************/

/* Ask a player to ally myself */
R_ALLY_OFFER:									_T_ALLY _T_ME R_EOS			/* ally me */
											|	_T_ALLY 	R_EOS				/* "ally" at the end (otherwise breaks "ally me") */
										/*	|	_T_ALLY */
											;

/* Chech if player is willing
 * to initiate some actions
 */
R_ASK_READINESS:							_T_GO _T_QM;		/* go? */

/* Tell to start some action */
R_INITIATE_ACTION:							_T_GO R_EOD;			/* go!! */

/* Tell to drop a beacon */
R_DEMAND_BEACON:							R_PUT_DOWN R_A_OR_EMPTY _T_BEACON;		/* put a beacon */

/* Tell to meet in the center of the map */
R_MEET_CENTER:								_T_GO _T_CENTER R_EOS;		/* go center */

/* Ask for the current status */
R_ASK_STATUS:								_T_STATUS R_EOS;				/* status? */

/* Player is building units */
R_BUILDING_UNITS:							R_INCREASING_NUMBER _T_UNITS R_EOD;		/* pumping units */

/* Stop command */
R_STOP:										_T_STOP R_EOD;			/* stop */

/* Ask if player has power */
R_WONDER_IF_HAVE_POWER:						R_DO_YOU_HAVE_ANY _T_POWER R_EOQ;		/* do you have power? */

/* Ask for help */
R_DEMAND_HELP:									_T_HELP _T_ME R_EOS		/* help me!!!!! */
											|	_T_HELP R_EOS				/* help!?!? */
											;

R_GRATITUDE:									_T_THANK_YOU
											|	_T_THANKS
											;

/* Tell player i'm safe - no danger anymore */
R_REPORT_SAFETY:								_T_IM _T_OK	R_EOD		/* i'm ok */
											|	_T_IM _T_FINE R_EOD
											|	R_REPORT_SAFETY _T_NOW R_EOD
											|	R_REPORT_SAFETY R_GRATITUDE R_EOD
											|	R_REPORT_SAFETY _T_COMMA R_GRATITUDE R_EOD
											|	R_GRATITUDE R_REPORT_SAFETY R_EOD
											|	R_GRATITUDE _T_COMMA R_REPORT_SAFETY R_EOD
											|	R_GRATITUDE R_GRATITUDE R_EOD
											;


/* Positive order feedback */
R_AFFIRMATIVE:									R_AFFIRMATIVE_FORMS R_EOD		/* yes, roger etc */
											|	R_AFFIRMATIVE R_AFFIRMATIVE_FORMS R_EOD		/* accept repetitions: "yes yes yeah" */
											// |	R_AFFIRMATIVE 						/* can also be emotional: "yea!!! yeah! yes!!!!" */
											;

/* Ask to ally a player */
R_ALLY_PLAYER:								_T_ALLY R_PLAYER R_EOD		/* ally blue */
											{
												/* Store for scripts */
												scrParameter.type = VAL_INT;
												scrParameter.v.ival = $2;
												if(!chat_store_parameter(&scrParameter))
												{
													chat_error("chat command: Too many parameters in the message");
												}
											}
											;

/* Ask to start attack a player */
R_ATTACK_PLAYER:							R_INITIATE_ATTACK R_PLAYER R_EOD		/* attack blue */
												{
													/* Store for scripts */
													scrParameter.type = VAL_INT;
													scrParameter.v.ival = $2;
													if(!chat_store_parameter(&scrParameter))
													{
														chat_error("chat command: Too many parameters in the message");
													}
												}
											|	_T_GO R_PLAYER R_EOD					/* go blue */
												{
													/* Store for scripts */
													scrParameter.type = VAL_INT;
													scrParameter.v.ival = $2;
													if(!chat_store_parameter(&scrParameter))
													{
														chat_error("chat command: Too many parameters in the message");
													}
												}
											;

/* Notify i'm attacking player */
R_ATTACKING_PLAYER:							R_ATTACKING R_PLAYER R_EOD				/* attacking blue */
												{
													/* Store for scripts */
													scrParameter.type = VAL_INT;
													scrParameter.v.ival = $2;
													if(!chat_store_parameter(&scrParameter))
													{
														chat_error("chat command: Too many parameters in the message");
													}
												}
											;

/* Notify about player using VTOLs */
R_PLAYER_HAS_VTOLS:							R_PLAYER R_POSSESSES _T_VTOLS R_EOD	/* blue has VTOLS */
												{
													/* Store for scripts */
													scrParameter.type = VAL_INT;
													scrParameter.v.ival = $1;
													if(!chat_store_parameter(&scrParameter))
													{
														chat_error("chat command: Too many parameters in the message");
													}
												}
											;
%%

/* Initialize Bison and start chat processing */
BOOL chatLoad(char *pData, UDWORD size)
{
	SDWORD	cmdIndex,parseResult;

	/* Don't parse the same message again for a different player */
	if(strcmp(pData, &(chat_msg.lastMessage[0])) == 0)	//just parsed this message for some other player
	{
		return TRUE;			//keep all the parsed data unmodified
	}

	/* Tell bison what to parse */
	chatSetInputBuffer(pData, size);

	/* Reset previous chat processing */
	chat_msg.numCommands = 0;

	/* Initialize command data */
	for(cmdIndex=0; cmdIndex<MAX_CHAT_COMMANDS; cmdIndex++)
	{
		chat_reset_command(cmdIndex);
	}

	/* Invoke bison and parse */
	parseResult = chat_parse();

	/* Remember last message we parsed */
	strcpy(&(chat_msg.lastMessage[0]), pData);

	/* See if we were successfull parsing */
	if (parseResult != 0)
	{
		return FALSE;
	}

	return TRUE;
}

/* A simple error reporting routine */
void chat_error(const char *pMessage,...)
{
	int		line;
	char	*pText;
	char	aTxtBuf[1024];
	va_list	args;

	va_start(args, pMessage);

	vsprintf(aTxtBuf, pMessage, args);
	chatGetErrorData(&line, &pText);
	//debug(LOG_WARNING, "multiplayer message parse error: %s at line %d, token: %d, text: '%s'",
	//      aTxtBuf, line, chat_char, pText);

	va_end(args);
}
