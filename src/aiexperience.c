//aiexperience.c
#include "objmem.h"
#include "objectdef.h"

#include "map.h"
#include "lib/framework/frame.h"
#include "lib/script/interp.h"
#include "lib/script/stack.h"
#include "lib/script/codeprint.h"
#include "lib/script/script.h"
#include "multiplay.h"

#include "console.h"		//for RIGHT_JUSTIFY
#include "geometry.h"

#include "aiexperience.h"

SDWORD baseLocation[MAX_PLAYERS][MAX_PLAYERS][2];		//each player's visible enemy base x,y coords for each player (hence 3d)
SDWORD oilLocation[MAX_PLAYERS][MAX_OIL_LOCATIONS][2];	//remembered oil locations

SDWORD baseDefendLocation[MAX_PLAYERS][MAX_BASE_DEFEND_LOCATIONS][2];
SDWORD baseDefendLocPrior[MAX_PLAYERS][MAX_BASE_DEFEND_LOCATIONS];		//Priority

SDWORD oilDefendLocation[MAX_PLAYERS][MAX_OIL_DEFEND_LOCATIONS][2];
SDWORD oilDefendLocPrior[MAX_PLAYERS][MAX_OIL_DEFEND_LOCATIONS];


void InitializeAIExperience()
{
	SDWORD i,j;

	for(i=0;i<MAX_PLAYERS;i++)
	{
		for(j=0;j<MAX_PLAYERS;j++)
		{
			baseLocation[i][j][0] = -1;
			baseLocation[i][j][1] = -1;
		}

		for(j=0; j < MAX_BASE_DEFEND_LOCATIONS; j++)
		{
			baseDefendLocation[i][j][0] = -1;
			baseDefendLocation[i][j][1] = -1;

			baseDefendLocPrior[i][j] = -1;
		}

		for(j=0; j < MAX_OIL_DEFEND_LOCATIONS; j++)
		{
			oilDefendLocation[i][j][0] = -1;
			oilDefendLocation[i][j][1] = -1;

			oilDefendLocPrior[i][j] = -1;
		}

		//oil locations
		for(j=0; j < MAX_OIL_LOCATIONS; j++)
		{
			oilLocation[i][j][0] = -1;
			oilLocation[i][j][1] = -1;
		}
	}
}

BOOL LoadAIExperience(BOOL bNotify)
{
	SDWORD i;
	for(i=0;i<game.maxPlayers;i++)
	{
		LoadPlayerAIExperience(i, bNotify);
	}

	return TRUE;
}

BOOL SaveAIExperience(BOOL bNotify)
{
	SDWORD i;
	for(i=0;i<game.maxPlayers;i++)
	{
		(void)SavePlayerAIExperience(i, bNotify);
	}

	return TRUE;
}

BOOL LoadPlayerAIExperience(SDWORD nPlayer, BOOL bNotify)
{
	if((nPlayer > -1) && (nPlayer < MAX_PLAYERS))
	{
		if(ReadAISaveData(nPlayer))
		{
			//addConsoleMessage("Experience loaded successfully.",RIGHT_JUSTIFY);

			if(bNotify)
				console("Experience for player %d loaded successfully.", nPlayer);

			return TRUE;
		}
	}

	//addConsoleMessage("Failed to load experience (no experience saved?).",RIGHT_JUSTIFY);
	console("Failed to load experience for player %d (no experience saved?).",nPlayer);
	return FALSE;
}

BOOL SavePlayerAIExperience(SDWORD nPlayer, BOOL bNotify)
{
	if((nPlayer > -1) && (nPlayer < MAX_PLAYERS))
	{
		if(!WriteAISaveData(nPlayer))
		{
			debug(LOG_ERROR,"SavePlayerAIExperience - failed to save exper");

			//addConsoleMessage("Failed to save experience.",RIGHT_JUSTIFY);
			console("Failed to save experience for player %d.", nPlayer);
/*
			res = MessageBox(frameGetWinHandle(), "WriteAISaveData failed, delete this experience file?", "Confirmation",
							 MB_ICONQUESTION | MB_YESNO);
			if (res == IDYES)
			{
				//ToDo: delete
				//winQuit = TRUE;
			}
*/
			return FALSE;
		}
		//else
		//{
		//	addConsoleMessage("Experience saved successfully.",RIGHT_JUSTIFY);
		//}
			
	}

	//addConsoleMessage("Experience saved successfully.",RIGHT_JUSTIFY);

	if(bNotify)
		console("Experience for player %d saved successfully.", nPlayer);

	return TRUE;
}

BOOL SetUpOutputFile(STRING * pMapName,SDWORD nPlayer)
{
	STRING			sPlayer[255] = "";
	STRING			SaveDir[MAX_PATH] = "";		//"multiplay\\LearnData\\";
	STRING			FileName[255] = "";

	//debug(LOG_ERROR,"SetUpOutputFile");

	//strcpy( SaveDir, PHYSFS_getUserDir() );
	//strcat( SaveDir, WZ_WRITEDIR );				//TODO: fix it
	//strcat( SaveDir, PHYSFS_getDirSeparator() );

	strcpy( SaveDir, "multiplay/learndata/" );

	//strcat( SaveDir, "multiplay" );
	//strcat( SaveDir, PHYSFS_getDirSeparator() );
	//strcat( SaveDir, "learndata" );
	//strcat( SaveDir, PHYSFS_getDirSeparator() );

	/* Assemble dir string */
	sprintf(sPlayer,"%d",nPlayer);

	strcat( SaveDir, "player");	
	strcat( SaveDir, sPlayer );
	//strcat( SaveDir, "/" );	//like "multiplay\learndata\player0\"

	/* Create dir on disk */
	if ( !PHYSFS_mkdir(SaveDir))
	{
		debug( LOG_ERROR, "SetUpOutputFile: Error creating directory \"%s\": %s", SaveDir, PHYSFS_getLastError() );
		return FALSE;
	}

	strcat( SaveDir, "/" );

	/* Create filename */
	strcpy(FileName,SaveDir);
	strcat(FileName,game.map);		//Map name
	strcat(FileName,".lrn");		//Like "multiplay\learndata\player0\Rush.lrn"

	/* Open file */
	aiSaveFile[nPlayer] = NULL;
	aiSaveFile[nPlayer] = PHYSFS_openWrite(FileName);	//fopen(FileName, "wb");	//new write
	if (!aiSaveFile[nPlayer])
	{
		debug(LOG_ERROR,"SetUpOutputFile(): Couldn't open debugging output file: '%s' for player %d", FileName,nPlayer);
		return FALSE;
	}

	return TRUE;
}

BOOL SetUpInputFile(SDWORD nPlayer)
{
	STRING			FileName[255] = "";
	STRING			sPlayer[255] = "";
	STRING			SaveDir[MAX_PATH] = "";		// "multiplay\\learndata\\";

	/* assemble "multiplay\learndata\" */
	strcat( SaveDir, "multiplay/learndata/" );

	/* Assemble dir */
	sprintf(sPlayer,"%d",nPlayer);

	strcat(SaveDir,"player");
	strcat(SaveDir,sPlayer);
	strcat(SaveDir,"/");	//like "multiplay\learndata\player0\"

	/* Create filename */
	strcpy(FileName,SaveDir);
	strcat(FileName,game.map);		//map name
	strcat(FileName,".lrn");		//Like "multiplay\learndata\player0\Rush.lrn"

	aiSaveFile[nPlayer] = NULL;
	aiSaveFile[nPlayer] = PHYSFS_openRead(FileName);
	if (!aiSaveFile[nPlayer])
	{
		debug(LOG_ERROR,"SetUpInputFile(): Couldn't open input file: '%s' for player %d:\n%s", FileName, nPlayer, PHYSFS_getLastError());
		return FALSE;
	}

	return TRUE;
}


BOOL ExperienceRecallOil(SDWORD nPlayer)
{
	FEATURE					*psFeature;
return TRUE;

	/* Make visible all oil derricks */
	for(psFeature = apsFeatureLists[0]; psFeature != NULL; psFeature = psFeature->psNext)
	{
		if(psFeature->psStats->subType == FEAT_OIL_RESOURCE)
		{

			printf_console("Enabling feature at x: %d y: %d",psFeature->x/128,psFeature->y/128);

			psFeature->visible[nPlayer] = TRUE;
		}
	}
}

BOOL WriteAISaveData(SDWORD nPlayer)
{

	FEATURE					*psFeature;
	STRUCTURE				*psCurr;
	SDWORD					x=0,y=0;
	SDWORD					NumEntries=0;	//How many derricks/oil resources will be saved
	UDWORD					PosXY[MAX_OIL_ENTRIES];		//Locations, 0=x,1=y,2=x etc
	SDWORD						i;

	/* Prepair experience file for the current map */
	if(!SetUpOutputFile(game.map,nPlayer))
	{
		debug(LOG_ERROR,"Failed to prepare experience file for player %d",nPlayer);
		return FALSE;
	}

	if (aiSaveFile[nPlayer])
	{
		//debug(LOG_ERROR,"WriteAISaveData - aiSaveFile ok");


		/* Version */
		NumEntries = 1;		//Version
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1) != 1)
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write version for player %d",nPlayer);
			return FALSE;
		}

		//fwrite(&NumEntries,sizeof(NumEntries),1,aiSaveFile[nPlayer]);	//Version

		//debug(LOG_ERROR,"WriteAISaveData - Version ok");

		/************************/
		/*		Enemy bases		*/
		/************************/
		NumEntries = MAX_PLAYERS;

		/* max num of players to store */
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1) != 1)			//num of players to store
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write MAX_PLAYERS for player %d",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"WriteAISaveData - MAX_PLAYERS ok");

		/* base locations of all players */
		if(PHYSFS_write(aiSaveFile[nPlayer], baseLocation[nPlayer], sizeof(SDWORD), MAX_PLAYERS * 2) != (MAX_PLAYERS * 2))			//num of players to store
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write base locations for player %d",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"WriteAISaveData - Enemy bases ok");

		/************************************/
		/*		Base attack locations		*/
		/************************************/
		NumEntries = MAX_BASE_DEFEND_LOCATIONS;

		/* write MAX_BASE_DEFEND_LOCATIONS */
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(SDWORD), 1) < 1)
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write defence locations count for player %d",nPlayer);
			return FALSE;
		}

		/* base defence locations */
		if(PHYSFS_write(aiSaveFile[nPlayer], baseDefendLocation[nPlayer], sizeof(SDWORD), MAX_BASE_DEFEND_LOCATIONS * 2) < (MAX_BASE_DEFEND_LOCATIONS * 2))
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write defence locations for player %d",nPlayer);
			return FALSE;
		}

		/* base defend priorities */
		if(PHYSFS_write(aiSaveFile[nPlayer], baseDefendLocPrior[nPlayer], sizeof(SDWORD), MAX_BASE_DEFEND_LOCATIONS * 2) < (MAX_BASE_DEFEND_LOCATIONS * 2))
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write defence locations priority for player %d",nPlayer);
			return FALSE;
		}
		
		//debug(LOG_ERROR,"WriteAISaveData - Base attack locations ok");

		/************************************/
		/*		Oil attack locations		*/
		/************************************/
		NumEntries = MAX_OIL_DEFEND_LOCATIONS;

		/* MAX_OIL_DEFEND_LOCATIONS */
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(SDWORD), 1) < 1)
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write oil defence locations count for player %d",nPlayer);
			return FALSE;
		}

		/* oil locations */
		if(PHYSFS_write(aiSaveFile[nPlayer], oilDefendLocation[nPlayer], sizeof(SDWORD), MAX_OIL_DEFEND_LOCATIONS * 2) < (MAX_OIL_DEFEND_LOCATIONS * 2))
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write oil defence locations for player %d",nPlayer);
			return FALSE;
		}

		/* oil location priority */
		if(PHYSFS_write(aiSaveFile[nPlayer], oilDefendLocPrior[nPlayer], sizeof(SDWORD), MAX_OIL_DEFEND_LOCATIONS * 2) < (MAX_OIL_DEFEND_LOCATIONS * 2))
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write oil defence locations priority for player %d",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"WriteAISaveData - Oil attack locations ok");
		

		/****************************/
		/*		Oil Resources		*/
		/****************************/
		NumEntries = MAX_OIL_LOCATIONS;

		/* MAX_OIL_LOCATIONS */
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1) < 1)
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write oil locations count for player %d",nPlayer);
			return FALSE;
		}

		NumEntries = 0;		//Num of oil resources

		/* first save everything what we recalled from last load */
		for(i=0; i<MAX_OIL_LOCATIONS; i++)
		{
			if(oilLocation[nPlayer][i][0] <= 0 || oilLocation[nPlayer][i][1] <= 0)
				continue;		//skip to next one, since nothing stored

			PosXY[NumEntries * 2] = oilLocation[nPlayer][i][0];		//x
			PosXY[NumEntries * 2 + 1] = oilLocation[nPlayer][i][1];	//y

			NumEntries++;
		}
		
		/* now remember new ones that are not in memory yet (discovered this time) */
		for(psFeature = apsFeatureLists[0]; psFeature != NULL; psFeature = psFeature->psNext)
		{
			if(psFeature->psStats->subType == FEAT_OIL_RESOURCE)
			{
				if (psFeature->visible[nPlayer])	//OR godMode)
				{
					if(!canRecallOilAt(nPlayer, psFeature->x, psFeature->y))	//already stored?
					{
						/* Save X */
						PosXY[NumEntries * 2] = psFeature->x;

						/* Save Y */
						PosXY[NumEntries * 2 + 1] = psFeature->y;

						//printf_console("New oil visible x: %d y: %d. Storing.", PosXY[NumEntries * 2]/128,PosXY[NumEntries * 2 + 1]/128);

						NumEntries++;
					}
				}
			}
		}

		//Save Derricks as oil resources, since most of them will be unoccupied when experiance will be loaded
		for(i=0;i<MAX_PLAYERS;i=i+1)
		{
			for (psCurr = apsStructLists[i]; psCurr != NULL; psCurr = psCurr->psNext)
			{
				if (psCurr->pStructureType->type == REF_RESOURCE_EXTRACTOR)
				{
					if(psCurr->visible[nPlayer])	//if can see it
					{
						if(!canRecallOilAt(nPlayer, psCurr->x, psCurr->y))	//already stored?
						{
							//psResExtractor = (RES_EXTRACTOR *)psCurr->pFunctionality;

							x = psCurr->x;
							y = psCurr->y;

							//printf_console("Found derrick at x: %d, y: %d,, width: %d",psCurr->x/128,psCurr->y/128,mapWidth);
						
							// Save X //
							PosXY[NumEntries * 2] = psCurr->x;

							// Save Y //
							PosXY[NumEntries * 2 + 1] = psCurr->y;

							//printf_console("New derrick visible x: %d y: %d. Storing.", PosXY[NumEntries * 2]/128,PosXY[NumEntries * 2 + 1]/128);

							NumEntries++;
						}
					}
				}
			}
		}
		

		/* Write number of Oil Resources */
		if(PHYSFS_write(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1) < 1)
		{
			debug(LOG_ERROR,"WriteAISaveData: failed to write stored oil locations count for player %d",nPlayer);
			return FALSE;
		}
		
		//printf_console("Num Oil Resources: %d ****",NumEntries);

		/* Write Oil Resources coords */
		if(NumEntries > 0)		//Anything to save
		{
			if(PHYSFS_write(aiSaveFile[nPlayer], PosXY, sizeof(UDWORD), NumEntries * 2) < (NumEntries * 2))
			{
				debug(LOG_ERROR,"WriteAISaveData: failed to write oil locations fir player %d",nPlayer);
				return FALSE;
			}
		}
	}
	else
	{
		debug(LOG_ERROR,"WriteAISaveData(): no output file for player %d",nPlayer);
		return FALSE;
	}

	//printf_console("AI settings file written for player %d",nPlayer);

	return PHYSFS_close(aiSaveFile[nPlayer]);
}

BOOL canRecallOilAt(SDWORD nPlayer, SDWORD x, SDWORD y)
{
	SDWORD i;

	/* go through all remembered oil and check */
	for(i=0; i<MAX_OIL_LOCATIONS; i++)
	{
		if(oilLocation[nPlayer][i][0] != x)
			continue;

		if(oilLocation[nPlayer][i][1] != y)
			continue;

		return TRUE;		//yep, both matched
	}

	return FALSE;			//no
}

BOOL ReadAISaveData(SDWORD nPlayer)
{
	FEATURE					*psFeature;
	SDWORD					x=0,y=0;
	SDWORD					NumEntries=0;	//How many derricks/oil resources will be saved
	UDWORD					PosXY[MAX_OIL_ENTRIES];		//Locations, 0=x,1=y,2=x etc
	SDWORD						i;
	BOOL					Found;

	if(!SetUpInputFile(nPlayer))
	{	//printf_console
		//debug(LOG_ERROR,"No experience data loaded for %d",nPlayer);
		return FALSE;
	}
	else
	{
		/* Read data version */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to read version number for player '%d'",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"version: %d", NumEntries);

		/************************/
		/*		Enemy bases		*/
		/************************/

		/* read max number of players (usually 8) */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to read number of players for player '%d'",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"num enemy base: %d", NumEntries);

		/* read base locations of all players */
		if (PHYSFS_read(aiSaveFile[nPlayer], baseLocation[nPlayer], sizeof(SDWORD), NumEntries * 2 ) != (NumEntries * 2) )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load baseLocation for player '%d'",nPlayer);
			return FALSE;
		}

		/************************************/
		/*		Base attack locations		*/
		/************************************/

		/* read MAX_BASE_DEFEND_LOCATIONS */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(SDWORD), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to read MAX_BASE_DEFEND_LOCATIONS for player '%d'",nPlayer);
			return FALSE;
		}

		/* check it's the same as current MAX_BASE_DEFEND_LOCATIONS */
		if(NumEntries > MAX_BASE_DEFEND_LOCATIONS)
		{
			debug(LOG_ERROR, "ReadAISaveData(): saved MAX_BASE_DEFEND_LOCATIONS and current one don't match (%d / %d)", NumEntries, MAX_BASE_DEFEND_LOCATIONS);
			return FALSE;
		}

		//debug(LOG_ERROR,"num base attack loc: %d", NumEntries);

		/* read base defence locations */
		if (PHYSFS_read(aiSaveFile[nPlayer], baseDefendLocation[nPlayer], sizeof(SDWORD), NumEntries * 2 ) != (NumEntries * 2) )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load baseDefendLocation for player '%d'",nPlayer);
			return FALSE;
		}

		/* read base defend priorities */
		if (PHYSFS_read(aiSaveFile[nPlayer], baseDefendLocPrior[nPlayer], sizeof(SDWORD), NumEntries * 2 ) != (NumEntries * 2) )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load baseDefendLocPrior for player '%d'",nPlayer);
			return FALSE;
		}

		/************************************/
		/*		Oil attack locations		*/
		/************************************/

		/* read MAX_OIL_DEFEND_LOCATIONS */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to read max number of oil attack locations for player '%d'",nPlayer);
			return FALSE;
		}

		/* check it's the same as current MAX_OIL_DEFEND_LOCATIONS */
		if(NumEntries > MAX_OIL_DEFEND_LOCATIONS)
		{
			debug(LOG_ERROR, "ReadAISaveData(): saved MAX_OIL_DEFEND_LOCATIONS and current one don't match (%d / %d)", NumEntries, MAX_OIL_DEFEND_LOCATIONS);
			return FALSE;
		}

		//debug(LOG_ERROR,"num oil attack loc: %d", NumEntries);

		/* read oil locations */
		if (PHYSFS_read(aiSaveFile[nPlayer], oilDefendLocation[nPlayer], sizeof(SDWORD), NumEntries * 2 ) != (NumEntries * 2) )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load oilDefendLocation for player '%d'",nPlayer);
			return FALSE;
		}

		/* read oil location priority */
		if (PHYSFS_read(aiSaveFile[nPlayer], oilDefendLocPrior[nPlayer], sizeof(SDWORD), NumEntries * 2 ) != (NumEntries * 2) )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load oilDefendLocPrior for player '%d'",nPlayer);
			return FALSE;
		}

		/****************************/
		/*		Oil Resources		*/
		/****************************/

		/* read MAX_OIL_LOCATIONS */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to load MAX_OIL_LOCATIONS for player '%d'",nPlayer);
			return FALSE;
		}

		/* check it's the same as current MAX_OIL_LOCATIONS */
		if(NumEntries > MAX_OIL_LOCATIONS)
		{
			debug(LOG_ERROR, "ReadAISaveData(): saved MAX_OIL_LOCATIONS and current one don't match (%d / %d)", NumEntries, MAX_OIL_LOCATIONS);
			return FALSE;
		}


		/* Read number of Oil Resources */
		if (PHYSFS_read(aiSaveFile[nPlayer], &NumEntries, sizeof(NumEntries), 1 ) != 1 )
		{
			debug(LOG_ERROR,"ReadAISaveData(): Failed to read Oil Resources count for player '%d'",nPlayer);
			return FALSE;
		}

		//debug(LOG_ERROR,"Num oil: %d", NumEntries);

		if(NumEntries > 0)	//any oil resources were saved?
		{
			/* Read Oil Resources coordinates */
			if (PHYSFS_read(aiSaveFile[nPlayer], PosXY, sizeof(UDWORD), NumEntries * 2 ) != (NumEntries * 2) )
			{
				debug(LOG_ERROR,"ReadAISaveData(): Failed to read Oil Resources coordinates for player '%d'",nPlayer);
				return FALSE;
			}

			for(i=0; i<NumEntries; i++)
			{
				Found = FALSE;

				//re-read into remory
				if(i < MAX_OIL_LOCATIONS)	//didn't max out?
				{
					oilLocation[nPlayer][i][0] = PosXY[i * 2];		//x
					oilLocation[nPlayer][i][1] = PosXY[i * 2 + 1];	//y
				}

				/* Iterate through all Oil Resources and try to match coordinates */
				for(psFeature = apsFeatureLists[0]; psFeature != NULL; psFeature = psFeature->psNext)
				{
					if(psFeature->psStats->subType == FEAT_OIL_RESOURCE)	//Oil resource
					{
						if (!(psFeature->visible[nPlayer]))		//Not visible yet
						{
							if((PosXY[i * 2] == psFeature->x) && (PosXY[i * 2 + 1] == psFeature->y))	/* Found it */
							{
								//printf_console("Matched oil resource at x: %d y: %d", PosXY[i * 2]/128,PosXY[i * 2 + 1]/128);

								psFeature->visible[nPlayer] = TRUE;		//Make visible for AI
								Found = TRUE;
								break;
							}
						}

					}
				}

				//if(!Found)		//Couldn't find oil resource with this coords on the map
				//	printf_console("!!Failed to match oil resource #%d at x: %d y: %d", i,PosXY[i * 2]/128,PosXY[i * 2 + 1]/128);
			}
		}
	}


	return PHYSFS_close(aiSaveFile[nPlayer]);
}

BOOL OilResourceAt(UDWORD OilX,UDWORD OilY, SDWORD VisibleToPlayer)
{
	FEATURE					*psFeature;
	SDWORD					x=0,y=0;
	SDWORD					NumEntries=0;	//How many derricks/oil resources will be saved
	BOOL					Found;


	/* Iterate through all Oil Resources and try to match coordinates */
	for(psFeature = apsFeatureLists[0]; psFeature != NULL; psFeature = psFeature->psNext)
	{
		if(psFeature->psStats->subType == FEAT_OIL_RESOURCE)	//Oil resource
		{
			if ((VisibleToPlayer < 0) || (!(psFeature->visible[VisibleToPlayer])))		//Not visible yet
			{
				if((OilX == psFeature->x) && (OilY == psFeature->y))	/* Found it */
				{
					printf_console("Matched oil resource at x: %d y: %d", OilX/128,OilY/128);

					psFeature->visible[VisibleToPlayer] = TRUE;		//Make visible for AI
					Found = TRUE;
					break;
				}
			}

		}
	}

	return TRUE;
}




//x and y are passed by script, find out if this loc is close to
//an already stored loc, if yes then increase its priority
BOOL StoreBaseDefendLoc(SDWORD x, SDWORD y, SDWORD nPlayer)
{
	SDWORD	i, index;
	BOOL	found;
	
	index = GetBaseDefendLocIndex(x, y, nPlayer);
	if(index < 0)			//this one is new
	{
		//find an empty element
		found = FALSE;
		for(i=0; i < MAX_BASE_DEFEND_LOCATIONS; i++)
		{
			if(baseDefendLocation[nPlayer][i][0] < 0)	//not initialized yet
			{
				//addConsoleMessage("Base defense location - NEW LOCATION.", RIGHT_JUSTIFY);

				baseDefendLocation[nPlayer][i][0] = x;
				baseDefendLocation[nPlayer][i][1] = y;

				baseDefendLocPrior[nPlayer][i] = 1;

				found = TRUE;

				return TRUE;
			}
		}

		addConsoleMessage("Base defense location - NO SPACE LEFT.",RIGHT_JUSTIFY);
		return FALSE;		//not enough space to store
	}
	else		//this one already stored
	{
		//addConsoleMessage("Base defense location - INCREASED PRIORITY.",RIGHT_JUSTIFY);

		baseDefendLocPrior[nPlayer][index]++;	//higher the priority

		if(baseDefendLocPrior[nPlayer][index] == SDWORD_MAX)
			baseDefendLocPrior[nPlayer][index] = 1;			//start all over

		SortBaseDefendLoc(nPlayer);				//now sort everything
	}

	return TRUE;
}

SDWORD GetBaseDefendLocIndex(SDWORD x, SDWORD y, SDWORD nPlayer)
{
	SDWORD	i,range;

	range  = SAME_LOC_RANGE << TILE_SHIFT;		//in world units

	for(i=0; i < MAX_BASE_DEFEND_LOCATIONS; i++)
	{
		if((baseDefendLocation[nPlayer][i][0] > 0) && (baseDefendLocation[nPlayer][i][1] > 0))		//if this one initialized
		{
			//check if very close to an already stored location
			if(dirtySqrt(x,y,baseDefendLocation[nPlayer][i][0],baseDefendLocation[nPlayer][i][1]) < range)
			{
				return i;								//end here
			}
		}
	}

	return -1;
}

//sort the priorities, placing the higher ones at the top
BOOL SortBaseDefendLoc(SDWORD nPlayer)
{
	SDWORD i, prior, temp,LowestPrior,LowestIndex,SortBound;



	SortBound = MAX_BASE_DEFEND_LOCATIONS-1;	//nothing sorted yet, point at last elem

	while(SortBound >= 0)		//while didn't reach the top
	{
		LowestPrior = (SDWORD_MAX - 1);

		LowestIndex = -1;

		//find lowest element
		for(i=0; i <= SortBound; i++)
		{
			prior = baseDefendLocPrior[nPlayer][i];
			if(prior < LowestPrior)	//lower and isn't a flag meaning this one wasn't initialized with anything
			{
				LowestPrior = prior;
				LowestIndex = i;
			}
		}

		//huh, nothing found? (probably nothing set yet, no experience)
		if(LowestIndex < 0)
		{
			//debug(LOG_ERROR,"sortBaseDefendLoc() - No lowest elem found");
			return TRUE;
		}

		//swap
		if(LowestPrior < baseDefendLocPrior[nPlayer][SortBound])	//need to swap? (might be the same elem)
		{
			//priority
			temp = baseDefendLocPrior[nPlayer][SortBound];
			baseDefendLocPrior[nPlayer][SortBound] = baseDefendLocPrior[nPlayer][LowestIndex];
			baseDefendLocPrior[nPlayer][LowestIndex] = temp;

			//x
			temp = baseDefendLocation[nPlayer][SortBound][0];
			baseDefendLocation[nPlayer][SortBound][0] = baseDefendLocation[nPlayer][LowestIndex][0];
			baseDefendLocation[nPlayer][LowestIndex][0] = temp;

			//y
			temp = baseDefendLocation[nPlayer][SortBound][1];
			baseDefendLocation[nPlayer][SortBound][1] = baseDefendLocation[nPlayer][LowestIndex][1];
			baseDefendLocation[nPlayer][LowestIndex][1] = temp;
		}

		SortBound--;		//in any case lower the boundry, even if didn't swap
	}

	return TRUE;
}

void BaseExperienceDebug(SDWORD nPlayer)
{
	SDWORD i;

	printf_console("-------------");
	for(i=0; i< MAX_BASE_DEFEND_LOCATIONS; i++)
	{
		printf_console("%d) %d - %d (%d)",i,baseDefendLocation[nPlayer][i][0] >> TILE_SHIFT ,baseDefendLocation[nPlayer][i][1] >> TILE_SHIFT, baseDefendLocPrior[nPlayer][i] );
	}
	printf_console("-------------");
}

void OilExperienceDebug(SDWORD nPlayer)
{
	SDWORD i;

	printf_console("-------------");
	for(i=0; i< MAX_OIL_DEFEND_LOCATIONS; i++)
	{
		printf_console("%d) %d - %d (%d)",i,oilDefendLocation[nPlayer][i][0] >> TILE_SHIFT ,oilDefendLocation[nPlayer][i][1] >> TILE_SHIFT, oilDefendLocPrior[nPlayer][i] );
	}
	printf_console("-------------");
}


//x and y are passed by script, find out if this loc is close to
//an already stored loc, if yes then increase its priority
BOOL StoreOilDefendLoc(SDWORD x, SDWORD y, SDWORD nPlayer)
{
	SDWORD	i, index;
	BOOL	found;
	
	index = GetOilDefendLocIndex(x, y, nPlayer);
	if(index < 0)			//this one is new
	{
		//find an empty element
		found = FALSE;
		for(i=0; i < MAX_OIL_DEFEND_LOCATIONS; i++)
		{
			if(oilDefendLocation[nPlayer][i][0] < 0)	//not initialized yet
			{
				//addConsoleMessage("Oil defense location - NEW LOCATION.", RIGHT_JUSTIFY);

				oilDefendLocation[nPlayer][i][0] = x;
				oilDefendLocation[nPlayer][i][1] = y;

				oilDefendLocPrior[nPlayer][i] = 1;

				found = TRUE;

				return TRUE;
			}
		}

		addConsoleMessage("Oil defense location - NO SPACE LEFT.",RIGHT_JUSTIFY);
		return FALSE;		//not enough space to store
	}
	else		//this one already stored
	{
		//addConsoleMessage("Oil defense location - INCREASED PRIORITY.",RIGHT_JUSTIFY);

		oilDefendLocPrior[nPlayer][index]++;	//higher the priority

		if(oilDefendLocPrior[nPlayer][index] == SDWORD_MAX)
			oilDefendLocPrior[nPlayer][index] = 1;			//start all over

		SortOilDefendLoc(nPlayer);				//now sort everything
	}

	return TRUE;
}

SDWORD GetOilDefendLocIndex(SDWORD x, SDWORD y, SDWORD nPlayer)
{
	SDWORD	i,range;

	range  = SAME_LOC_RANGE << TILE_SHIFT;		//in world units

	for(i=0; i < MAX_OIL_DEFEND_LOCATIONS; i++)
	{
		if((oilDefendLocation[nPlayer][i][0] > 0) && (oilDefendLocation[nPlayer][i][1] > 0))		//if this one initialized
		{
			//check if very close to an already stored location
			if(dirtySqrt(x,y,oilDefendLocation[nPlayer][i][0],oilDefendLocation[nPlayer][i][1]) < range)
			{
				return i;								//end here
			}
		}
	}

	return -1;
}

//sort the priorities, placing the higher ones at the top
BOOL SortOilDefendLoc(SDWORD nPlayer)
{
	SDWORD i, prior, temp,LowestPrior,LowestIndex,SortBound;

	SortBound = MAX_OIL_DEFEND_LOCATIONS-1;	//nothing sorted yet, point at last elem

	while(SortBound >= 0)		//while didn't reach the top
	{
		LowestPrior = (SDWORD_MAX - 1);

		LowestIndex = -1;

		//find lowest element
		for(i=0; i <= SortBound; i++)
		{
			prior = oilDefendLocPrior[nPlayer][i];
			if(prior < LowestPrior)	//lower and isn't a flag meaning this one wasn't initialized with anything
			{
				LowestPrior = prior;
				LowestIndex = i;
			}
		}

		//huh, nothing found? (probably nothing set yet, no experience)
		if(LowestIndex < 0)
		{
			//debug(LOG_ERROR,"sortBaseDefendLoc() - No lowest elem found");
			return TRUE;
		}

		//swap
		if(LowestPrior < oilDefendLocPrior[nPlayer][SortBound])	//need to swap? (might be the same elem)
		{
			//priority
			temp = oilDefendLocPrior[nPlayer][SortBound];
			oilDefendLocPrior[nPlayer][SortBound] = oilDefendLocPrior[nPlayer][LowestIndex];
			oilDefendLocPrior[nPlayer][LowestIndex] = temp;

			//x
			temp = oilDefendLocation[nPlayer][SortBound][0];
			oilDefendLocation[nPlayer][SortBound][0] = oilDefendLocation[nPlayer][LowestIndex][0];
			oilDefendLocation[nPlayer][LowestIndex][0] = temp;

			//y
			temp = oilDefendLocation[nPlayer][SortBound][1];
			oilDefendLocation[nPlayer][SortBound][1] = oilDefendLocation[nPlayer][LowestIndex][1];
			oilDefendLocation[nPlayer][LowestIndex][1] = temp;
		}

		SortBound--;		//in any case lower the boundry, even if didn't swap
	}

	return TRUE;
}

BOOL CanRememberPlayerBaseLoc(SDWORD lookingPlayer, SDWORD enemyPlayer)
{
	if(lookingPlayer < 0 || enemyPlayer < 0)
		return FALSE;

	if(lookingPlayer >= MAX_PLAYERS || enemyPlayer >= MAX_PLAYERS)
		return FALSE;
	
	if(baseLocation[lookingPlayer][enemyPlayer][0] <= 0)
		return FALSE;
	if(baseLocation[lookingPlayer][enemyPlayer][1] <= 0) 
		return FALSE;

	return TRUE;
}

BOOL CanRememberPlayerBaseDefenseLoc(SDWORD player, SDWORD index)
{
	if(player < 0)
		return FALSE;
	if(player >= MAX_PLAYERS)
		return FALSE;
	if(index < 0 || index >= MAX_BASE_DEFEND_LOCATIONS)
		return FALSE;
	if(baseDefendLocation[player][index][0] <= 0)
		return FALSE;
	if(baseDefendLocation[player][index][1] <= 0) 
		return FALSE;

	return TRUE;
}

BOOL CanRememberPlayerOilDefenseLoc(SDWORD player, SDWORD index)
{
	if(player < 0)
		return FALSE;
	if(player >= MAX_PLAYERS)
		return FALSE;
	if(index < 0 || index >= MAX_BASE_DEFEND_LOCATIONS)
		return FALSE;
	if(oilDefendLocation[player][index][0] <= 0)
		return FALSE;
	if(oilDefendLocation[player][index][1] <= 0) 
		return FALSE;

	return TRUE;
}
