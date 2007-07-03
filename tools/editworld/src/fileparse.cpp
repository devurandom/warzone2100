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

#include	<windows.h>
#include	<fstream>
#include	<stdlib.h>
#include	<conio.h>
#include	<string.h>
#include	<assert.h>

#include	"fileparse.h"
#include	"debugprint.h"


CFileParser::CFileParser() :
	m_File(NULL)
{
	strcpy(m_Brk,"= \n\r\t");
}

CFileParser::CFileParser(std::istream& file, short flags) :
	m_File(NULL)
{
	m_Flags = flags;

	// Seek to the end to determine the file/stream's size
	file.seekg(0, std::ios::end);
	std::ifstream::pos_type fileSize = file.tellg();
	file.seekg(0);

	delete m_File;

	m_File = new char[fileSize + static_cast<std::ifstream::pos_type>(1)];
	if(m_File)
	{
		file.readsome(static_cast<std::istream::char_type*>(m_File), fileSize);
		m_File[fileSize]=0;
	}

	m_Pos = m_File;

	m_BufferSize = static_cast<long>(fileSize) + 1;
	strcpy(m_Brk,"= \n\r\t");
}

CFileParser::~CFileParser(void)
{
	delete m_File;
}


void CFileParser::Rewind(void)
{
	m_Pos = m_File;
}


BOOL CFileParser::ParseString(char *Ident,char *Word,short Size)
{
	char Tmp[256];
	if(Ident) {
		Parse(Tmp,sizeof(Tmp));
		if( (strcmp(Ident,Tmp)!=0) || (Tmp[0]==0) ) {
			return FALSE;
		}
	}
	Parse(Word,Size);

	return TRUE;
}


BOOL CFileParser::ParseInt(char *Ident,int *Int)
{
	char Tmp[256];
	if(Ident) {
		Parse(Tmp,sizeof(Tmp));
		if( (strcmp(Ident,Tmp)!=0) || (Tmp[0]==0) ) {
			return FALSE;
		}
	}
	Parse(Tmp,sizeof(Tmp));
	*Int = TokenToInt(Tmp);

	return TRUE;
}


BOOL CFileParser::ParseFloat(char *Ident,float *Float)
{
	char Tmp[256];
	if(Ident) {
		Parse(Tmp,sizeof(Tmp));
		if( (strcmp(Ident,Tmp)!=0) || (Tmp[0]==0) ) {
			return FALSE;
		}
	}
	Parse(Tmp,sizeof(Tmp));
	*Float = TokenToFloat(Tmp);

	return TRUE;
}


BOOL CFileParser::ParseDouble(char *Ident,double *Double)
{
	char Tmp[256];
	if(Ident) {
		Parse(Tmp,sizeof(Tmp));
		if( (strcmp(Ident,Tmp)!=0) || (Tmp[0]==0) ) {
			return FALSE;
		}
	}
	Parse(Tmp,sizeof(Tmp));
	*Double = TokenToDouble(Tmp);

	return TRUE;
}


void CFileParser::SetBreakCharacters(char *brk)
{
	strcpy(m_Brk,brk);
}


int CFileParser::CountTokens(void)
{
	char String[MAXTOKENSIZE];

	int NumTokens = 0;

	do {
		Parse(String,sizeof(String));
		if(String[0]) {
			NumTokens++;
		}
	} while(String[0]);

	Rewind();

	return NumTokens;
}


void CFileParser::Parse(char *Word,short Size)
{
	m_LastPos = m_Pos;

	if(m_Flags & FP_SKIPCOMMENTS) {
		while(1) {
			m_Pos = StringSkipBlanks(m_Pos);
			if(m_Flags & FP_ASMCOMMENTS) {
				if(m_Pos[0]==';') {
					// Skip ASM comment.
					m_Pos = StringSkipLine(m_Pos);
				} else {
					break;
				}
			} else {
				if((m_Pos[0]=='/') && (m_Pos[1]=='*')) {
					// Skip C comment.
					while(!((m_Pos[0]=='*') && (m_Pos[1]=='/'))) {
						m_Pos++;
					}
					m_Pos += 2;
				} else if((m_Pos[0]=='/') && (m_Pos[1]=='/')) {
					// Skip C++ comment.
					m_Pos = StringSkipLine(m_Pos);
				} else {
					break;
				}
			}
		}
		m_Pos = StringNextToken(m_Pos,Word,Size,m_Brk);
	} else {
		m_Pos = StringSkipBlanks(m_Pos);
		m_Pos = StringNextToken(m_Pos,Word,Size,m_Brk);
	}
}


void CFileParser::UnParse(void)
{
	m_Pos = m_LastPos;
}


TokenID *CFileParser::FindTokenID(char *Token,TokenID *IDLookup)
{
	TokenID *TokID = IDLookup;

	while(TokID->Token) {
		if(strcmp(TokID->Token,Token)==0) {
			return TokID;
		}

		TokID++;
	}

	return NULL;
}


BOOL CFileParser::FindToken(char *Token)
{
	char Tmp[256];
	do {
		Parse(Tmp,sizeof(Tmp));
		if(strcmp(Token,Tmp)==0) {
			return TRUE;
		}
	}
	while(*Tmp);

	return FALSE;
}


BOOL CFileParser::FindTokenList(char *Token, ...)
{
	BOOL FoundFirst;
	char *Tok;
	char Tmp[256];
	va_list args;

	FoundFirst = FALSE;
	do {
		if( (FoundFirst = FindToken(Token)) ) {
//			DebugPrint("%s,",Token);
			BOOL IsMatch = TRUE;
			va_start(args,Token);
			do {
				Tok = va_arg(args,char*);
				if(Tok!=NULL) {
					Parse(Tmp,sizeof(Tmp));
//					DebugPrint("Tok %s, Tmp %s\n",Tok,Tmp);
					if(strcmp(Tmp,Tok)!=0) {
						IsMatch=FALSE;
						break;
					}
				}
			} while(Tok!=NULL);
			va_end(args);

			if(IsMatch) {
//				DebugPrint("IsMatch\n");
				return TRUE;
			}
		}
	} while(FoundFirst);

	return FALSE;
}


int CFileParser::TokenToInt(char *Word)
{
	return atoi(Word);
}


float CFileParser::TokenToFloat(char *Word)
{
	return (float)atof(Word);
}


double CFileParser::TokenToDouble(char *Word)
{
	return atof(Word);
}


char *CFileParser::StringNextToken(char *s,char *tok,short toklen,char *brk)
{
	short	i = 0;

	char Break[256];
	char *QuoteBreak="\n\r";
	BOOL InQuoutes = FALSE;

	strcpy(Break,brk);

	while(!StringBreakChar(*s,Break) && *s!=0) {
		if(m_Flags & FP_QUOTES) {
			if(*s == '"') {
				if(InQuoutes) {
					strcpy(Break,brk);
					InQuoutes = FALSE;
				} else {
					strcpy(Break,QuoteBreak);
					InQuoutes = TRUE;
				}
			} else {
				if(i<toklen) {
					tok[i++]=StringFixCase(*s);
				}
			}
		} else {
			if(i<toklen) {
				tok[i++]=StringFixCase(*s);
			}
		}

		s++;
	}

	tok[i] = 0;

	while(StringBreakChar(*s,brk) && *s!=0) s++;

	return(s);
}


char CFileParser::StringFixCase(char chr)
{
	if(m_Flags & FP_UPPERCASE) {
		if( (chr >= 'a') && (chr <='z') ) {
			chr -='a'-'A';
		}
	}

	if(m_Flags & FP_LOWERCASE) {
		if( (chr >= 'A') && (chr <='Z') ) {
			chr +='a'-'A';
		}
	}

	return chr;
}


BOOL CFileParser::StringBreakChar(char chr,char *brk)
{
	short	i;

	for(i = 0; i < (short)strlen(brk); i++) {
		if(chr == brk[i]) return(1);
	}

	return(0);
}


char *CFileParser::StringSkipBlanks(char *p)
{
	while( (*p == ' ') || (*p == 9) || (*p == 10) || (*p == 13) ) {
		if(*p == 0) return(p);

		p++;
	}

	return(p);
}


char *CFileParser::StringSkipLine(char *p)
{
	while( (*p != 10) && (*p != 13) && (*p != 0) ) {
		p++;
	}

	if(*p) {
		p = StringSkipBlanks(p);
	}

	return(p);
}

