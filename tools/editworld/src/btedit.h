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

	$Revision$
	$Id$
	$HeadURL$
*/
// BTEdit.h : main header file for the BTEDIT application
//

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CBTEditApp:
// See BTEdit.cpp for the implementation of this class
//

class CBTEditCommandLineInfo : public CCommandLineInfo
{
public:
	CBTEditCommandLineInfo() {
		m_bForceEmulation = FALSE;
		m_bForceHardware = FALSE;
		m_bForcePrimary = FALSE;
		m_bForceSecondary = FALSE;
		m_bForceRamp = FALSE;
		m_bForceRGB = FALSE;
		m_bMapSizePower2 = TRUE;
	}
	BOOL m_bForceEmulation;
	BOOL m_bForceHardware;
	BOOL m_bForcePrimary;
	BOOL m_bForceSecondary;
	BOOL m_bForceRamp;
	BOOL m_bForceRGB;
	BOOL m_bMapSizePower2;
	virtual void ParseParam(const char* pszParam,BOOL bFlag,BOOL bLast);
};

extern CBTEditCommandLineInfo g_cmdInfo;

class CBTEditApp : public CWinApp
{
public:
	CBTEditApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CBTEditApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CBTEditApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
