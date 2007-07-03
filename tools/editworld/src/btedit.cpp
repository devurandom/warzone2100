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
// BTEdit.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "btedit.h"

#include "mainfrm.h"
#include "bteditdoc.h"
#include "bteditview.h"
//#include "debugwin.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CBTEditApp


BEGIN_MESSAGE_MAP(CBTEditApp, CWinApp)
	//{{AFX_MSG_MAP(CBTEditApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
END_MESSAGE_MAP()

CBTEditCommandLineInfo g_cmdInfo;

void CBTEditCommandLineInfo::ParseParam(const char* pszParam,BOOL bFlag,BOOL bLast)
{
	if (bFlag)
	{
		if (lstrcmpA(pszParam, "emulation") == 0)
		{
			m_bForceEmulation = TRUE;
			return;
		}
		if (lstrcmpA(pszParam, "hardware") == 0)
		{
			m_bForceHardware = TRUE;
			return;
		}
		if (lstrcmpA(pszParam, "primary") == 0)
		{
			m_bForcePrimary = TRUE;
			return;
		}
		if (lstrcmpA(pszParam, "secondary") == 0)
		{
			m_bForceSecondary = TRUE;
			return;
		}
		if (lstrcmpA(pszParam, "ramp") == 0)
		{
			m_bForceRamp = TRUE;
			return;
		}
		if (lstrcmpA(pszParam, "rgb") == 0)
		{
			m_bForceRGB = TRUE;
			return;
		}
//		if (lstrcmpA(pszParam, "nosizecheck") == 0)
//		{
//			m_bMapSizePower2 = FALSE;
//			return;
//		}
		m_bMapSizePower2 = FALSE;
	}
	CCommandLineInfo::ParseParam(pszParam, bFlag, bLast);
}

/////////////////////////////////////////////////////////////////////////////
// CBTEditApp construction

CBTEditApp::CBTEditApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CBTEditApp object

CBTEditApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CBTEditApp initialization

char g_HomeDirectory[1024];
char g_WorkDirectory[1024];

HCURSOR g_Wait;
HCURSOR g_Pointer;
HCURSOR g_PointerPaint;
HCURSOR g_PointerFill;
HCURSOR g_PointerDrag;
HCURSOR g_PointerSelRect;
HCURSOR g_PointerSelPoint;
HCURSOR g_PointerPipet;
HCURSOR g_PointerPliers;

HICON g_IconIncrement;
HICON g_IconDecrement;
HICON g_IconSmallBrush;
HICON g_IconSmallBrush2;
HICON g_IconLargeBrush;
HICON g_IconLargeBrush2;

BOOL CBTEditApp::InitInstance()
{
	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

//	pDebugThread = AfxBeginThread(RUNTIME_CLASS(DebugThread));


#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

    SetRegistryKey("Pumpkin Studios");
	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CBTEditDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CBTEditView));
	AddDocTemplate(pDocTemplate);

	GetCurrentDirectory(sizeof(g_HomeDirectory),g_HomeDirectory);

	g_Wait = ::LoadCursor(NULL,IDC_WAIT);
	g_Pointer = LoadCursor(IDC_POINTER);
	g_PointerPaint = LoadCursor(IDC_POINTER_PAINT);
	g_PointerFill = LoadCursor(IDC_POINTER_FILL);
	g_PointerDrag = LoadCursor(IDC_POINTER_HGTDRAG);
	g_PointerSelRect = LoadCursor(IDC_POINTER_SELRECT);
	g_PointerSelPoint = LoadCursor(IDC_POINTER);
	g_PointerPipet = LoadCursor(IDC_PIPET);
	g_PointerPliers = LoadCursor(IDC_PLIERS);

	g_IconIncrement = LoadIcon(IDI_INCREMENT);
	g_IconDecrement = LoadIcon(IDI_DECREMENT);
	g_IconSmallBrush = LoadIcon(IDI_SMALLBRUSH);
	g_IconSmallBrush2 = LoadIcon(IDI_SMALLBRUSH2);
	g_IconLargeBrush = LoadIcon(IDI_LARGEBRUSH);
	g_IconLargeBrush2 = LoadIcon(IDI_LARGEBRUSH2);

	// Parse command line for standard shell commands, DDE, file open
//	CCommandLineInfo cmdInfo;
	ParseCommandLine(g_cmdInfo);

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(g_cmdInfo))
		return FALSE;

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CBTEditApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

/////////////////////////////////////////////////////////////////////////////
// CBTEditApp commands
