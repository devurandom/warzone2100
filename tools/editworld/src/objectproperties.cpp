// ObjectProperties.cpp : implementation file
//

#include "stdafx.h"
#include "btedit.h"
#include "objectproperties.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CObjectProperties dialog


CObjectProperties::CObjectProperties(CWnd* pParent /*=NULL*/)
	: CDialog(CObjectProperties::IDD, pParent)
{
	//{{AFX_DATA_INIT(CObjectProperties)
	m_Class = _T("");
	m_Name = _T("");
	m_PlayerID = 0;
	m_PosX = 0.0f;
	m_PosY = 0.0f;
	m_PosZ = 0.0f;
	m_RotX = 0;
	m_RotY = 0;
	m_RotZ = 0;
	m_UniqueID = 0;
	m_GPosX = 0;
	m_GPosY = 0;
	m_GPosZ = 0;
	//}}AFX_DATA_INIT
}


void CObjectProperties::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CObjectProperties)
	DDX_Text(pDX, IDC_OP_CLASS, m_Class);
	DDV_MaxChars(pDX, m_Class, 256);
	DDX_Text(pDX, IDC_OP_NAME, m_Name);
	DDV_MaxChars(pDX, m_Name, 256);
	DDX_Text(pDX, IDC_OP_PLAYERID, m_PlayerID);
	DDV_MinMaxInt(pDX, m_PlayerID, 0, 8);
	DDX_Text(pDX, IDC_OP_POSX, m_PosX);
	DDX_Text(pDX, IDC_OP_POSY, m_PosY);
	DDX_Text(pDX, IDC_OP_POSZ, m_PosZ);
	DDX_Text(pDX, IDC_OP_ROTX, m_RotX);
	DDV_MinMaxInt(pDX, m_RotX, 0, 359);
	DDX_Text(pDX, IDC_OP_ROTY, m_RotY);
	DDV_MinMaxInt(pDX, m_RotY, 0, 359);
	DDX_Text(pDX, IDC_OP_ROTZ, m_RotZ);
	DDV_MinMaxInt(pDX, m_RotZ, 0, 359);
	DDX_Text(pDX, IDC_OP_UID, m_UniqueID);
	DDV_MinMaxInt(pDX, m_UniqueID, 0, 2147483647);
	DDX_Text(pDX, IDC_WOP_POSX, m_GPosX);
	DDX_Text(pDX, IDC_WOP_POSY, m_GPosY);
	DDX_Text(pDX, IDC_WOP_POSZ, m_GPosZ);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CObjectProperties, CDialog)
	//{{AFX_MSG_MAP(CObjectProperties)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CObjectProperties message handlers
