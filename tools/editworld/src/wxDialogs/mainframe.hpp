/*
	This file is part of Warzone 2100.
	Copyright (C) 2007  Giel van Schijndel
	Copyright (C) 2007  Warzone Resurrection Project

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

#ifndef __INCLUDED_MAINFRAME_HPP__
#define __INCLUDED_MAINFRAME_HPP__

#include <wx/wxprec.h>

#ifndef WX_PRECOMP
    //(*Headers(MainFrame)
    #include <wx/frame.h>
    //*)
#endif

class MainFrame: public wxFrame
{
    public:
        MainFrame(wxWindow* parent = NULL);

        //(*Declarations(MainFrame)
        //*)

    private:
        //(*Identifiers(MainFrame)
        //*)

    private:
        //(*Handlers(MainFrame)
        void OnAbout(wxCommandEvent& event);
        //*)

    private:
        DECLARE_EVENT_TABLE()
};

#endif // __INCLUDED_MAINFRAME_HPP__
