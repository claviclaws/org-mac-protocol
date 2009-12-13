(*
org-remember.scpt --- make a note in an org-mode file, linking to the front
document and any selected text

Copyright (C) 2009 Christopher Suckling

Author: Christopher Suckling <suckling at gmail dot com>

This file is Free Software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

It is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

Vesion: 0.630

Commentary

Part of org-mac-protocol

Installation

1) Copy to ~/Library/Scripts/orgQSLib/

Please see org-mac-protocol.org for full installation and usage instructions
*)

set ASTID to AppleScript's text item delimiters
set text item delimiters to {":"}
set myPath to (path to me) as text
set orgQSLib to (text items 1 through -2 of myPath) & "orgQSLib:" as text
set AppleScript's text item delimiters to ASTID

set getEmacsLib to (load script file ((orgQSLib as text) & "getEmacsClient.scpt"))
set getItemMetaLib to (load script file ((orgQSLib as text) & "getItemMetadata.scpt"))
global escapeLib
set escapeLib to ((orgQSLib as text) & "escape.rb")
set theProtocol to "org-protocol:/mac-remember:/y/"

tell application "System Events"
	set theApp to item 1 of (get name of processes whose frontmost is true)
end tell

set theScript to getEmacsLib's getEmacsClient() & " " & getItemMetaLib's getItemMetadata(theProtocol, theApp)

do shell script theScript