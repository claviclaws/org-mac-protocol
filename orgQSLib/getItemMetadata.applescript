(*
getItemMetadata.scpt --- retrieve information about selected items and create links to them

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

on encodeURIComponent(theURI)
	global escapeLib
	set escURI to do shell script "ruby " & (POSIX path of escapeLib) & " " & quoted form of theURI
end encodeURIComponent


on getItemMetadata(theProtocol, theApp)
	if (theApp as string) is not "Safari-Tabs" then
		set theErrorURL to POSIX path of (path to application theApp)
		set theErrorMessage to theApp & ": no AppleScript support"
		set escErrorURL to encodeURIComponent(theErrorURL)
		set escErrorMessage to encodeURIComponent(theErrorMessage)
		
		set escApp to encodeURIComponent(theApp)
	end if
	
	if (theApp as string) = "Safari" then
		tell application "Safari"
			set theURL to do JavaScript "document.URL" in document 1
			set theTitle to (do JavaScript "document.title" in document 1) & ":" & theApp
			set theContent to do JavaScript "window.getSelection()" in document 1
		end tell
		set escURL to encodeURIComponent(theURL)
		set escTitle to encodeURIComponent(theTitle)
		set escContent to encodeURIComponent(theContent)
		
		set theLink to theProtocol & escURL & "/" & escTitle & "/" & escContent & ":" & escApp
		
	else
		
		if (theApp as string) = "Safari-Tabs" then
			tell application "Safari"
				set theTabs to tabs of window 1
				set theLinkList to {}
				repeat with eachTab in theTabs
					set escURL to (do JavaScript "encodeURIComponent(document.URL)" in eachTab)
					set escTitle to (do JavaScript "encodeURIComponent(document.title)" in eachTab)
					set eachLink to escURL & "/" & escTitle & "/" & "::"
					copy eachLink to end of theLinkList
				end repeat
			end tell
			set theLink to theProtocol & (theLinkList as string)
			
			
		else
			
			
			if (theApp as string) = "Skim" then
				tell application "Skim"
					
					set theScheme to "skim:"
					set theDoc to front document
					set theTitle to (name of theDoc) & ":" & theApp
					set thePath to (path of theDoc) & "::"
					set theSelection to selection of theDoc
					set theContent to contents of (get text for theSelection)
					set thePage to (get index for current page of theDoc)
				end tell
				set escScheme to encodeURIComponent(theScheme)
				set escTitle to encodeURIComponent(theTitle)
				set escPath to encodeURIComponent(thePath)
				set escContent to encodeURIComponent(theContent)
				
				set theLink to theProtocol & escScheme & escPath & thePage & "/" & escTitle & "/" & escContent & ":" & escApp
				
			else
				
				if (theApp as string) = "BibDesk" then
					tell application "BibDesk"
						set templateText to "<$publications>
<$pubType=article?>
<$authors.name.@componentsJoinedByCommaAndAnd/> <$fields.Year/>. <$fields.Title/>. <$fields.Journal/>, <$fields.Volume/>(<$fields.Number/>), <$fields.Pages/>.
<?$pubType=book?>
<$authors.name.@componentsJoinedByCommaAndAnd/> <$fields.Year/>. <$fields.Title/>. <$fields.Address/>: <$fields.Publisher/>.
<?$pubType=unpublished?>
<$authors.name.@componentsJoinedByCommaAndAnd/> <$fields.Title/>. <$fields.Note/>
<?$pubType?>
<$authors.name.@componentsJoinedByCommaAndAnd/> <$fields.Year/>. <$fields.Title/>. <$fields.Journal/>, <$fields.Volume/>(<$fields.Number/>), <$fields.Pages/>
</$pubType?>
</$publications>
"
						set theScheme to "bibdesk:"
						set theDoc to front document
						set theTitle to (name of theDoc) & "::"
						set thePath to (path of theDoc) & "::"
						set theSelection to the selection of theDoc
						set thePub to item 1 of theSelection
						set theContent to templated text of theDoc using text templateText for thePub
						set theCite to cite key of thePub
						set theRef to ":" & theApp
					end tell
					set escScheme to encodeURIComponent(theScheme)
					set escTitle to encodeURIComponent(theTitle)
					set escCite to encodeURIComponent(theCite)
					set escPath to encodeURIComponent(thePath)
					set escContent to encodeURIComponent(theContent)
					set escRef to encodeURIComponent(theRef)
					
					set theLink to theProtocol & escScheme & escPath & escCite & "/" & escTitle & escCite & escRef & "/" & escContent & ":" & escApp
					
				else
					
					if (theApp as string) = "Mail" then
						tell application "Mail"
							
							set theSelection to selection
							repeat with theMessage in theSelection
								set theID to message id of theMessage
								set thesubject to (subject of theMessage) & ":" & theApp
								set theContent to content of theMessage
							end repeat
							
							set theScheme to "message://"
						end tell
						set escID to encodeURIComponent(theID)
						set escSubject to encodeURIComponent(thesubject)
						set escScheme to encodeURIComponent(theScheme)
						set escContent to encodeURIComponent(theContent)
						
						set theLink to theProtocol & escScheme & escID & "/" & escSubject & "/" & escContent & ":" & escApp
						
						
					else
						
						if (theApp as string) = "iTunes" then
							tell application "iTunes"
								set theScheme to "iTunes:"
								set theID to (persistent ID of (item 1 of selection))
								set theName to (name of (item 1 of selection)) & ":" & theApp
								set theTitle to (name of (item 1 of selection))
								set theComposer to (composer of (item 1 of selection))
								set theAlbum to (album of (item 1 of selection))
								set theArtist to (artist of (item 1 of selection))
								set theContent to "
" & theTitle & "
" & theAlbum & "
" & theComposer & "
" & theArtist
							end tell
							set escScheme to encodeURIComponent(theScheme)
							set escName to encodeURIComponent(theName)
							set escTitle to encodeURIComponent(theTitle)
							set escComposer to encodeURIComponent(theComposer)
							set escAlbum to encodeURIComponent(theAlbum)
							set escArtist to encodeURIComponent(theArtist)
							set escContent to encodeURIComponent(theContent)
							
							
							set theLink to theProtocol & escScheme & theID & "/" & escName & "/" & escContent & ":" & escApp
							
						else
							
							
							if (theApp as string) = "Finder" then
								tell application "Finder"
									set theScheme to "file:/"
									set theItem to selection as alias
									set thePath to POSIX path of theItem
									set theTitle to (name of (get info for theItem)) & ":" & theApp
								end tell
								set escScheme to encodeURIComponent(theScheme)
								set escPath to encodeURIComponent(thePath)
								set escTitle to encodeURIComponent(theTitle)
								
								set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & ":" & escApp
								
							else
								
								if (theApp as string) = "Terminal" then
									tell application "Terminal"
										tell front window
											set theContent to contents of selected tab
										end tell
										set theScheme to "file:/"
										set escScheme to encodeURIComponent(theScheme)
										set theName to (name of front window) & ":" & theApp
									end tell
									set escName to encodeURIComponent(theName)
									set escContent to encodeURIComponent(theContent)
									
									set theLink to theProtocol & escScheme & escErrorURL & "/" & escName & "/" & escContent & ":" & escApp
									
								else
									
									if (theApp as string) = "firefox-bin" then
										tell application "Firefox"
											set theURL to «class curl» of window 1
										end tell
										set escURL to encodeURIComponent(theURL)
										set theLink to theProtocol & escURL & "/" & escURL & ":" & escApp
										
										
									else
										
										tell application (theApp as string)
											set theScheme to "file:/"
											set appUnsupported to false
											try
												set theDoc to front document
											on error
												set appUnsupported to true
											end try
											if appUnsupported is false then
												set theTitle to (name of theDoc) & ":" & theApp
												set thePath to path of theDoc
												
											end if
										end tell
										
										set escScheme to encodeURIComponent(theScheme)
										if appUnsupported is true then
											set theLink to theProtocol & escScheme & escErrorURL & "/" & escErrorMessage & ":" & escApp
										else
											set escPath to encodeURIComponent(thePath)
											set escTitle to encodeURIComponent(theTitle)
											set theLink to theProtocol & escScheme & escPath & "/" & escTitle & ":" & escApp
										end if
										
									end if
								end if
							end if
						end if
					end if
				end if
			end if
		end if
	end if
	
	return theLink
	
end getItemMetadata