(*
getItemMetadata.scpt --- get data to be passed to org-protocol from front application

Copyright (C) 2009, 2010 Christopher Suckling

Author:  Christopher Suckling <suckling at gmail dot com>
		Alexander Poslavsky <alexander.poslavsky at gmail.com>

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

Vesion: 0.634

Commentary

Part of org-mac-protocol

Installation

1) Open in AppleScript Editor
2) Save as File Format: Script in  ~/Library/Scripts/orgQSLib/

Please see org-mac-protocol.org for full installation and usage instructions
*)

global theLink
global escErrorURL
global escErrorMessage
global escApp

on getItemMetadata(theProtocol, theApp)
	
	if (theApp as string) is not "Safari-Tabs" then
		linkError(theProtocol, theApp)
	end if
	
	if (theApp as string) = "Safari" then
		linkSafari(theProtocol, theApp)
	else
		if (theApp as string) = "Safari-Tabs" then
			linkSafariTabs(theProtocol, theApp)
		else
			if (theApp as string) = "firefox-bin" then
				linkFirefox(theProtocol, theApp)
			else
				if (theApp as string) = "Google Chrome" then
					linkChrome(theProtocol, theApp)
				else
					if (theApp as string) = "Skim" then
						linkSkim(theProtocol, theApp)
					else
						if (theApp as string) = "BibDesk" then
							linkBibDesk(theProtocol, theApp)
						else
							if (theApp as string) = "Pages" then
								linkPages(theProtocol, theApp)
							else
								if (theApp as string) = "Numbers" then
									linkNumbers(theProtocol, theApp)
								else
									if (theApp as string) = "Keynote" then
										linkKeynote(theProtocol, theApp)
									else
										if (theApp as string) = "Mail" then
											linkMail(theProtocol, theApp)
										else
											if (theApp as string) = "Address Book" then
												linkAddressBook(theProtocol, theApp)
											else
												if (theApp as string) = "iTunes" then
													linkITunes(theProtocol, theApp)
												else
													if (theApp as string) = "Terminal" then
														linkTerminal(theProtocol, theApp)
													else
														if (theApp as string) = "Finder" then
															linkFinder(theProtocol, theApp)
														else
															linkApplication(theProtocol, theApp)
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
				end if
			end if
		end if
	end if
	
	return theLink
	
end getItemMetadata

on encodeURIComponent(theURI)
	global escapeLib
	set escURI to do shell script "ruby " & (POSIX path of escapeLib) & " " & quoted form of theURI
end encodeURIComponent


(*
Format of links:

org-protocol:/protocol:/key/URI/description/short description/content:application name

theProtocol - org-protocol:/protocol:/key/
theApp - application name

short description - for remember templates; removes theApp and other contextual information from description
*)

on linkError(theProtocol, theApp)
	set theErrorURL to POSIX path of (path to application theApp)
	set theErrorMessage to theApp & ": no AppleScript support"
	set escErrorURL to encodeURIComponent(theErrorURL)
	set escErrorMessage to encodeURIComponent(theErrorMessage)
	set escApp to encodeURIComponent(theApp)
end linkError

on linkSafari(theProtocol, theApp)
	tell application "Safari"
		set theURL to do JavaScript "document.URL" in document 1
		set theShortTitle to (do JavaScript "document.title" in document 1)
		set theTitle to theShortTitle & ":" & theApp
		set theContent to do JavaScript "window.getSelection()" in document 1
	end tell
	
	set escURL to encodeURIComponent(theURL)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escURL & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
end linkSafari

on linkSafariTabs(theProtocol, theApp)
	tell application "Safari"
		set theTabs to tabs of window 1
		set theLinkList to {}
		repeat with eachTab in theTabs
			set theURL to (do JavaScript "document.URL" in eachTab)
			set theTitle to (do JavaScript "document.title" in eachTab)
			set escURL to encodeURIComponent(theURL) of me
			set escTitle to encodeURIComponent(theTitle) of me
			set eachLink to escURL & "/" & escTitle & "/" & "::"
			copy eachLink to end of theLinkList
		end repeat
	end tell
	
	set theLink to theProtocol & theLinkList
end linkSafariTabs

on linkFirefox(theProtocol, theApp)
	tell application "Firefox"
		activate
		set oldClipboard to the clipboard
		tell application "System Events"
			keystroke "l" using command down
			keystroke "c" using command down
		end tell
		delay 0.15
		set theURL to the clipboard
		set the clipboard to oldClipboard
		set theShortTitle to (get name of window 1)
		set theTitle to theShortTitle & ":Firefox"
		set theContent to the clipboard
	end tell
	
	set escURL to encodeURIComponent(theURL)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escURL & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":Firefox"
end linkFirefox

on linkChrome(theProtocol, theApp)
	set oldClipboard to the clipboard
	tell application "Google Chrome"
		tell application "System Events"
			keystroke "l" using command down
			keystroke "c" using command down
		end tell
		delay 0.15
		set theURL to the clipboard
		set the clipboard to oldClipboard
		set theShortTitle to (get name of window 1)
		set theTitle to theShortTitle & ":" & theApp
		set theContent to the clipboard
	end tell
	
	set escURL to encodeURIComponent(theURL)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escURL & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
end linkChrome


on linkSkim(theProtocol, theApp)
	tell application "Skim"
		set theScheme to "skim:"
		set theDoc to front document
		set theShortTitle to (name of theDoc)
		set theTitle to theShortTitle & ":" & theApp
		set thePath to (path of theDoc) & "::"
		set theSelection to selection of theDoc
		set theContent to contents of (get text for theSelection)
		if theContent is missing value then
			set theContent to ""
		end if
		set thePage to (get index for current page of theDoc)
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escPath to encodeURIComponent(thePath)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escScheme & escPath & thePage & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
end linkSkim

on linkBibDesk(theProtocol, theApp)
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
		set theReference to templated text of theDoc using text templateText for thePub
		set theCite to cite key of thePub
		set theAppName to ":" & theApp
		set theKeywords to keywords of thePub
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escTitle to encodeURIComponent(theTitle)
	set escCite to encodeURIComponent(theCite)
	set escPath to encodeURIComponent(thePath)
	set escReference to encodeURIComponent(theReference)
	set escAppName to encodeURIComponent(theAppName)
	
	set theKeywordsSed to (do shell script "echo \"" & theKeywords & "\" | sed -e 's/[;,]//g'")
	
	set theProperty to ":PROPERTIES:
  :BIBDESK:  " & theKeywordsSed & "
  :END:"
	set theContent to theProperty & "

  " & theReference
	set escContent to encodeURIComponent(theContent)
	
	
	set theLink to theProtocol & escScheme & escPath & escCite & "/" & escTitle & escCite & escAppName & "/" & escCite & "/" & escContent & ":" & escApp
end linkBibDesk

on linkPages(theProtocol, theApp)
	tell application "Pages"
		set theScheme to "pages:"
		set theDoc to front document
		set theShortTitle to (name of theDoc)
		set theTitle to theShortTitle & ":" & theApp
		set thePath to (path of theDoc) & "::"
		set theContent to the selection of theDoc
		set theCharOff to character offset of theContent
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escPath to encodeURIComponent(thePath)
	set escContent to encodeURIComponent(theContent as text)
	
	set theLink to theProtocol & escScheme & escPath & theCharOff & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
end linkPages

on linkNumbers(theProtocol, theApp)
	tell application "Numbers"
		set theScheme to "numbers:"
		set theDoc to front document
		set theShortTitle to (name of theDoc)
		set theTitle to theShortTitle & ":" & theApp
		set thePath to (path of theDoc) & "::"
		tell theDoc
			set theSheet to 0
			repeat with i from 1 to the count of sheets
				tell sheet i
					set x to the count of (tables whose selection range is not missing value)
				end tell
				if x is not 0 then
					set theSheet to i
					exit repeat
				end if
			end repeat
			if theSheet is 0 then
				set theSheet to 1 & "::"
				set theTable to 1 & "::"
				set theRange to "A1:A1"
				set theContent to ""
			else
				tell sheet theSheet
					set theTable to first table whose selection range is not missing value
					tell theTable
						set theSheet to (theSheet as text) & "::"
						set theTable to (name of theTable) & "::"
						set theRange to (name of selection range)
						set theRangeValues to value of every cell of selection range
						set AppleScript's text item delimiters to " "
						set theContent to theRangeValues as string
						set AppleScript's text item delimiters to ""
					end tell
				end tell
			end if
		end tell
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	set escPath to encodeURIComponent(thePath)
	set escSheet to encodeURIComponent(theSheet)
	set escTable to encodeURIComponent(theTable)
	set escRange to encodeURIComponent(theRange)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escScheme & escPath & escSheet & escTable & escRange & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
end linkNumbers

on linkKeynote(theProtocol, theApp)
	tell application "Keynote"
		set theScheme to "keynote:"
		set theDoc to front slideshow
		set theShortTitle to (name of theDoc)
		set theTitle to theShortTitle & ":Keynote"
		set thePath to (path of theDoc) & "::"
		set theSlide to (current slide of front slideshow)
		set theSlideIndex to (slide number of theSlide)
		set theContent to (title of theSlide) & "
	
	" & (body of theSlide)
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escPath to encodeURIComponent(thePath)
	set escTitle to encodeURIComponent(theTitle)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escScheme & escPath & theSlideIndex & "/" & escTitle & "/" & escShortTitle & "/" & escContent & ":" & escApp
	
end linkKeynote

on linkMail(theProtocol, theApp)
	tell application "Mail"
		set theSelection to selection
		repeat with theMessage in theSelection
			set theID to message id of theMessage
			set theShortSubject to (subject of theMessage)
			set theSubject to theShortSubject & ":" & theApp
			set theContent to content of theMessage
		end repeat
		set theScheme to "message://"
	end tell
	
	set escID to encodeURIComponent(theID)
	set escShortSubject to encodeURIComponent(theShortSubject)
	set escSubject to encodeURIComponent(theSubject)
	set escScheme to encodeURIComponent(theScheme)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escScheme & escID & "/" & escSubject & "/" & escShortSubject & "/" & escContent & ":" & escApp
end linkMail

on linkAddressBook(theProtocol, theApp)
	(*
By Alexander Poslavsky
*)
	tell application "Address Book"
		set theScheme to "address:"
		set AllContacts to selection
		if number of items in AllContacts = 1 then
			set one_contact to item 1 of AllContacts
			set theID to id of one_contact
			set theName to name of one_contact
		else
			tell application "System Events"
				activate
				display dialog "Error: Choose one contact"
			end tell
		end if
	end tell
	
	set escID to encodeURIComponent(theID)
	set escName to encodeURIComponent(theName)
	set escScheme to encodeURIComponent(theScheme)
	set theLink to theProtocol & escScheme & escID & "/" & escName & ":" & escApp
end linkAddressBook


on linkITunes(theProtocol, theApp)
	tell application "iTunes"
		set theScheme to "iTunes:"
		set theID to (persistent ID of (item 1 of selection))
		set theShortName to (name of (item 1 of selection))
		set theName to theShortName & ":" & theApp
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
	set escShortName to encodeURIComponent(theShortName)
	set escName to encodeURIComponent(theName)
	set escTitle to encodeURIComponent(theTitle)
	set escComposer to encodeURIComponent(theComposer)
	set escAlbum to encodeURIComponent(theAlbum)
	set escArtist to encodeURIComponent(theArtist)
	set escContent to encodeURIComponent(theContent)
	set theLink to theProtocol & escScheme & theID & "/" & escName & "/" & escShortName & "/" & escContent & ":" & escApp
end linkITunes

on linkTerminal(theProtocol, theApp)
	tell application "Terminal"
		tell front window
			set theContent to contents of selected tab
		end tell
		set theScheme to "file:/"
		set theShortName to (name of front window)
		set theName to theShortName & ":" & theApp
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escShortName to encodeURIComponent(theShortName)
	set escName to encodeURIComponent(theName)
	set escContent to encodeURIComponent(theContent)
	
	set theLink to theProtocol & escScheme & escErrorURL & "/" & escName & "/" & escShortName & "/" & escContent & ":" & escApp
end linkTerminal

on linkFinder(theProtocol, theApp)
	tell application "Finder"
		set theScheme to "file:/"
		set theItem to selection as alias
		set thePath to POSIX path of theItem
		set theShortTitle to (name of (get info for theItem))
		set theTitle to theShortTitle & ":" & theApp
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	set escPath to encodeURIComponent(thePath)
	set escShortTitle to encodeURIComponent(theShortTitle)
	set escTitle to encodeURIComponent(theTitle)
	
	set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & escShortTitle & ":" & escApp
end linkFinder

on linkApplication(theProtocol, theApp)
	tell application (theApp as string)
		set theScheme to "file:/"
		set appUnsupported to false
		try
			set theDoc to front document
		on error
			set appUnsupported to true
		end try
		if appUnsupported is false then
			set theShortTitle to (name of theDoc)
			set theTitle to theShortTitle & ":" & theApp
			set thePath to path of theDoc
			
		end if
	end tell
	
	set escScheme to encodeURIComponent(theScheme)
	if appUnsupported is true then
		set theLink to theProtocol & escScheme & escErrorURL & "/" & escErrorMessage & ":" & escApp
	else
		set escPath to encodeURIComponent(thePath)
		set escShortTitle to encodeURIComponent(theShortTitle)
		set escTitle to encodeURIComponent(theTitle)
		set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & escShortTitle
	end if
end linkApplication