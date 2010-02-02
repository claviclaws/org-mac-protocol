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
			set escURL to (do JavaScript "encodeURIComponent(document.URL)" in eachTab)
			set escTitle to (do JavaScript "encodeURIComponent(document.title)" in eachTab)
			set eachLink to escURL & "/" & escTitle & "/" & "::"
			copy eachLink to end of theLinkList
		end repeat
	end tell
	
	set theLink to theProtocol & (theLinkList as string)
end linkSafariTabs

on linkSkim(theProtocol, theApp)
	tell application "Skim"
		set theScheme to "skim:"
		set theDoc to front document
		set theShortTitle to (name of theDoc)
		set theTitle to theShortTitle & ":" & theApp
		set thePath to (path of theDoc) & "::"
		set theSelection to selection of theDoc
		set theContent to contents of (get text for theSelection)
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
	end tell
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
		set escTitle to encodeURIComponent(theTitle)
		set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & escShortTitle
	end if
end linkApplication