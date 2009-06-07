(*
getItemMetadate.scpt --- retrieve information about selected items and create links to them

Author: Christopher Suckling <suckling AT gmail DOT com>

Vesion: 0.627

Commentary

Part of org-annotation-quicksilver

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/orgQSLib/

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)


on getItemMetadata(theProtocol, theApp)
	if (theApp as string) = "Safari" then
		tell application "Safari"
			set theURL to do JavaScript "encodeURIComponent(document.URL)" in document 1
			set theTitle to do JavaScript "encodeURIComponent(document.title)" in document 1
			set theContent to do JavaScript "encodeURIComponent(window.getSelection())" in document 1
			
			set theLink to theProtocol & theURL & "/" & theTitle & "/" & theContent
		end tell
		
	else
		
		if (theApp as string) = "Skim" then
			tell application "Skim"
				
				set theScheme to "file:/"
				set theDoc to front document
				set theTitle to name of theDoc
				set thePath to path of theDoc
				set theSelection to selection of theDoc
				set theContent to get text for theSelection
				
				set escScheme to URI Escape theScheme
				set escTitle to URI Escape theTitle additional "/"
				set escPath to URI Escape thePath additional "/"
				set escContent to URI Escape theContent additional "/"
				
				set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & escContent
			end tell
			
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
					set theScheme to "file:/"
					set theDoc to front document
					set theTitle to name of theDoc
					set thePath to path of theDoc
					set theSelection to the selection of theDoc
					set thePub to item 1 of theSelection
					set theContent to templated text of theDoc using text templateText for thePub
					set theCite to cite key of thePub
					
					set escScheme to URI Escape theScheme
					set escTitle to URI Escape theTitle additional "/"
					set escCite to URI Escape theCite additional "/"
					set escPath to URI Escape thePath additional "/"
					set escContent to URI Escape theContent additional "/"
					
					set theLink to theProtocol & "file:/" & escPath & "::" & escCite & "/" & escTitle & "::" & escCite & "/" & escContent
				end tell
				
			else
				
				if (theApp as string) = "Mail" then
					tell application "Mail"
						
						set theSelection to selection
						repeat with theMessage in theSelection
							set theID to message id of theMessage
							set thesubject to subject of theMessage
						end repeat
						
						set theScheme to "message://"
						
						set escID to URI Escape theID additional "/"
						set escSubject to URI Escape thesubject additional "/"
						set escScheme to URI Escape theScheme additional "/"
						
						set theLink to theProtocol & escScheme & theID & "/" & thesubject
					end tell
					
					
				else
					
					if (theApp as string) = "Finder" then
						tell application "Finder"
							set theScheme to "file:/"
							set theItem to selection as alias
							set thePath to POSIX path of theItem
							set theTitle to name of (get info for theItem)
							
							set escScheme to URI Escape additional "/"
							set escPath to URI Escape additional "/"
							set escTitle to URI Escape additional "/"
							
							set theLink to theProtocol & escScheme & escPath & "/" & escTitle
						end tell
						
					else
						
						if (theApp as string) = "Terminal" then
							tell application "Terminal"
								if get processes of window 1 contains "mutt" then
									activate
									tell application "System Events" to tell process "Terminal" to keystroke "I"
								end if
							end tell
							
						else
							
							tell application (theApp as string)
								set theScheme to "file:/"
								set theDoc to front document
								set theTitle to name of theDoc
								set thePath to path of theDoc
								
								set escScheme to URI Escape theScheme additional "/"
								set escPath to URI Escape thePath additional "/"
								set escTitle to URI Escape theTitle additional "/"
								
								set theLink to theProtocol & escScheme & escPath & "/" & escTitle
							end tell
							
						end if
					end if
				end if
			end if
		end if
	end if
	return theLink
end getItemMetadata