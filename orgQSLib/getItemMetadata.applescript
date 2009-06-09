(*
getItemMetadata.scpt --- retrieve information about selected items and create links to them

Author: Christopher Suckling <suckling AT gmail DOT com>

Vesion: 0.627

Commentary

Part of org-annotation-quicksilver

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/orgQSLib/

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)


on getItemMetadata(theProtocol, theApp)
	set theErrorURL to POSIX path of (path to application theApp)
	set theErrorMessage to theApp & ": no AppleScript support"
	set escErrorURL to URI Escape theErrorURL additional "@#$%^&*(){}[]=:/,;?+"
	set escErrorMessage to URI Escape theErrorMessage additional "~!@#$%^&*(){}[]=:/,;?+"
	
	set escApp to URI Escape theApp additional "~!@#$%^&*(){}[]=:/,;?+"
	
	if (theApp as string) = "Safari" then
		tell application "Safari"
			set theURL to do JavaScript "document.URL" in document 1
			set theTitle to do JavaScript "document.title" in document 1
			set theContent to do JavaScript "window.getSelection()" in document 1
			
			set escURL to URI Escape theURL additional "~!@#$%^&*(){}[]=:/,;?+"
			set escTitle to URI Escape theTitle additional "~!@#$%^&*(){}[]=:/,;?+"
			set escContent to URI Escape theContent additional "~!@#$%^&*(){}[]=:/,;?+"
			
			set theLink to theProtocol & escURL & "/" & escTitle & "/" & escContent & ":" & escApp
		end tell
		
	else
		
		if (theApp as string) = "Skim" then
			tell application "Skim"
				
				set theScheme to "file:/"
				set theDoc to front document
				set theTitle to name of theDoc
				set thePath to path of theDoc
				set theSelection to selection of theDoc
				set theContent to contents of (get text for theSelection)
				
				set escScheme to URI Escape theScheme additional "~!@#$%^&*(){}[]=:/,;?+"
				set escTitle to URI Escape theTitle additional "~!@#$%^&*(){}[]=:/,;?+"
				set escPath to URI Escape thePath additional "~!@#$%^&*(){}[]=:/,;?+"
				set escContent to URI Escape theContent additional "~!@#$%^&*(){}[]=:/,;?+"
				
				set theLink to theProtocol & escScheme & escPath & "/" & escTitle & "/" & escContent & ":" & escApp
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
					set theTitle to (name of theDoc) & "::"
					set thePath to (path of theDoc) & "::"
					set theSelection to the selection of theDoc
					set thePub to item 1 of theSelection
					set theContent to templated text of theDoc using text templateText for thePub
					set theCite to cite key of thePub
					
					set escScheme to URI Escape theScheme additional "~!@#$%^&*(){}[]=:/,;?+"
					set escTitle to URI Escape theTitle additional "~!@#$%^&*(){}[]=:/,;?+"
					set escCite to URI Escape theCite additional "~!@#$%^&*(){}[]=:/,;?+"
					set escPath to URI Escape thePath additional "~!@#$%^&*(){}[]=:/,;?+"
					set escContent to URI Escape theContent additional "~!@#$%^&*(){}[]=:/,;?+"
					
					set theLink to theProtocol & escScheme & escPath & escCite & "/" & escTitle & escCite & "/" & escContent & ":" & "escApp"
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
						
						set escID to URI Escape theID additional "~!@#$%^&*(){}[]=:/,;?+"
						set escSubject to URI Escape thesubject additional "~!@#$%^&*(){}[]=:/,;?+"
						set escScheme to URI Escape theScheme additional "~!@#$%^&*(){}[]=:/,;?+"
						
						set theLink to theProtocol & escScheme & theID & "/" & thesubject & ":" & "escApp"
					end tell
					
					
				else
					
					if (theApp as string) = "Finder" then
						tell application "Finder"
							set theScheme to "file:/"
							set theItem to selection as alias
							set thePath to POSIX path of theItem
							set theTitle to name of (get info for theItem)
							
							set escScheme to URI Escape theScheme additional "~!@#$%^&*(){}[]=:/,;?+"
							set escPath to URI Escape thePath additional "~!@#$%^&*(){}[]=:/,;?+"
							set escTitle to URI Escape theTitle additional "~!@#$%^&*(){}[]=:/,;?+"
							
							set theLink to theProtocol & escScheme & escPath & "/" & escTitle & ":" & escApp
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
								set escScheme to URI Escape theScheme additional "~!@#$%^&*(){}[]=:/,;?+"
								set appUnsupported to false
								try
									set theDoc to front document
								on error
									set theLink to theProtocol & escScheme & escErrorURL & "/" & escErrorMessage
									set appUnsupported to true
								end try
								if appUnsupported is false then
									set theTitle to name of theDoc
									set thePath to path of theDoc
									set escPath to URI Escape thePath additional "~!@#$%^&*(){}[]=:/,;?+"
									set escTitle to URI Escape theTitle additional "~!@#$%^&*(){}[]=:/,;?+"
									set theLink to theProtocol & escScheme & escPath & "/" & escTitle & ":" & escApp
								end if
								
							end tell
							
						end if
					end if
				end if
			end if
		end if
	end if
	return theLink
end getItemMetadata