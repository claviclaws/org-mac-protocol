(*
getItemMetadate.scpt --- retrieve information about selected items and create links to them

Author: Christopher Suckling <suckling AT gmail DOT com>

Vesion: 0.624

Commentary

Part of org-annotation-quicksilver

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/orgQSLib/

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)


on getItemMetadata(theProtocol, theApp)
	if (theApp as string) = "Safari" then
		tell application "Safari"
			set theURL to do JavaScript "document.URL" in document 1
			set theTitle to do JavaScript "escape(document.title)" in document 1
			set theContent to do JavaScript "escape(window.getSelection())" in document 1
			
			set theLink to theProtocol & theURL & "::remember::" & theTitle & "::remember::" & theContent
		end tell
		
	else
		
		if (theApp as string) = "Skim" then
			tell application "Skim"
				
				set theDoc to front document
				set theTitle to name of theDoc
				set thePath to path of theDoc
				set theSelection to selection of theDoc
				set theContent to get text for theSelection
				set theLink to theProtocol & "file:/" & thePath & "::remember::" & theTitle & "::remember::" & theContent
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
					set theDoc to front document
					set theTitle to name of theDoc
					set thePath to path of theDoc
					set theSelection to the selection of theDoc
					set thePub to item 1 of theSelection
					set theContent to templated text of theDoc using text templateText for thePub
					set theCite to cite key of thePub
					set theLink to theProtocol & "file:/" & thePath & "::" & theCite & "::remember::" & theTitle & "::" & theCite & "::remember::" & theContent
				end tell
				
			else
				
				if (theApp as string) = "Mail" then
					tell application "Mail"
						
						set theSelection to selection
						repeat with theMessage in theSelection
							set theID to message id of theMessage
							set thesubject to subject of theMessage
						end repeat
						set theLink to theProtocol & "message://" & theID & "::remember::" & thesubject & "::remember::"
					end tell
					
					
				else
					
					if (theApp as string) = "Finder" then
						tell application "Finder"
							set theItem to selection as alias
							set thePath to POSIX path of theItem
							set theTitle to name of (get info for theItem)
							set theContent to ""
							
							set theLink to theProtocol & "file:/" & thePath & "::remember::" & theTitle & "::remember::"
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
								set theDoc to front document
								set theTitle to name of theDoc
								set thePath to path of theDoc
								set theLink to theProtocol & "file:/" & thePath & "::remember::" & theTitle & "::remember::"
							end tell
							
						end if
					end if
				end if
			end if
		end if
	end if
	return theLink
end getItemMetadata