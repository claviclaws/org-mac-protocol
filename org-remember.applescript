(*
org-remember.scpt --- make a note in an org-mode file, linking to the front
document and any selected text

Author: Christopher Suckling: suckling AT gmail DOT com

Version: 0.609

Commentary

Part of org-annotation-quicksilver

Currently supported applications:

Finder
Safari
Mail
Skim
BibDesk
mutt (using mairix as an index and runinng in a Termail.app window)

If you run the script on an unsupported, but AppleScript aware application, the script
attempt to create a link to that document (but not selected content), but may not be
successful. 

Syntax

Text processed by Quicksilver:

[blank]

Append link and selected content using *remember* template "QS Inbox"

foobar

Append note, link and selected content using *remember* template "QS Inbox"

foobar::x

Append note, link and selected content using *remember* template x

::x

Raise Emacs and initialize a *remember* buffer containing link and
selected content using *remember* template x

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/

2) Restart Quicksilver

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)

using terms from application "Quicksilver"
	on process text theText
		
		set AppleScript's text item delimiters to "::"
		if (count text items in theText) = 0 then
			set theTemplate to "q"
			set theFunction to "org-annotation-helper-file"
			set raiseEmacs to false
		else
			if (count text items in theText) = 1 then
				set theFunction to "org-annotation-helper-file"
				set raiseEmacs to false
				set theTemplate to "q"
			else
				if (count text items in theText) = 2 then
					set theTemplate to last text item in theText
					set theText to first text item in theText
					if theText is "" then
						set theFunction to "org-annotation-helper-remember"
						set raiseEmacs to true
					else
						set theFunction to "org-annotation-helper-file"
						set raiseEmacs to false
					end if
				end if
			end if
		end if
		set AppleScript's text item delimiters to ""
		
		tell application "System Events"
			set theApp to item 1 of (get name of processes whose frontmost is true)
		end tell
		
		if (theApp as string) = "Safari" then
			tell application "Safari"
				set theURL to do JavaScript "document.URL" in document 1
				set theTitle to do JavaScript "escape(document.title)" in document 1
				set theContent to do JavaScript "escape(window.getSelection())" in document 1
				
				set theLink to "remember://" & theURL & "::remember::" & theTitle & "::remember::" & theContent
			end tell
			
		else
			
			if (theApp as string) = "Skim" then
				tell application "Skim"
					set theDoc to front document
					set theTitle to name of theDoc
					set thePath to path of theDoc
					set theSelection to selection of theDoc
					set theContent to get text for theSelection
					set theLink to "remember://" & "file:/" & thePath & "::remember::" & theTitle & "::remember::" & theContent
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
						set theLink to "remember://" & "file:/" & thePath & "::" & theCite & "::remember::" & theTitle & "::" & theCite & "::remember::" & theContent
					end tell
					
				else
					
					if (theApp as string) = "Mail" then
						tell application "Mail"
							
							set theSelection to selection
							repeat with theMessage in theSelection
								set theID to message id of theMessage
								set thesubject to subject of theMessage
							end repeat
							set theLink to "remember://" & "message://" & theID & "::remember::" & thesubject & "::remember::"
						end tell
						
					else
						
						if (theApp as string) = "Finder" then
							tell application "Finder"
								set theItem to selection as alias
								set thePath to POSIX path of theItem
								set theTitle to name of (get info for theItem)
								set theLink to "remember://" & "file:/" & thePath & "::remember::" & theTitle & "::remember::"
							end tell
							
						else
							
							tell application (theApp as string)
								set theDoc to front document
								set theTitle to name of theDoc
								set thePath to path of theDoc
								set theLink to "remember://" & "file:/" & thePath & "::remember::" & theTitle & "::remember::"
							end tell
							
						end if
					end if
				end if
			end if
		end if
		
		
		
		set noAnnotation to false
		
		set theScript to "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e \"(progn (" & theFunction & " \\\"" & theLink & "\\\" \\\"" & theText & "\\\" \\\"" & theTemplate & "\\\" \\\"" & noAnnotation & "\\\") nil)\""
		
		do shell script theScript
		
		if raiseEmacs is true then
			tell application "Emacs"
				activate
			end tell
		end if
		
		close "QuickSilver"
		
	end process text
end using terms from
