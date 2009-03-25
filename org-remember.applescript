(*
org-remember.scpt --- make a note in an org-mode file, linking to the front
document and any selected text

Author: Christopher Suckling <suckling AT gmail DOT com>

Version: 0.624

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

Raise Emacs and initialize a *remember* buffer containing link and selected content using
*remember* template x

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/

2) Restart Quicksilver

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)

property orgQSLib : ((path to home folder as text) & "Library:Application Support:Quicksilver:Actions:orgQSLib:")
property getEmacsLib : load script ((orgQSLib as text) & "getEmacsClient.scpt") as alias
property getItemMetaLib : load script ((orgQSLib as text) & "getItemMetadata.scpt") as alias
property theProtocol : "remember://"

using terms from application "Quicksilver"
	on process text theText
		
		set AppleScript's text item delimiters to "::"
		if (count text items in theText) = 0 then
			set theTemplate to "q"
			set rememberFile to "file"
			set raiseEmacs to false
		else
			if (count text items in theText) = 1 then
				set rememberFile to "file"
				set raiseEmacs to false
				set theTemplate to "q"
			else
				if (count text items in theText) = 2 then
					set theTemplate to last text item in theText
					set theText to first text item in theText
					if theText is "" then
						set rememberFile to "remember"
						set raiseEmacs to true
					else
						set rememberFile to "file"
						set raiseEmacs to false
					end if
				end if
			end if
		end if
		set AppleScript's text item delimiters to ""
		
		tell application "System Events"
			set theApp to item 1 of (get name of processes whose frontmost is true)
		end tell
		
		
		set noAnnotation to false
		
		set theScript to getEmacsLib's getEmacsClient() & " -e \"(progn (org-annotation-helper-remember \\\"" & getItemMetaLib's getItemMetadata(theProtocol, theApp) & "\\\" \\\"" & theText & "\\\" \\\"" & theTemplate & "\\\" \\\"" & noAnnotation & "\\\" \\\"" & rememberFile & "\\\") nil)\""
		
		do shell script theScript
		
		if raiseEmacs is true then
			tell application "Emacs"
				activate
			end tell
		end if
		
		close "QuickSilver"
		
	end process text
end using terms from