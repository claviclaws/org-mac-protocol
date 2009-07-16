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

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/

2) Restart Quicksilver

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)

property orgQSLib : ((path to home folder as text) & "Library:Application Support:Quicksilver:Actions:orgQSLib:")
property getEmacsLib : load script ((orgQSLib as text) & "getEmacsClient.scpt") as alias
property getItemMetaLib : load script ((orgQSLib as text) & "getItemMetadata.scpt") as alias
property theProtocol : "org-protocol:/mac-remember:/y/"

tell application "System Events"
	set theApp to item 1 of (get name of processes whose frontmost is true)
end tell

set theScript to getEmacsLib's getEmacsClient() & " " & getItemMetaLib's getItemMetadata(theProtocol, theApp)

do shell script theScript
