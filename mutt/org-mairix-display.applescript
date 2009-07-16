(*
org-mairix-display --- OS X 10.5 only. Handles links passed by org-mairix.el

Author: Christopher Suckling: suckling AT gmail DOT com

Version: 0.609

Commentary

Looks for a Terminal.app window running a mutt process; if found, opens a new tab
and launches a new mutt process displaying the mairix query.

If no mutt process running, opens a new Terminal.app window and launchs a mutt process 
displaying the mairix query.

Requirements

OS X 10.5
mutt
mairix
org-mairix.el
Optional: mutt.terminal (included in this archive)

Installation

1) Copy this script to directory of your choice.

2) Either:

Import mutt.terminal settings into Terminal.app
(Terminal - Preferences - Settings - Import)

Or:

Edit all occurances of 

set current settings of front window to settings set "mutt"
set bounds of front window to {20, 10, 1160, 775}

to your own tastes.

3) Edit all occurances of

~/Library/Maildir/mairix-search

to the path to your mairix results maildir.

4) Add the following to your .emacs:

(setq org-mairix-open-command "mairix %args% %search%")
(setq org-mairix-threaded-links t)
(setq org-mairix-mutt-display-command "osascript /pathto/org-mairix-display.scpt %search%")
(setq org-mairix-display-hook 'org-mairix-mutt-display-results)

*)

on run argv
	tell application "System Events"
		if not (exists process "Terminal") then
			tell application "Terminal"
				activate
			end tell
		end if
		repeat while not (exists process "Terminal")
			delay 0.5
		end repeat
	end tell
	tell application "Terminal"
		if (count of windows) is greater than 0 then
			repeat with i from 1 to count of windows
				if get processes of window i contains "mutt" then
					activate
					set miniaturized of window i to false
					tell application "System Events" to tell process "Terminal" to keystroke "t" using command down
					delay 1
					set current settings of front window to settings set "mutt"
					set bounds of front window to {20, 10, 1160, 775}
					set custom title of front window to "mairix query: " & item 1 of argv
					do script "mutt -e \"push <display-message>\" -f ~/Library/Maildir/mairix-search" in window 1
					exit repeat
				else
					do script "mutt -e \"push <display-message>\" -f ~/Library/Maildir/mairix-search"
					set current settings of front window to settings set "mutt"
					set bounds of front window to {20, 10, 1160, 775}
					set custom title of front window to "mairix query: " & argv
					exit repeat
				end if
			end repeat
		else
			do script "mutt -e \"push <display-message>\" -f ~/Library/Maildir/mairix-search"
			set current settings of front window to settings set "mutt"
			set bounds of front window to {20, 10, 1160, 775}
			set custom title of front window to "mairix query: " & argv
		end if
	end tell
end run

