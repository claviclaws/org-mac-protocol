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

