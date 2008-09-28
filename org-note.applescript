using terms from application "Quicksilver"
	on process text theText
		
		set AppleScript's text item delimiters to "::"
		if (count text items in theText) = 0 then
			return 0
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
		
		
		set theLink to "remember://http://dummy::remember::dummy::remember::"
		set noAnnotation to true
		
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