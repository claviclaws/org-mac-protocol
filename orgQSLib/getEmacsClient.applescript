(*
getEmacsClient.scpt --- set the path to emacsclient

Author: Christopher Suckling <suckling AT gmail DOT com>

Vesion: 0.624

Commentary

Part of org-annotation-quicksilver

Installation

1) Copy to ~/Library/Application Support/Quicksilver/Actions/orgQSLib/

2) Edit the below path to emacsclient

Please see org-annotation-quicksilver.org for full installation and usage instructions
*)

on getEmacsClient()
	set theEmacsClient to "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
	return theEmacsClient
end getEmacsClient