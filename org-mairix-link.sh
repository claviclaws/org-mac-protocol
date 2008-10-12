#!/bin/bash

# org-mairix-link.sh --- Parse headers extracted from selected message in mutt; pass them to emacsclient in as an org-mode link.

# Author: Christopher Suckling: suckling AT gmail DOT com

# Installation

# 1) Copy org-mairix-link.sh to directory of your choice

# 2) Add the following macro to your .muttrc

# macro index,pager "I" "<pipe-message>/usr/bin/formail -X Message-ID -X Subject | cut -d\">\" -f1 | tr -d \"<\" | ~/.mutt/org-mairix-link.sh\n"

headers=$(mktemp /tmp/messageID.XXXXXXXXXXXX) || exit 1
cat > $headers

messageID=$(grep '^Message-[Ii][Dd]' $headers | sed -e 's/Message-[Ii][Dd]: //g')
subject=$(grep '^Subject' $headers | sed -e 's/Subject: //g')

 /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e "(progn (org-annotation-helper-remember \"annotation://mairix:t:m:$messageID\::remember::$subject\") nil)"

trap 'rm -f $headers' EXIT