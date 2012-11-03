#!/bin/bash

BROWSER=$@
osascript -e '
	tell application "'"$BROWSER"'"
	    activate
	    tell application "System Events" to keystroke "r" using command down
	end tell
'