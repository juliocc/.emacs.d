#!/bin/bash

#pbd=/usr/libexec/PlistBuddy
emacsapp=`brew --prefix --installed emacs-plus@29`/Emacs.app
infoplist="${emacsapp}/Contents/Info.plist"

PlistBuddy -c "Add :LSEnvironment dict" "${infoplist}"
PlistBuddy -c "Add :LSEnvironment:PATH string" "${infoplist}"
PlistBuddy -c "Set :LSEnvironment:PATH $(echo "$PATH")" "${infoplist}"
PlistBuddy -c "Print :LSEnvironment" "${infoplist}"
touch ${emacsapp}
