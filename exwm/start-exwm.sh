#! /bin/sh
#xss-lock -- bash "~/lock.sh"
setxkbmap -option 'grp:shifts_toggle, ctrl:swapcaps' -layout 'fr' -variant 'us-azerty' -model 'pc105'
nitrogen --restore
picom &
exec dbus-launch --exit-with-session emacs --deamon --debug-init  
exec sleep 3
exec emacsclient -mm
