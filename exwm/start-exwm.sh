#! /bin/sh

picom -b 

exec dbus-launch --exit-with-session emacs -mm --debug-init
