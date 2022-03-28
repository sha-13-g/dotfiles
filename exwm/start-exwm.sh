#! /bin/sh


nitrogen --restore
picom &
exec dbus-launch --exit-with-session emacs -mm --debug-init
