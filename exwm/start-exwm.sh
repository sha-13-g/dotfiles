#! /bin/sh
#xss-lock -- bash "~/lock.sh"
# -layout 'fr' -variant 'us-azerty' -model 'pc105' # 


setxkbmap -option 'grp:shifts_toggle, ctrl:swapcaps'

# Load compositor
picom &

# Set nice pointer cursor.
# xsetroot -cursor_name left_ptr &
# unclutter &

# Run dunst in background
# dunst &

# Load trayer and polybar
# polybar &

# trayer --edge top --align right --height 15 --width 5 --iconspacing 5 &

nitrogen --restore

# pa-applet &
# nm-applet &

exec dbus-launch --exit-with-session emacs -mm --debug-init

# picom &
# exec dbus-launch --exit-with-session emacs --deamon --debug-init  
# exec sleep 3
# exec emacsclient -mm
