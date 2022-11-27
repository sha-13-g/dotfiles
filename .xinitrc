#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi



xhost +SI:localuser:$USER

xss-lock -- slock &

lxpolkit &
xrdb ~/Git_repos/dotfiles/.emacs.d/exwm/Xresources 

exec picom &
exec nitrogen --restore &

setxkbmap -layout 'fr' -variant 'us-azerty' -model 'pc105' -option 'ctrl:swapcaps' 

exec dbus-launch --exit-with-session emacs -mm --debug-init
