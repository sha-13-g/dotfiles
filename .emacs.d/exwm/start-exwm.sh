# #! /bin/sh
# #xss-lock -- bash "~/lock.sh"
# # -layout 'fr' -variant 'us-azerty' -model 'pc105' # 

# userresources=$HOME/.Xresources
# usermodmap=$HOME/.Xmodmap
# sysresources=/etc/X11/xinit/.Xresources
# sysmodmap=/etc/X11/xinit/.Xmodmap

# # merge in defaults and keymaps

# if [ -f $sysresources ]; then







#     xrdb -merge $sysresources

# fi

# if [ -f $sysmodmap ]; then
#     xmodmap $sysmodmap
# fi

# if [ -f "$userresources" ]; then







#     xrdb -merge "$userresources"

# fi

# if [ -f "$usermodmap" ]; then
#     xmodmap "$usermodmap"
# fi

# # start some nice programs

# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#   [ -x "$f" ] && . "$f"
#  done
#  unset f
# fi

exec setxkbmap -option 'ctrl:swapcaps'
exec picom &
exec nitrogen --restore

#dbus-launch --exit-with-session

exec emacs -mm --debug-init -l ~/.emacs.d/gbl-modules/gbl-emacs-desktop.el
