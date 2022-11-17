#!/usr/bin/bash

sudo pacman -Sy
sudo pacman -S cantarell-fonts ttf-fira-code ttf-jetbrains-mono cmake nodejs npm pyqt5  networkmanager intel-ucode alsa-utils polkit picom nitrogen alacritty z xorg-apps xorg-server arandr alsa-utils exa apache nginx youtube-dl xdg-utils alsa xdg-user-dirs xdg-utils lxsessions

git config --global user.name "Ganfina Brice"
git config --global user.email "ganfinab@gmail.com"

mkdir -p git_repos/

sudo git clone https://aur.archlinux.com/yay
(cd git_repos/yay && makepkg -si)

yay -Sy ly youtubedl-gui
systemctl enable ly.service
npm install -g typescript-languages-servers

(cd git_repos/ && git clone https://github.com/sha-13-g/org)

systemctl start NetworkManager.service
systemctl enable ly.service
systemctl disable getty@tty2.service

sudo ln -s ~/git_repos/dotfiles/.emacs.d/exwm/exwm.desktop /usr/share/xsessions/
sudo ln -s ~/git_repos/dotfiles/.emacs.d/exwm/exwm /usr/bin/
sudo ln -s ~/git_repos/dotfiles/etc/X11/xorg.conf.d/00-keyboard.conf /etc/X11/xorg.conf.d/
sudo ln -s ~/git_repos/dotfiles/.bashrc ~/
sudo ln -s ~/git_repos/dotfiles/.config/ ~/.config
sudo ln -s ~/git_repos/dotfiles/.emacs.d/ .emacs.d

sudo cp -v /etc/X11/xinit/xinitrc ~/.xinitrc

xdg-setting set default-web-browser org.qutebrowser.qutebrowser.desktop
