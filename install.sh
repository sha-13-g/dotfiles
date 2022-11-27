#!/usr/bin/bash

mkdir -p ~/Git_repos/
mv ~/dotfiles ~/Git_repos
(cd ~/Git_repos && git clone https://aur.archlinux.org/yay)
(cd ~/Git_repos/yay && makepkg -si)
(cd ~/Git_repos/ && git clone --recurse-submodules https://github.com/fairyglade/ly)
(cd ~/Git_repos/ly/ && make && make install installsystemd)


sudo pacman -S cantarell-fonts ttf-fira-code ttf-jetbrains-mono cmake nodejs npm pyqt5  networkmanager intel-ucode alsa-utils polkit picom nitrogen alacritty z xorg-apps xorg-server arandr alsa-utils exa apache nginx youtube-dl xdg-utils alsa xdg-user-dirs xdg-utils lxsession man mpv ttf-sourcecodepro-nerd ttf-hack-nerd firefox firefox slock xss-lock

git config --global user.name "Ganfina Brice"
git config --global user.email "ganfinab@gmail.com"

xdg-user-dirs-update

#(cd ~/Documents && git clone https://github.com/sha-13-g/org)

#systemctl start networkmanager.service
#systemctl enable ly.service
#systemctl disable getty@tty2.service

rm -rvf ~/.bashrc
rm -rvf ~/.bash_profile 

sudo ln -sv ~/Git_repos/dotfiles/.emacs.d/exwm/exwm.desktop /usr/share/xsessions/
sudo ln -sv ~/Git_repos/dotfiles/.bashrc ~/
sudo ln -sv ~/Git_repos/dotfiles/.bash_profile ~/
sudo ln -sv ~/Git_repos/dotfiles/.config/ ~/.config
sudo ln -sv ~/Git_repos/dotfiles/.emacs.d/ .emacs.d
sudo ln -sv ~/Git_repos/dotfiles/.xinitrc ~/

systemctl enable ly.service

xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop

yay -S chromium-widevine
#systemctl enable ly.service
npm install -g typescript-language-server typescript
npm i -g vscode-langservers-extracted
