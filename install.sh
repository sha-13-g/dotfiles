#!/usr/bin/bash
mkdir -pv ~/.config/ ~/documents ~/downloads ~/pictures ~/videos ~/images ~/desktop ~/.emacs.d ~/music/ ~/repos/

(cd ~/repos && git clone https://aur.archlinux.org/yay)
(cd ~/repos/yay && makepkg -si)

sudo pacman -S cantarell-fonts ttf-fira-code ttf-jetbrains-mono cmake nodejs npm pyqt5  networkmanager intel-ucode alsa-utils polkit picom nitrogen alacritty z xorg-apps xorg-server arandr alsa-utils exa apache nginx youtube-dl xdg-utils alsa xdg-user-dirs xdg-utils lxsession man mpv ttf-sourcecodepro-nerd ttf-hack-nerd firefox firefox slock xss-lock discord brightnessctl blueman isync notmuch scrot gparted rsync wireshark-cli wireshark-qt nmap npm network-manager-applet feh xclip tmux

git config --global user.name "Ganfina Brice"
git config --global user.email "ganfinab@gmail.com"

xdg-user-dirs-update

git clone https://github.com/sha-13-g/org

systemctl --now enable NetworkManager.service


rm -rvf ~/.bashrc
rm -rvf ~/.bash_profile 

sudo ln -fv ~/dotfiles/.emacs.d/exwm/exwm.desktop /usr/share/xsessions/
ln -sfv ~/dotfiles/.bashrc ~/
ln -sfv ~/dotfiles/.bash_profile ~/


ln -sfv ~/dotfiles/.config/* ~/.config/
ln -sfv ~/dotfiles/.emacs.d/* ~/.emacs.d/
ln -sfv ~/dotfiles/.xinitrc ~/

xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop

sudo npm install -g typescript-language-server typescript vscode-langservers-extracted
yay -S chromium-widevine xdman hakuneko-desktop google-chrome
