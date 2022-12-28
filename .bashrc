#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias l='exa --icons'
alias la='exa -a --icons'
alias ll='exa -l --icons'
alias lla='exa -la --icons'

alias w='wiki-search'
alias wh='wiki-search-html'

alias p='sudo pacman -S'
alias ps='sudo pacman -sS'
alias py='sudo pacman -Sy'
alias pr='sudo pacman -R'
alias puy='sudo pacman -Suy'
alias pU="sudo pacman -U" 

alias t='tmux'
alias ta='tmux a'
alias tas='tmux a -t'
alias ts='tmux ls'
alias tk='tmux kill-session -t'

alias nw='nmcli dev wifi'
alias nc='nmcli dev wifi con'
alias ns='nmcli dev status'

alias y='yay -S '
alias yr='yay -R'
alias yy='yay -Sy'
alias ys='yay -sS'
alias yuy='yay -Suy'

alias m='mkdir -p'

alias cl='clear'

alias c='cp -rv'
alias sc='sudo cp -rv'

alias f='sudo fdisk -l'
alias lb='lsblk'

alias gc='git clone'
alias gs='git status'
alias gp='git push'
alias gP='git pull'
alias gi='git init'
alias gS='git stash'

# alias z='z.sh'

PS1='[\u@\h \W]\$ '

/usr/share/z/z.sh
export PATH=/home/gbl13/.local/bin:/usr/share/z:/home/gbl13/.guix-profile/bin:$PATH


# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

GUIX_PROFILE="/home/gbl13/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
