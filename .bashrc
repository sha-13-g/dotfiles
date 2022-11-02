#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias l='exa -l --icons'
alias la='exa -la --icons'

alias p='sudo pacman -S'
alias ps='sudo pacman -sS'
alias py='sudo pacman -Sy'
alias pr='sudo pacman -R'
alias puy='sudo pacman -Suy'

alias y='yay -S '
alias yy='yay -Sy'
alias yr='yay -R'
alias yuy='yay -Suy'

alias m='mkdir -p'

alias cl='clear'

alias c='cp -rv'
alias sc='sudo cp -rv'

alias f='sudo fdisk -l'

alias gc='git clone'
alias gs='git status'
alias gp='git push'
alias gP='git pull'

# alias z='z.sh'

PS1='[\u@\h \W]\$ '

/usr/share/z/z.sh
export PATH=/home/gbl13/.local/bin/:/usr/share/z/:$PATH

# Automatically added by the Guix install script.
if [ -n "$GUIX_ENVIRONMENT" ]; then
    if [[ $PS1 =~ (.*)"\\$" ]]; then
        PS1="${BASH_REMATCH[1]} [env]\\\$ "
    fi
fi

