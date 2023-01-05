#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias l='exa --icons'
alias lt='exa --tree --icons'
alias la='exa -a --icons'
alias lat='exa -a --tree --icons'
alias ll='exa -l --icons'
alias llt='exa -l --tree --icons'
alias lla='exa -la --icons'
alias llat='exa -la --tree --icons'

alias w='wiki-search'
alias wh='wiki-search-html'

alias p='sudo pacman -S'
alias pS='sudo pacman -sS'
alias py='sudo pacman -Sy'
alias pr='sudo pacman -R'
alias puy='sudo pacman -Suyy'
alias pU="sudo pacman -U" 

alias t='tmux'
alias ta='tmux a'
alias tas='tmux a -t'
alias ts='tmux ls'
alias tk='tmux kill-session -t'
alias tn='tmux new -s'

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

alias gc='git commit -a'
alias gC='git clone'
alias gs='git status'
alias ga='git add'
alias gs='git status'
alias gp='git push'
alias gP='git pull'
alias gi='git init'
alias gS='git stash'

# alias z='z.sh'

PS1='[\u@\h \W]\$ '

# /usr/share/z/z.sh
# export PATH=/home/gbl13/.local/bin:/usr/share/z:/home/gbl13/.guix-profile/bin:$PATH

