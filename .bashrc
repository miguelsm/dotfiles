#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias ssh-x='ssh -c arcfour,blowfish-cbc -YC'
alias suspend='systemctl suspend'
alias tmux='TERM=xterm-256color tmux'

export EDITOR=zile

#PS1='[\u@\h \W]\$ '
PS1='\[$(tput bold)\]\[$(tput setaf 2)\][\u@\h \W]\\$ \[$(tput sgr0)\]'
