#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export PURE_PROMPT_SYMBOL='>'
export GOPATH=$HOME/go
export PATH=$PATH:$HOME/bin:$GOPATH/bin:$HOME/npm/bin

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

#bindkey "^[[A" history-search-backward
#bindkey "^[[B" history-search-forward
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

#alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS

alias q='rlwrap $HOME/q/l32/q'
alias e='emacsclient -n'
alias reset='printf "\033c"'
alias lerna='npx lerna'
alias sdc='npx shadow-cljs'
