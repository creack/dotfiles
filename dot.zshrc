#!/bin/zsh
ulimit -c 0

#if [ $TERM != "screen" ] && [[ -z "$TMUX" ]]; then
#   export TERM=xterm-256color
#   exec tmux a
#fi

export HOSTTYPE=`uname -s`
export HOST=`uname -n`
export SHELL="/bin/zsh"
export PAGER="most"
export EDITOR="emacsclient -c -t -a=''"
export WATCH="all"
export LANG="en_US.UTF-8"
export TERM="xterm-256color"
export GPGKEY=CB6E3FF3

function clean {
    foreach tildefile (./${1}/*~(.N) ./${1}/.*~(.N) ./${1}/\#*\#(.N) ./${1}/.\#*\#(.N) ./${1}/a.out(.N))
    rm -vf ${tildefile} | sed 's/\/\//\//'
    end

    find ./${1} -name '.\#*' -delete
    find ./${1} -name '*~' -delete
}

function gogrep() {
    grep -R $1 `find . -name '*.go'`
}

setprompt ()
{
    setopt prompt_subst
    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
(( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"
    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}

    case $TERM in
xterm*)
    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
    ;;
screen*)
    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
    ;;
*)
        PR_TITLEBAR=''
    ;;
    esac


    ###
    # Decide whether to set a screen title
    if [[ "$TERM" == "screen" ]]; then
PR_STITLE=$'%{\ekzsh\e\\%}'
    else
PR_STITLE=''
fi

PROMPT="
($PR_BLUE%n$PR_NO_COLOUR@$PR_GREEN%m$PR_NO_COLOUR):<$PR_CYAN%~$PR_NO_COLOUR>
[$PR_RED%*$PR_NO_COLOUR]$ "

RPROMPT="$(git_super_status)"

}


# Ls aliases
alias l="ls -l"
alias ll="ls -l"
alias la="ls -lA"
alias lsh="ls -h"
alias lh="ls -lh"
alias llh="ls -lh"
alias lah="ls -lAh"
alias ls="ls --color=auto"
alias rm="rm -v"
alias grep="grep --color=auto -n"
# Other aliases
alias ne="emacs"
alias ciao="kill -9 -1"
alias emacs="emacsclient -t -c -a=''"
alias gocov="sudo -E ~/goroot/bin/gocov test -deps -exclude-goroot . | gocov report"

hosts=(`grep ^Host ~/.ssh/config | sed s/Host\ //`)
hosts=($hosts `awk '{print $1}' ~/.ssh/known_hosts | tr ',' ' ' | tr -d '['  | tr -d ']'`)

# The following lines were added by compinstall
zstyle ':completion:*:hosts' hosts $hosts
zstyle ':completion:*' completer _expand _complete _approximate
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z} m:{a-zA-Z}={A-Za-z}' 'l:|=* r:|=*' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' menu select=long-list select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/$USER/.zshrc'

autoload -Uz compinit
compinit

source ~/.zsh_go
source ~/.zsh_git_prompt

setprompt


# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd nomatch
unsetopt beep extendedglob notify
bindkey -e
# End of lines configured by zsh-newuser-install

## Use color in completion
zstyle ':completion:*' list-colors ${(s.:.)LSCOLORS}


#switch ± and ~ (macbookpro)
eval "insert-key-tilde () { LBUFFER+='~'; }"
eval "insert-key-magicquote () { LBUFFER+='\`'; }"

# OSX specific
zle -N insert-key-tilde
bindkey ± insert-key-tilde

zle -N insert-key-magicquote
bindkey § insert-key-magicquote
export GOPATH=~/go
export GOROOT=~/goroot
export GOBIN=$GOROOT/bin
export PATH=$GOBIN:$PATH
export GOPATH=~/go
export GOROOT=~/goroot
export GOBIN=$GOROOT/bin
export PATH=$GOBIN:$PATH
