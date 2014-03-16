# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme zish

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
set fish_plugins autojump bundler git tmux

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

set -x GOROOT /usr/local/go
set -x GOBIN $GOROOT/bin
set -x GOPATH $HOME/development/gocode

set -x PAGER="most"
set -x EDITOR="emacsclient -c -t -a=''"
set -x WATCH="all"
set -x LANG="en_US.UTF-8"
set -x TERM="xterm-256color"
set -x GPGKEY=CB6E3FF3

alias ls='ls -lh --color=auto'
alias emacs="emacsclient -t -c -a=''"
alias gocov="sudo -E ~/goroot/bin/gocov test -deps -exclude-goroot . | gocov report"
alias rm="rm -v"
alias grep="grep --color=auto -n"

function clean --description "Remove unwanted temp files"
    find ./$argv[1] -name 'flymake_*.go' -delete
    find ./$argv[1] -name '.flymake_*.go' -delete
    find ./$argv[1] -name '.\#*' -delete
    find ./$argv[1] -name '*~' -delete
    find ./$argv[1] -name '*.orig' -delete
end
