# -*- mode: sh -*-

fpath=(/usr/local/share/zsh-completions $fpath)

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="simple"
# Use case sensitive completion.
CASE_SENSITIVE="true"
# Disable auto-correct.
DISABLE_CORRECTION="true"
# Autostart tmux.
[ -z "$ZSH_TMUX_AUTOSTART" ] && ZSH_TMUX_AUTOSTART="true"
# Autostart docker-machine default.
DOCKER_MACHINE_AUTOSTART="true"
DOCKER_MACHINE_NAME="default"

plugins=(
    encode64
    git
    gnu-utils
    golang
    gpg-agent
    mosh
    rsync
    ssh-agent
    sudo
    tmux
)

## User configuration
function main() {
    # Set M-l as lowercase word.
    bindkey "^[l" down-case-word
    # Disable shared history.
    unsetopt share_history

    # Set host metadata.
    [ -z "$LANG" ]     && export LANG=en_US.UTF-8
    [ -z "$HOSTTYPE" ] && export HOSTTYPE=$(uname -s)
    [ -z "$HOST" ]     && export HOST=$(uname -n)
    [ -z "$SHELL" ]    && export SHELL=$(which zsh)

    # Set GPGKEY if exists.
    [ -f $HOME/.gpgkey ] && export GPGKEY=$(cat $HOME/.gpgkey)

    # Set docker-cloud namespace if exists.
    [ -f $HOME/.dockercloudorg ] && export DOCKERCLOUD_NAMESPACE=$(cat $HOME/.dockercloudorg)

    if [ "$HOSTTYPE" = "Darwin" ]; then
	export MANPATH="/usr/local/man:$MANPATH"
	export LSCOLORS="ExFxCxDxBxegedabagacad"
    fi

    # Use "most" as pager. Better than "less" or "more".
    export PAGER="most"
    # Set editor to emacs.
    export EDITOR="emacs"

    # Helper funciton to delete all temporary / cache files.
    # Usage: clean [path]
    function clean {
	foreach tildefile (./${1}/*~(.N) ./${1}/.*~(.N) ./${1}/\#*\#(.N) ./${1}/.\#*\#(.N) ./${1}/a.out(.N))
	rm -vf ${tildefile} | sed 's/\/\//\//'
	end

	find ./${1} -name 'flymake_*.go' -delete
	find ./${1} -name '.flymake_*.go' -delete
	find ./${1} -name '.\#*' -delete
	find ./${1} -name '*~' -delete
	find ./${1} -name '*.orig' -delete
	find ./${1} -name '*.test' -delete
    }

    alias emacs="emacsclient -a ''  -ct"
    alias grep="grep --color=auto -n"
    alias rm="rm -v"
    alias a64="encode64"
    alias d64="decode64"
    alias bc="bc -l $HOME/.bcrc"
}

# Helper to fetch current time with ms.
function mstime() {
    # Get the time in "ns" and trim down the last 6 digits.
    echo "$(gdate +%s.%N | sed 's/......$//')" | \bc
}

function timed() {
    name=$1
    shift
    cmd=$1
    shift
    echo "[$name] Loading..."
    a=$(mstime)
    $cmd $@
    b=$(mstime)
    echo "[$name] Loaded in $(echo "($b - $a)" | \bc) ms"
}

# Save cursor position.
tput sc
echo

# Load oh-my-zsh.
timed "oh-my-zsh" source $ZSH/oh-my-zsh.sh

# Load powerline.
timed "powerline" source /usr/local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# Load main user config.
timed "user config" main

# Load docker-machine "plugin".
[ -f "$HOME/.zsh_docker" ] && timed "docker config" source $HOME/.zsh_docker

# Load golang config.
[ -f "$HOME/.zsh_golang" ] && timed "golang config" source $HOME/.zsh_golang

# Load private config if exists.
[ -f "$HOME/.zsh_priv_config" ] && timed "local user config" source $HOME/.zsh_priv_config

# Restore cursor & clear line.
tput rc; tput el

function igo() {
    tmp=$(mktemp); mv $tmp $tmp.go; echo "package main\nfunc main() {" > $tmp.go; cat >> $tmp.go; echo "}" >> $tmp.go; goimports -w $tmp.go; go run $tmp.go; \rm $tmp.go
}
