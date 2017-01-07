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

    # Customize the prompt a little.
    source ~/.zsh_git_prompt
    PROMPT='
(%{$fg_bold[blue]%}%n%{$reset_color%}@%{$fg_bold[green]%}%m%{$reset_color%}):<%{$fg_bold[cyan]%}%(5~|%-1~/…/%3~|%4~)%{$reset_color%}>
[%{$fg_bold[red]%}%D{%a %b %d %r}%{$reset_color%}]$(git_super_status)%{$reset_color%}%% '

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

    # Load docker-machine "plugin".
    [ -f "$HOME/.zsh_docker" ] && source $HOME/.zsh_docker

    # Load golang config.
    [ -f "$HOME/.zsh_golang" ] && source $HOME/.zsh_golang

    # Load private config if exists.
    [ -f "$HOME/.zsh_priv_config" ] && source $HOME/.zsh_priv_config
}

# Helper to fetch current time with ms.
function mstime() {
    # Get the time in "ns" and trim down the last 6 digits.
    echo "$(gdate +%s.%N | sed 's/......$//')" | \bc
}

# Save cursor position.
tput sc
echo

echo "Loading oh-my-sh..."
a=$(mstime)
source $ZSH/oh-my-zsh.sh
b=$(mstime)
echo "Loaded in $(echo "($b - $a)" | \bc) ms"

echo "Loading user config..."
a=$(mstime)
main
b=$(mstime)
echo "Loaded in $(echo "($b - $a)" | \bc) ms"

# Restore cursor & clear line.
tput rc; tput el
