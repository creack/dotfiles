# -*- mode: sh -*-

fpath=(/usr/local/share/zsh-completions $fpath)

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="simple"
# Use case sensitive completion.
CASE_SENSITIVE="true"
# Disable auto-correct.
DISABLE_CORRECTION="true"
# Don't set "dity" for untracked file.
DISABLE_UNTRACKED_FILES_DIRTY="true"
# Autostart tmux.
ZSH_TMUX_AUTOSTART="true"
# Autostart docker-machine default.
DOCKER_MACHINE_AUTOSTART="true"
DOCKER_MACHINE_NAME="default"

plugins=(
    aws
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

    # Customize the prompt a little (shorten pwd)
    ZSH_THEME_GIT_PROMPT_PREFIX="("
    ZSH_THEME_GIT_PROMPT_SUFFIX=")"
    ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg_bold[red]%}✗%{$reset_color%}"
    ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg_bold[green]%}✔%{$reset_color%}"

    PROMPT="
(%{$fg_bold[blue]%}%n%{$reset_color%}@%{$fg_bold[green]%}%m%{$reset_color%}):<%{$fg_bold[cyan]%}%~%{$reset_color%}>
[%{$fg_bold[red]%}%D{%a %b %d %r}%{$reset_color%}]$(git_prompt_info)%{$reset_color%}%% "

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

# Load oh-my-zsh.
tput sc
echo "Loading oh-my-sh..."
source $ZSH/oh-my-zsh.sh
echo "Loading user config..."
main
tput rc; tput el
