# -*- mode: sh -*-

# echo '['$(date)'] Welcome!' >&2
# # Auto load TMUX.
#
# #export TERM=xterm-256color
#
# # If we don't have the TMUX env set, we are probably outside tmux.
# if [ -z "$TMUX_PANE" ]; then
#     tmux list-sessions && exec tmux a || exec tmux
# fi
#
export PATH=$HOME/go/bin:$PATH
function legacy() {

export PATH=$PATH:~/.local/bin

#fpath=(/usr/local/share/zsh-completions $fpath)

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME=${ZSH_THEME-"simple"}
# Use case sensitive completion.
CASE_SENSITIVE="true"
# Disable auto-correct.
DISABLE_CORRECTION="true"
# Autostart tmux.
[ "$TERM" = "xterm" ] && export TERM=xterm-truecolor
[ -z "$ZSH_TMUX_AUTOSTART" ] && ZSH_TMUX_AUTOSTART="true"
# Autostart docker-machine default.
DOCKER_MACHINE_AUTOSTART="false"
DOCKER_MACHINE_NAME="default"
TIMED="false"

plugins=(
    git
    tmux
    ssh-agent
    fly
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
	local target=${1:-./}
	local maxdepth=${2:-3}

	patterns='*~ .*~ \#*\# .\#* a.out flymake_*.go .flymake_*.go *.orig *.test'
	echo "\( -name $(echo $patterns | sed 's/ / -or -name /g') \) -print -delete" | xargs find $target -maxdepth $maxdepth -type f
    }

    hash gls 2> /dev/null && alias ls=gls
    hash gdate 2> /dev/null && alias date=gdate

    alias szsh="sudo sh -c 'unset HOME; zsh'"
    alias sudozsh=szsh
    alias ls="ls --color=auto"
    alias emacs="emacsclient -a ''  -c -t"
    alias grep="grep --color=auto -n"
    alias rm="rm -v"
    alias a64="encode64"
    alias d64="decode64"
    alias bc="bc -l $HOME/.bcrc"
    alias vfgrep="fgrep --exclude-dir .git --exclude-dir .terraform --exclude-dir vendor -R"
    alias vgrep="grep --exclude-dir .git --exclude-dir vendor -R"
}

# Helper to fetch current time with ms.
function mstime() {
    # Get the time in "ns" and trim down the last 6 digits.
    echo "$(date +%s.%N | sed 's/......$//')" | \bc
}

function timed() {
    name=$1
    shift
    cmd=$1
    shift

    if [ ! "$TIMED" = "true" ]; then
	$cmd $@
	return
    fi

    echo "[$name] Loading..."
    a=$(mstime)
    $cmd $@
    b=$(mstime)
    echo "[$name] Loaded in $(echo "($b - $a)" | \bc) ms"
}

# Save cursor position.
#tput sc
echo

# Load private config if exists.
[ -f "$HOME/.zsh_priv_config" ] && timed "local user config" source $HOME/.zsh_priv_config

# Load oh-my-zsh.
timed "oh-my-zsh" source $ZSH/oh-my-zsh.sh

# Load powerline.
#timed "powerline" source /usr/local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# Load main user config.
timed "user config" main

# Load docker-machine "plugin".
[ -f "$HOME/.zsh_docker" ] && timed "docker config" source $HOME/.zsh_docker

# Load golang config.
[ -f "$HOME/.zsh_golang" ] && timed "golang config" source $HOME/.zsh_golang

# Restore cursor & clear line.
#tput rc; tput el

function igo() {
    tmp=$(mktemp); mv $tmp $tmp.go; echo "package main\nfunc main() {" > $tmp.go; cat >> $tmp.go; echo "}" >> $tmp.go; goimports -w $tmp.go; go run $tmp.go; \rm $tmp.go
}

function encode64() {
    if [[ $# -eq 0 ]]; then
        cat | base64
    else
        printf '%s' $1 | base64
    fi
}

function decode64() {
    if [[ $# -eq 0 ]]; then
        cat | base64 --decode
    else
        printf '%s' $1 | base64 --decode
    fi
}

function restart_agent() {
    echo2 "Restart gpg agent."
    # Restart the gpg agent.
    # shellcheck disable=SC2046
    kill -9 $(pidof scdaemon) >/dev/null 2>&1 || true
    # shellcheck disable=SC2046
    kill -9 $(pidof gpg-agent) >/dev/null 2>&1 || true
    gpg-connect-agent /bye >/dev/null 2>&1 || true
    gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1 || true

    gpg-agent --homedir $GNUPGHOME --daemon --enable-ssh-support >& /dev/null || true
}

function rgpg() {
    unset GPGPROFILE
    local ret=$(gpg --card-status 2> /dev/null | \grep Serial | sed 's/.*: //')
    if [ -n "$ret" ]; then
	export GPGPROFILE=$ret
	export GNUPGHOME=$HOME/.gnupg.$ret
    else
	export GNUPGHOME=$HOME/.gnupg
    fi
    [ ! -f "$HOME/.gpgprofile" ] && touch $HOME/.gpgprofile

    export SSH_AUTH_SOCK=$GNUPGHOME/S.gpg-agent.ssh
    if [ ! -S "$SSH_AUTH_SOCK" ] || [ ! "$(cat $HOME/.gpgprofile)" = "$GPGPROFILE" ]; then
	echo $GPGPROFILE > $HOME/.gpgprofile
	restart_agent
    fi
    export GPGKEY=$(gpg --list-secret-keys --keyid-format short | \fgrep '[S]' | head -1 | sed 's#.*/##' | sed 's# .*##')
}
#rgpg

#export TERM=xterm-256color

autoload -U +X bashcompinit && bashcompinit

complete -o nospace -C $HOME/go/bin/gocomplete go
complete -C $HOME/go/bin/swapexcomplete swapex

function jwt() { echo $(ret=$(cat | sed 's/\(.*\)\..*/\1/');  echo '{"header":'; echo -n $ret | sed 's/\(.*\)\..*/\1/' | d64; echo ', "payload":';echo $ret | sed 's/.*\.//' | d64 ; echo '"}}') | jq . }

function prettybyte() {
    n=$1;
    [ -z "$n" ] && n=$(cat);
    [ -z "$BASE" ] && base=1024 || base=$BASE
    [ "$n" -ge "$(echo $base ^ 5 | bc)" ] && echo $(echo "$n / ($base ^ 5)" | bc)TiB && return;
    [ "$n" -ge "$(echo $base ^ 4 | bc)" ] && echo $(echo "$n / ($base ^ 4)" | bc)TiB && return;
    [ "$n" -ge "$(echo $base ^ 3 | bc)" ] && echo $(echo "$n / ($base ^ 3)" | bc)GiB && return;
    [ "$n" -ge "$(echo $base ^ 2 | bc)" ] && echo $(echo "$n / ($base ^ 2)" | bc)MiB && return;
    [ "$n" -ge "$(echo $base ^ 1 | bc)" ] && echo $(echo "$n / ($base ^ 1)" | bc)KiB && return;
    [ "$n" -lt "$(echo $base ^ 1 | bc)" ] && echo $(echo "$n / ($base ^ 0)" | bc)iB' '  && return;
}

# Parse the output of `du -sc *`, sort it and display it with prettybyte.
# Similar to `du -shc *` but sorted.
function prettydu() {
    sort -n |
	while read l; do
	    size=$(echo $l | sed 's/[ \t].*//');
	    pth=$(echo $l | sed 's/.*[ \t]//');
	    printf "% 12s % 8s %s\n" "$(echo "$size" | prettybyte)" "" "$pth";
	done
}

# Putty bindking for meta left/right
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word
}

legacy

custom_prompt

function lock() {
    ssh gcharmes@mlla3221.magicleap.ds "open -a ScreenSaverEngine; pmset displaysleepnow"&
    export DISPLAY=:0
    xtrlock
}

complete -o nospace -C /home/guillaume/go/bin/terraform terraform

complete -o nospace -C /home/guillaume/go/bin/gocomplete go

autoload -U compinit && compinit

function true-color() {
awk 'BEGIN{
    s="/\\/\\/\\/\\/\\"; s=s s s s s s s s;
    for (colnum = 0; colnum<77; colnum++) {
        r = 255-(colnum*255/76);
        g = (colnum*510/76);
        b = (colnum*255/76);
        if (g>255) g = 510-g;
        printf "\033[48;2;%d;%d;%dm", r,g,b;
        printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
        printf "%s\033[0m", substr(s,colnum+1,1);
    }
    printf "\n";
}'
}
