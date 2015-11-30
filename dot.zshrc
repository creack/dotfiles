# -*- mode: sh -*-

fpath=(~/.zsh_comp $fpath)

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="simple"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git brew go gnu-utils mosh osx tmux vagrant ssh-agent emacs docker encode64)

ZSH_TMUX_AUTOSTART=true

source $ZSH/oh-my-zsh.sh

# User configuration

export MANPATH="/usr/local/man:$MANPATH"
export LSCOLORS="ExFxCxDxBxegedabagacad"
export LANG=en_US.UTF-8
export GPGKEY=CB6E3FF3
export PAGER="most"
export HOSTTYPE=`uname -s`
export HOST=`uname -n`
export EDITOR="emacs"
export SHELL="/usr/local/bin/zsh"

export GOROOT="$HOME/goroot"
export GOBIN="$GOROOT/bin"
export GOPATH="$HOME/go"

export PATH="$GOBIN:/usr/local/bin:/usr/local/sbin:$PATH"

function loaddocker() {
    if [ ! -f /tmp/.dockercache ]; then
	docker-machine start dev
	docker-machine env dev > /tmp/.dockercache
	docker-machine ip dev > /tmp/.dockerip
    fi
    export DOCKER_IP=$(cat /tmp/.dockerip)
    eval $(cat /tmp/.dockercache)
}
loaddocker

function dockerclear {
    rm -f /tmp/.dockercache /tmp/.dockerip
    loaddocker
}

#export JAVA_HOME="$(/usr/libexec/java_home)"
#export AWS_ACCESS_KEY="<Your AWS Access ID>"
#export AWS_SECRET_KEY="<Your AWS Secret Key>"
#export EC2_HOME="/usr/local/Cellar/ec2-api-tools/1.6.13.0/libexec"

#emacsclient -s "/tmp/emacs503/server" -a ''  -ct
alias emacs="emacsclient -a ''  -ct"
alias grep="grep --color=auto -n"
alias rm="rm -v"

alias a64="encode64"
alias d64="decode64"

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


source ~/.zsh_git_prompt
PROMPT="
(%{$fg_bold[blue]%}%n%{$reset_color%}@%{$fg_bold[green]%}%m%{$reset_color%}):<%{$fg_bold[cyan]%}%~%{$reset_color%}>
[%{$fg_bold[red]%}%D{%r}%{$reset_color%}]%% "

RPROMPT="[%{$fg_bold[red]%}%D{%a %d %b}%{$reset_color%}]"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export WORKON_HOME=~/Envs
#source /usr/local/bin/virtualenvwrapper.sh

unsetopt share_history

deeppep8() { [ -z "$1" ] && 1="."; files=$(find $1 -name '*.py'); echo -n "Total issues: "; echo $files | xargs pep8 --count -qq; echo; echo $files | xargs pep8 --statistic -qq --select=E | sort -n; echo; echo $files | xargs pep8 --statistic -qq --select=W | sort -n }
pep8s(){[-z "$1" ] && 1="."; files=$(find $1 -name '*.py'); echo -n "Total issues: "; echo $files | xargs pep8 --count -qq; echo; echo $files | xargs pep8 --statistic -qq --select=E | sort -n; echo; echo $files | xargs pep8 --statistic -qq --select=W | sort -n }

export PATH=$PATH:/opt/vertica/bin


function update_go() {
    packages="
code.google.com/p/rog-go/exp/cmd/godef

github.com/ajstarks/svgo/benchviz
github.com/axw/gocov/gocov
github.com/cespare/prettybench
github.com/dougm/goflymake
github.com/golang/lint/golint
github.com/josharian/impl
github.com/kisielk/errcheck
github.com/kisielk/godepgraph
github.com/nsf/gocode
github.com/tools/godep

golang.org/x/tools/cmd/benchcmp
golang.org/x/tools/cmd/cover
golang.org/x/tools/cmd/godoc
golang.org/x/tools/cmd/goimports
golang.org/x/tools/cmd/gorename
golang.org/x/tools/cmd/oracle
golang.org/x/tools/cmd/stringer
golang.org/x/tools/cmd/vet

sourcegraph.com/sqs/goreturns
"
    for pkg in `echo "$packages" | tr "\n" ' '`; do
	echo $pkg
	go get -u $pkg
    done
}

# added by travis gem
[ -f /Users/guillaume/.travis/travis.sh ] && source /Users/guillaume/.travis/travis.sh

alias vv='docker exec -i vertica_c vsql -U dbadmin'
alias vvt='docker exec -it vertica_c vsql -U dbadmin'
bindkey "^[l" down-case-word

fpath=(/usr/local/share/zsh-completions $fpath)
