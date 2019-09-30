# User config.

# Use 24bit term.
export TERM=xterm-truecolor

# Set the path for pip/golang.
export PATH=~/.local/bin:~/go/bin:~/goroot/bin:/usr/local/bin:/snap/bin/:$PATH

# Use most as pager (for things like man, git diff, etc).
export PAGER=most

# Use emacs as default editor.
export EDITOR=emacs

# Enable go mod.
export GO111MODULE=on

# Start emacs as a daemon.
alias emacs="emacsclient -a ''  -c -t"

# Docker compose shortcuts in addition to the docker-compose oh-my-zsh plugin.
alias dcu='docker-compose up -d --build -t 1'
alias dcd='docker-compose down -v -t 1'
alias dcr='docker-compose restart -t 1'

# Docker run with current user settings mounted in.
alias udockerrun='docker run --rm --user $(id -u):$(id -g) -v $HOME:$HOME -w $(pwd) -e GOPATH=$HOME/go:/go'

# Docker wrappers for common tools.
alias swagger='udockerrun quay.io/goswagger/swagger'

# Putty bindking for meta left/right
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

# Set M-l as lowercase word.
bindkey "^[l" down-case-word

# Disable shared history so each term has it's own backlog.
unsetopt share_history

# Oh-my-zsh config.

ZSH_THEME="simple"
plugins=(
    git
    golang
    ssh-agent
    tmux
    docker
    docker-compose
)

# Set tmux autostart.
ZSH_TMUX_AUTOSTART=true

# Allow agent-forwarding.
zstyle :omz:plugins:ssh-agent agent-forwarding on

# Load oh-my-zsh.
export ZSH="/home/creack/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh

# Tell git to use the current tty for gpg passphrase prompt (needs to be at the end so the tty is within tmux, not out).
export GPG_TTY=$(tty)

# Init nvm in a function so it doesn't run each time (very slow).
function loadnvm() {
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}
