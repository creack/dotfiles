# User config.

# Use 24bit term.
export TERM=xterm-truecolor

# Set the path for pip/yarn/golang.
export PATH=~/.local/bin:~/go/bin:~/goroot/bin:/usr/local/bin:/usr/local/sbin:/snap/bin/:~/.yarn/bin:$PATH

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
alias udockerrun='docker run --rm --user $(id -u):$(id -g) -e HOME -v $HOME:$HOME -w $(pwd) -e GOPATH=$HOME/go:/go'

# Docker wrappers for common tools.
alias swagger='udockerrun quay.io/goswagger/swagger'
alias protoc='udockerrun creack/grpc:go1.13-protobuf3.9.0-grpc1.24.0-protocgengo1.3.2'
alias prototool='udockerrun --entrypoint prototool creack/grpc:go1.13-protobuf3.9.0-grpc1.24.0-protocgengo1.3.2'

# Protobuf Go generation.
alias gprotoc='protoc --go_out=plugins=grpc:.'

# Protobuf Go Validations generation.
alias gvprotoc='gprotoc --validate_out=lang=go:.'

# GRPC Gateway generation.
alias gwprotoc='protoc --grpc-gateway_out="logtostderr=true:."'

# Swagger generation.
alias sprotoc='protoc --swagger_out="logtostderr=true:."'

# Recursive grep go file.
alias fggrep="fgrep -R --exclude-dir=vendor --color --include='*.go'"

# Putty bindking for meta left/right
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

# Set M-l as lowercase word.
bindkey "^[l" down-case-word

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
if [ -n "$VSCODE_IPC_HOOK_CLI" ]; then
  ZSH_TMUX_AUTOSTART=false
else
  ZSH_TMUX_AUTOSTART=true
fi

# Allow agent-forwarding.
zstyle :omz:plugins:ssh-agent agent-forwarding on

# Load oh-my-zsh.
export ZSH=~/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Disable shared history so each term has it's own backlog.
unsetopt share_history

# Tell git to use the current tty for gpg passphrase prompt (needs to be at the end so the tty is within tmux, not out).
export GPG_TTY=$(tty)

# Init nvm in a function so it doesn't run each time (very slow).
function loadnvm() {
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

[ -f "$HOME/.zshrc_priv_config" ] && source "$HOME/.zshrc_priv_config"

VSCODE_IPC_HOOK_CLI=1
if [ -n "$VSCODE_IPC_HOOK_CLI" ]; then
  loadnvm
fi
