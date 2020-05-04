#zmodload zsh/zprof

# User config.

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

# Oh-my-zsh config.

# Disable completion security check as it is too slow. Don't manually add any completions before checking them.
ZSH_DISABLE_COMPFIX=true

ZSH_THEME="simple"
plugins=(
    git
    tmux
    docker
    docker-compose
)


# Set tmux autostart.
if [ -n "$VSCODE_IPC_HOOK_CLI" ] || [ "$TERM" = "dumb" ] || [ -z "$TERM" ]; then
  ZSH_TMUX_AUTOSTART=false
else
  ZSH_TMUX_AUTOSTART=true
fi

# TODO: Add ssh-agent plugin when not in remote server.
# Allow agent-forwarding.
#zstyle :omz:plugins:ssh-agent agent-forwarding on

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
    [ -f .nvmrc ] && nvm use || nvm use --lts
}

[ -f "$HOME/.zshrc_priv_config" ] && source "$HOME/.zshrc_priv_config"

#VSCODE_IPC_HOOK_CLI=1
if [ -n "$VSCODE_IPC_HOOK_CLI" ]; then
  loadnvm
fi

function rl() {
  local ssh_auth_sock=$(ls -t $(find /tmp/ssh-* -group $USER -name 'agent.*' 2> /dev/null) | head -1)
  if [ -S "${ssh_auth_sock}" ]; then
    echo2 "Refreshed ssh agent socket."
    export SSH_AUTH_SOCK=${ssh_auth_sock}
    # If within tmux, update the session env as well.
    [ -n "$TMUX" ] && tmux set-environment SSH_AUTH_SOCK ${SSH_AUTH_SOCK}
  fi
}

# If the ssh agent socket is not set or expired, reload it.
if [ -z "$SSH_AUTH_SOCK" ] || [ ! -S "$SSH_AUTH_SOCK" ]; then
  rl
fi

# Putty bindings for meta left/right
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

# Xterm bindings for meta left/right.
bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

# Set M-l as lowercase word.
bindkey "^[l" down-case-word

#zprof
