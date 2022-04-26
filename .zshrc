[ -n "${ZPROF}" ] && zmodload zsh/zprof

export TMUX_TZ=$(date +%Z)

export COLORTERM=truecolor

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
alias fggrep="fgrep -R --exclude-dir=vendor --exclude-dir=.cache --color --include='*.go'"

# Oh-my-zsh config.

# Disable completion security check as it is too slow. Don't manually add any completions before checking them.
ZSH_DISABLE_COMPFIX=true

ZSH_THEME="simple"
plugins=(
  git
  tmux
  docker
  docker-compose
  zsh-autosuggestions
  zsh-completions
  zsh-syntax-highlighting
  nvm
)

NVM_LAZY=true
NVM_CUSTOM_LAZY=true

# Set tmux autostart unless we are using vscode or emacs tramp.
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

# Enable shared history so we can reference history between terms.
setopt share_history
# Save each command in history to make sure we don't loose it.
setopt inc_append_history

# Tell git to use the current tty for gpg passphrase prompt (needs to be at the end so the tty is within tmux, not out).
export GPG_TTY=$(tty)

function rl() {
  local ssh_auth_sock=$(ls -t $(find /tmp/ssh-* -group $USER -name 'agent.*' 2> /dev/null) | head -1)
  if [ -S "${ssh_auth_sock}" ]; then
    echo "Refreshed ssh agent socket." >&2
    export SSH_AUTH_SOCK=${ssh_auth_sock}
    # If within tmux, update the session env as well.
    [ -n "$TMUX" ] && tmux set-environment SSH_AUTH_SOCK ${SSH_AUTH_SOCK}
  fi
}

# If the ssh agent socket is not set or expired, reload it.
if [ -z "$SSH_AUTH_SOCK" ] || [ ! -S "$SSH_AUTH_SOCK" ]; then
  rl
fi

# Update the ~/.gitconfig.local link to target a new profile.
function setgit() {
  local new_profile=$1
  local profile_path="${HOME}/.gitconfig.${new_profile}"
  local fail=0

  # Make sure the target exists.
  if [ ! -f "${profile_path}" ]; then
    echo "Git profile '${new_profile}' not found." >&2
    fail=1
  fi

  # Make sure the existing profile is a link and not a hard-set file.
  if [ ! -L "${HOME}/.gitconfig.local" ]; then
    echo "Error: The ~/.gitconfig.local  file is not a link." >&2
    fail=1
  fi

  if [ "${fail}" = 1 ]; then
    return 1;
  fi

  ln -f -s ${profile_path} ${HOME}/.gitconfig.local
}

# Small helper used in the prompt to show the current git profile.
function getgit() {
  if [ -f "${HOME}/.gitconfig.local" ]; then
    ls -l ${HOME}/.gitconfig.local | sed 's/.*\.gitconfig\.//'
  fi
}

# Small helper to cleanup aws env.
function unsetaws() {
  unset AWS_ACCESS_KEY_ID
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_SESSION_TOKEN
  unset AWS_REGION
  unset AWS_DEFAULT_REGION
  unset AWS_PROFILE
  unset AWS_DEFAULT_PROFILE
}

# Show the git profile in the prompt.
export PROMPT='%{$fg_bold[blue]%}($(getgit))%{$reset_color%}'${PROMPT}

# Putty bindings for meta left/right
bindkey '\e\eOD' backward-word
bindkey '\e\eOC' forward-word

# Xterm bindings for meta left/right.
bindkey "^[[1;3D" backward-word
bindkey "^[[1;3C" forward-word

# Set M-l as lowercase word.
bindkey "^[l" down-case-word

# Load more autocompletions.
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C ${HOME}/go/bin/terraform terraform
complete -o nospace -C ${HOME}/go/bin/vault vault

# Load the private config if set.
[ -f ~/.zshrc_priv_config ] && source ~/.zshrc_priv_config

# The NVM_LAZY implementation doesn't support loading the local .nvmrc
# and the NVM_AUTOLOAD is way too slow. Implement a custom loader.
if (( $+NVM_CUSTOM_LAZY )); then
  function custom-load-nvmrc() {
    # If we don't have a .nvmrc file, stop here.
    if [ ! -f .nvmrc ]; then
      return;
    fi

    # Undo the nvm lazy loading so we'll use the actual commands.
    unfunction node npm yarn 2> /dev/null

    # If we already have nvm loaded, print the versions and stop here.
    if [ -n "${NVM_BIN}" ]; then
      local loaded_nvm_version=$(echo $NVM_BIN | sed 's|.*versions/node/\(.*\)/bin|\1|')
      local loaded_nvm_major_version=$(echo $loaded_nvm_version | sed 's/v\?\([^.]*\).*/\1/')
      local expected_nvm_major_version=$(cat .nvmrc | sed 's/v\?\([^.]*\).*/\1/')
      if [ "${expected_nvm_major_version}" = "${loaded_nvm_major_version}" ]; then
        echo "Loaded node version: ${loaded_nvm_version}, local nvmrc: $(cat .nvmrc)" >&2
        return
      fi
    fi

    # Load nvm with the local .nvmrc.
    nvm use
  }
  autoload -U add-zsh-hook
  add-zsh-hook chpwd custom-load-nvmrc
  custom-load-nvmrc
fi
unset NVM_CUSTOM_LAZY

# Helper to load extra plugins at runtime.
function load-plugin() {
  for p in "$@"; do
    source $ZSH/plugins/"$p"/"$p".plugin.zsh
  done
}

# Lazy load slow plugins.
function helm kubectl aws {
  unfunction $0
  echo -n "Lazy loading '$0' plugin... " >&2
  load-plugin $0
  echo "Done." >&2
  $0 $@
}

[ -n "${ZPROF}" ] && zprof

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
