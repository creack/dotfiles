# Interactive zsh config.
# Profile startup with: ZPROF=1 zsh -ic exit
[[ -n "$ZPROF" ]] && zmodload zsh/zprof

# ---------------------------------------------------------------------------
# History
# ---------------------------------------------------------------------------
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
mkdir -p "${HISTFILE:h}"
HISTSIZE=100000
SAVEHIST=100000
setopt extended_history       # timestamps in history file
setopt hist_expire_dups_first # drop dups first when trimming
setopt hist_ignore_dups       # don't record consecutive dups
setopt hist_ignore_space      # leading-space commands aren't recorded
setopt hist_verify            # let !! expand for review before running
setopt inc_append_history     # append immediately (don't lose on crash)
setopt share_history          # share between concurrent sessions

# ---------------------------------------------------------------------------
# Options
# ---------------------------------------------------------------------------
setopt auto_cd                # `dir` == `cd dir`
setopt auto_pushd             # cd pushes onto dir stack
setopt pushd_ignore_dups
setopt extended_glob
setopt interactive_comments   # allow `# comments` in interactive shells
setopt no_beep

# ---------------------------------------------------------------------------
# Completion
# ---------------------------------------------------------------------------
# brew completions
if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:$FPATH"
fi

autoload -Uz compinit
# Speed up compinit: only run the security check once per day.
_zcompdump="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump"
mkdir -p "${_zcompdump:h}"
if [[ -n "$_zcompdump"(#qNmh-24) ]]; then
  compinit -C -d "$_zcompdump"
else
  compinit -d "$_zcompdump"
fi
unset _zcompdump

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:descriptions' format '[%d]'

# ---------------------------------------------------------------------------
# Key bindings
# ---------------------------------------------------------------------------
bindkey -e                              # emacs keymap
bindkey '^[[1;3D' backward-word         # alt-left
bindkey '^[[1;3C' forward-word          # alt-right
bindkey '^[l'     down-case-word        # M-l
bindkey '^[[3~'   delete-char           # delete

# ---------------------------------------------------------------------------
# Aliases
# ---------------------------------------------------------------------------
# ls: GNU coreutils on mac, plain ls on linux.
if [[ "$OSTYPE" == darwin* ]] && (( $+commands[gls] )); then
  alias ls='gls --color=auto --group-directories-first'
else
  alias ls='ls --color=auto --group-directories-first'
fi
alias ll='ls -lh'
alias la='ls -lAh'
alias l='ls -CF'

alias g='git'
alias gs='git status -sb'
alias gd='git diff'
alias gl='git lg'

# Debian/Ubuntu install fd as fdfind; expose as fd.
(( $+commands[fdfind] )) && ! (( $+commands[fd] )) && alias fd='fdfind'
# Same for batcat -> bat if ever installed.
(( $+commands[batcat] )) && ! (( $+commands[bat] )) && alias bat='batcat'

# emacs as a daemon client.
alias emacs="emacsclient -a '' -c -t"

# docker compose shortcuts.
alias dcu='docker compose up -d --build -t 1'
alias dcup='docker compose up -t 1'
alias dcd='docker compose down -t 1'
alias dcda='docker compose down -t 1 -v --remove-orphans'
alias dcl='docker compose logs'
alias dclf='docker compose logs -f'
alias dce='docker compose exec'
alias dcr='docker compose run --rm'

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

# Refresh SSH_AUTH_SOCK from the newest agent socket on disk (e.g. after
# reconnecting to a tmux session whose forwarded socket has expired).
rl() {
  local sock
  sock=$(ls -t $(find /tmp/ssh-* -group "$USER" -name 'agent.*' 2>/dev/null) 2>/dev/null | head -1)
  if [[ -S "$sock" ]]; then
    export SSH_AUTH_SOCK="$sock"
    [[ -n "$TMUX" ]] && tmux set-environment SSH_AUTH_SOCK "$sock"
    print -u2 "Refreshed SSH_AUTH_SOCK -> $sock"
  fi
}
[[ -z "$SSH_AUTH_SOCK" || ! -S "$SSH_AUTH_SOCK" ]] && rl

# Clear all AWS env vars (handy when bouncing between profiles).
unsetaws() {
  unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN \
        AWS_REGION AWS_DEFAULT_REGION AWS_PROFILE AWS_DEFAULT_PROFILE
}

# Pretty `docker compose ps` for the current project.
dcps() {
  local format project_name column_names
  project_name=$(basename "$PWD")
  format='{{.ID}}│{{printf "%.40s" .Image}}│{{.Command}}│{{.Label "com.docker.compose.service"}}│{{.Status}}│{{.Ports}}│{{.Networks}}│{{.Size}}'
  column_names=$(echo "$format" \
    | sed 's/Label [^}]*/Service/' \
    | sed 's/printf "[^"]*" //g' \
    | sed 's/{{\.\([^}]*\)}}│*/\1,/g' \
    | tr '[:lower:]' '[:upper:]')
  docker ps --all \
    --filter "label=com.docker.compose.project=$project_name" \
    --format "$format" "$@" \
    | column -t -s '│' -o '   ' -N "$column_names"
}

# ---------------------------------------------------------------------------
# Plugins (sourced last so they hook the final widget chain).
# ---------------------------------------------------------------------------

# Source the first readable file from a list of candidates.
_source_first() {
  local f
  for f in "$@"; do
    [[ -r "$f" ]] && { source "$f"; return 0; }
  done
  return 1
}

# autosuggestions — ghost text from history.
# Locations: brew (macOS/linuxbrew), apt (Debian/Ubuntu), dnf (Fedora).
_source_first \
  "${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh}" \
  "/usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh" \
  "/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"

# fzf — Ctrl-R / Ctrl-T / Alt-C + completion. `fzf --zsh` needs fzf >= 0.48.
if (( $+commands[fzf] )); then
  if fzf --help 2>&1 | grep -q -- '--zsh'; then
    source <(fzf --zsh)
  else
    # Older fzf: source key-bindings/completion files directly.
    _source_first \
      "${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/opt/fzf/shell/key-bindings.zsh}" \
      "/usr/share/doc/fzf/examples/key-bindings.zsh" \
      "/usr/share/fzf/key-bindings.zsh"
    _source_first \
      "${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/opt/fzf/shell/completion.zsh}" \
      "/usr/share/doc/fzf/examples/completion.zsh" \
      "/usr/share/fzf/completion.zsh"
  fi
fi

# syntax-highlighting — must be sourced last so it wraps everything else.
_source_first \
  "${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh}" \
  "/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" \
  "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

unfunction _source_first

# ---------------------------------------------------------------------------
# Prompt
# ---------------------------------------------------------------------------
if (( $+commands[starship] )); then
  eval "$(starship init zsh)"
fi

# ---------------------------------------------------------------------------
# Misc env that depends on tools being on PATH
# ---------------------------------------------------------------------------
export GPG_TTY=$(tty)
export DOCKER_BUILDKIT=1
(( $+commands[vivid] )) && export LS_COLORS="$(vivid generate gruvbox-dark-soft)"

# Per-host overrides (untracked).
[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

[[ -n "$ZPROF" ]] && zprof
