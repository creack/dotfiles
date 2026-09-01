# Interactive zsh config.
# Profile startup with: ZPROF=1 zsh -ic exit
[[ -n "$ZPROF" ]] && zmodload zsh/zprof

# ---------------------------------------------------------------------------
# tmux auto-attach
# ---------------------------------------------------------------------------
# Attach to the most recent existing tmux session, or create "main" if none
# exist. Bail out when:
#   - already inside tmux                ($TMUX set)
#   - running inside emacs               (vterm / eshell / tramp)
#   - running inside VS Code / Cursor    ($TERM_PROGRAM=vscode for both)
#   - the escape-hatch file is present   (~/.notmux)
#   - tmux isn't installed
if [[ -z "$TMUX" && -z "$INSIDE_EMACS" && "$TERM_PROGRAM" != "vscode" && ! -f "$HOME/.notmux" ]] && (( $+commands[tmux] )); then
  if tmux ls &>/dev/null; then
    exec tmux attach
  else
    exec tmux new-session -s main
  fi
fi

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

# Alt-backspace: delete one path component at a time (~/src/foo/bar -> ~/src/foo/)
# instead of the whole path. Default WORDCHARS treats / as part of a word.
backward-kill-path-component() {
  local WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
  zle backward-kill-word
}
zle -N backward-kill-path-component
bindkey '^[^?' backward-kill-path-component    # alt-backspace (ESC DEL)
bindkey '^[^H' backward-kill-path-component    # alt-backspace (ESC ^H)

# Up/Down: search history for entries matching what's already typed before the
# cursor (prefix search), rather than walking history in plain chronological
# order. Falls back to line movement within a multiline buffer.
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search    # up      (normal cursor keys)
bindkey '^[OA' up-line-or-beginning-search    # up      (application mode)
bindkey '^[[B' down-line-or-beginning-search  # down    (normal cursor keys)
bindkey '^[OB' down-line-or-beginning-search  # down    (application mode)

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

# Keep ssh-agent reliable without manual babysitting, when this machine holds
# the private key: run a single long-lived agent on a fixed socket. Every shell
# and tmux pane points at that path, so it survives tmux dropping SSH_AUTH_SOCK
# from new panes and the agent's own random socket name. A socket left stale by
# a reboot is detected and replaced. The key is loaded on demand; on macOS the
# passphrase comes from the keychain after the first unlock.
if [[ -O ~/.ssh/id_ed25519 ]]; then
  export SSH_AUTH_SOCK="$HOME/.ssh/agent/sock"
  mkdir -p "${SSH_AUTH_SOCK:h}"
  ssh-add -l &>/dev/null
  if [[ $? -eq 2 ]]; then              # nothing answering on the socket
    rm -f "$SSH_AUTH_SOCK"
    ssh-agent -a "$SSH_AUTH_SOCK" >/dev/null
  fi
  if ! ssh-add -l &>/dev/null; then    # agent up, but holding no keys yet
    if [[ "$OSTYPE" == darwin* ]]; then
      ssh-add --apple-use-keychain ~/.ssh/id_ed25519 2>/dev/null
    else
      ssh-add ~/.ssh/id_ed25519 2>/dev/null
    fi
  fi
fi

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
export GOPRIVATE="github.com/future-research/*"
(( $+commands[vivid] )) && export LS_COLORS="$(vivid generate gruvbox-dark-soft)"

# Per-host overrides (untracked).
[[ -r "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

[[ -n "$ZPROF" ]] && zprof
export PATH="$HOME/.local/bin:$PATH"

# bun completions
[ -s "/Users/guillaume/.bun/_bun" ] && source "/Users/guillaume/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
