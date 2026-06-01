# Loaded for every zsh invocation (interactive, non-interactive, scripts).
# Keep this minimal — heavier setup belongs in .zprofile or .zshrc.

# XDG base dirs.
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Editor / pager.
export EDITOR="emacsclient -a '' -c -t"
export VISUAL="$EDITOR"
export PAGER="${commands[most]:-less}"

# Truecolor everywhere we can.
export COLORTERM=truecolor

# Local bin first.
typeset -U path
path=(
  $HOME/.local/bin
  $HOME/go/bin
  $path
)
export PATH
