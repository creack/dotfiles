# Loaded for login shells (once per session).
# macOS Terminal/iTerm/tmux all start zsh as a login shell, so brew shellenv
# lives here rather than .zshrc.

if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi
