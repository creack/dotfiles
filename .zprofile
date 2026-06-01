# Loaded for login shells (once per session).
# macOS Terminal/iTerm/tmux all start zsh as a login shell, so brew shellenv
# lives here rather than .zshrc.

for _brew in /opt/homebrew/bin/brew /usr/local/bin/brew /home/linuxbrew/.linuxbrew/bin/brew; do
  if [[ -x "$_brew" ]]; then
    eval "$("$_brew" shellenv)"
    break
  fi
done
unset _brew
