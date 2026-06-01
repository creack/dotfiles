#!/usr/bin/env bash
# Install the Linux equivalent of the macOS Brewfile.
# Supports Debian/Ubuntu (apt). Other distros: install the listed packages
# yourself, then re-run `make links`.
set -euo pipefail

APT_PACKAGES=(
  zsh
  tmux
  emacs-nox
  git
  curl
  ca-certificates
  build-essential
  ripgrep
  fd-find
  fzf
  zsh-autosuggestions
  zsh-syntax-highlighting
  gnupg
  most
  tree
  python3
  python3-pip
  golang-go
)

require_sudo() {
  if [[ $EUID -ne 0 ]] && ! command -v sudo >/dev/null; then
    echo "Need root or sudo to install packages." >&2
    exit 1
  fi
}

sudo_() {
  if [[ $EUID -eq 0 ]]; then "$@"; else sudo "$@"; fi
}

install_apt() {
  require_sudo
  sudo_ apt-get update
  sudo_ apt-get install -y "${APT_PACKAGES[@]}"
}

install_starship() {
  if command -v starship >/dev/null; then
    echo "starship already installed: $(starship --version)"
    return
  fi
  echo "Installing starship..."
  curl -fsSL https://starship.rs/install.sh | sh -s -- --yes --bin-dir "$HOME/.local/bin"
}

install_gh() {
  if command -v gh >/dev/null; then return; fi
  echo "gh (GitHub CLI) is not in apt by default. Install instructions:"
  echo "  https://github.com/cli/cli/blob/trunk/docs/install_linux.md"
}

if command -v apt-get >/dev/null; then
  install_apt
else
  echo "Unsupported distro (no apt-get). Install equivalents of:" >&2
  printf '  - %s\n' "${APT_PACKAGES[@]}" >&2
  exit 1
fi

install_starship
install_gh

echo
echo "Done. Re-run 'make links' if needed, then 'chsh -s \$(command -v zsh)'."
