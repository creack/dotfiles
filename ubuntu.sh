#!/usr/bin/env sh

set -e

user=$(whoami)
if [ ! "${user}" = "root" ]; then
  echo "Granting sudoer nopassword to ${user}" >&2
  echo "${user} ALL=(ALL) NOPASSWD:ALL" | sudo tee "/etc/sudoers.d/90-${user}-root"
fi

curl -fsSL https://tailscale.com/install.sh | sudo sh

sudo apt-get install -y tmux most zsh watch htop build-essential mosh unzip python3-pip

if [ ! "${user}" = "root" ]; then
  pip3 install powerline-status
fi

make

. "${HOME}/.nvm/nvm.sh"
nvm install 14
nvm install 16
