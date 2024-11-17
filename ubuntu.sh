#!/usr/bin/env sh

set -e

user=$(whoami)
if [ ! "${user}" = "root" ]; then
  echo "Granting sudoer nopassword to ${user}" >&2
  echo "${user} ALL=(ALL) NOPASSWD:ALL" | sudo tee "/etc/sudoers.d/90-${user}-root"
fi


if ! hash sudo 2> /dev/null; then
  apt-get install -y sudo
fi

if [ -f /etc/apt/sources.list.d/pve-enterprise.list ]; then
  echo "deb http://download.proxmox.com/debian/pve bullseye pve-no-subscription" | sudo tee "/etc/apt/sources.list.d/pve-enterprise.list"
fi

curl -fsSL https://tailscale.com/install.sh | sudo sh

sudo apt-get install -y tmux most zsh watch htop build-essential zip unzip python3-pip git rsync git-lfs jq

if ! hash docker 2> /dev/null; then
  curl https://get.docker.com | sh
  sudo usermod -aG docker "${user}"
fi

if hash snap 2> /dev/null; then
  sudo snap install emacs --classic
  sudo snap install kubectl --classic
  sudo snap install kubectx --classic
else
  sudo apt-get install -y emacs-nox
fi

sudo chsh -s /usr/bin/zsh "${user}"
pip3 install --break-system-packages psutil powerline-status
pip3 install --break-system-packages yq
make

. "${HOME}/.nvm/nvm.sh"
nvm install --lts

vivid_version=0.8.0
wget "https://github.com/sharkdp/vivid/releases/download/v0.8.0/vivid_${vivid_version}_arm64.deb"
sudo dpkg -i "vivid_${vivid_version}_arm64.deb"
rm "vivid_${vivid_version}_arm64.deb"
export PATH="${PATH}:~/goroot/bin"
export GOBIN=~/go/bin
export GOROOT=~/goroot
go install github.com/owenthereal/ccat@latest
go install golang.org/x/tools/gopls@latest
go install sigs.k8s.io/kind@latest

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
. "$HOME/.cargo/env"
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

curl -fsSL https://get.pnpm.io/install.sh | sh -
