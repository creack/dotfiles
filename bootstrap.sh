#!/usr/bin/env sh

# Install the dotfiles.
make install

# Install brew.
hash brew || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install bundle.
hash python || brew install python
brew tap homebrew/bundle

# Execute the Brewfile bundle.
brew bundle --global

# Install powerline.
pip install --upgrade powerline-status psutil

# Install docker VM.
docker-machine env default >& /dev/null || docker-machine create --driver=virtualbox --virtualbox-disk-size=60000 --virtualbox-cpu-count=4 default

# Whitelist zsh.
cat /etc/shells | \grep /usr/local/bin/zsh > /dev/null || (set -x; sudo sh -c 'echo /usr/local/bin/zsh >> /etc/shells'; set +x)
# Set zsh to default.
chsh -s /usr/local/bin/zsh $(whoami)
