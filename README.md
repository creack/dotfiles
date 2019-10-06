# Bootstrap

## Dependencies

- git
- curl
- make

## Dotfiles

Running `make`

```sh
git clone https://github.com/creack/dotfiles ~/.dotfiles
cd ~/.dotfiles
make -j
```

Note that the Makefile manages:

- go
- oh-my-zsh
- nvm

## Configurations

### Import ssh/gpg keys

```sh
github_user=creack
curl https://github.com/${github_user}.gpg | gpg --import
curl https://github.com/${github_user}.keys >> ~/.ssh/authorized_keys
```

## Linux Specific

### Set default shell

```sh
chsh -s /usr/bin/zsh $(whoami)
```

### Setup sudo

As root:

```sh
apt-get update && apt-get install -y sudo
user=creack
echo "$user ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/90-primary-user
```

### Install Docker

```sh
# Docker itself.
curl -fsSL https://get.docker.com | sh
sudo groupadd docker
sudo usermod -aG docker $(whoami)
```

## OSX Specific

### Whitelist Brew's zsh / set default shell

```sh
sudo sh -c 'echo /usr/local/bin/zsh >> /etc/shells'
chsh -s /usr/local/bin/zsh $(whoami)
```

### Install Homebrew

Note that Apple announced they will remove ruby from OSX by default. Will need to update how to install brew.

```sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

### Install Docker

```sh
brew cask install virtualbox
brew install docker docker-machine docker-compose
docker-machine create --driver=virtualbox --virtualbox-disk-size=200000 --virtualbox-cpu-count=4 default
eval "$(docker-machine env)"
docker ps
```

### Set proper hostname

1. System Preferences -> Sharing -> Computer's Name

### Update keyboard settings

1. System Preferences -> Keyboard -> Key Repeat -> Set to Fast
2. System Preferences -> Keyboard -> Delay Until Repeat -> Set to Short

### Update trackpad settings

1. System Preferences -> Trackpad -> Tap to click -> Enable
2. System Preferences -> Trackpad -> Tracking speed -> Set to Fast

### Set proper screen resolution

1. System Preferences -> Displays -> Resolution: Scaled -> More Space

## Install Packages

Make sure to have all of this installed. Some might be installed by default.

Dependencies:

- git
- curl
- make

Common tools:

- tmux
- emacs (emacs-nox on linux)
- gpg
- ssh
- python / pip
  - powerline-status
  - awscli
- most
- zsh (on OSX, install it from brew to have newer version)
- remake
- ntpdate

## Usage

## Update Go

```sh
# Go itself.
(cd ~/.dotfiles && echo "1.13.1" > versions/go && make)
go version

# Go tools (golint, govet, guru, etc).
go get golang.org/x/tools/cmd/...

# Linter
(cd ~/.dotfiles && echo "1.19.1" > versions/golangci-lint && make)
golangci-lint --version
```

## Update Docker Compose

```sh
(cd ~/.dotfiles && echo "1.24.1" > versions/docker-compose && make)
docker-compose --version
```

## Use node.

Loading nvm is slow (~1 to 2second), so it is wrapped in a function instead of being loaded with the shell.

```sh
loadnvm
nvm install --lts
node --version
```

## Elpa fail

As of October 6th, 2019, elpa has been down for at least 2 weeks, preventing package installation from melpa.

Self contain reproduction of the issue (and check if it is still an issue):

```sh
docker run -it --rm ubuntu:19.04 bash -c 'apt-get update && apt-get install -y ca-certificates emacs-nox && emacs --no-init-file --eval "(progn (package-initialize) (package-refresh-contents))"'
```

Temporaty fix:

```sh
mkdir -p ~/elpaclone && cd ~/elpaclone && curl -L https://elpa.gnu.org/packages/archive-contents | perl -pe 's/(^\(1|\n)//g' | perl -pe 's/\]\)/])\n/g' | perl -pe 's/^ *\(([a-z0-9A-Z-]*).*\[\(([0-9 ]*).*(single|tar).*/\1-\2.\3/g' | perl -pe 's/ /./g' | perl -pe 's/single/el/g' | perl -pe 's/\)//g' | xargs -I {} curl -L  -O https://elpa.gnu.org/packages/\{\} && curl -L -O https://elpa.gnu.org/packages/archive-contents
```

```lisp
;; init-package.el

(setq package-archives '(("gnu" . "~/elpaclone")
                        ("melpa" . "https://melpa.org/packages/")))
```
