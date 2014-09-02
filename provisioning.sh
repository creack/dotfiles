#!/bin/bash
#
#
# WARNING: This provisioning script might be very generic,
# however, it does populate my public key. If someone is to
# use it, be careful to remove it. (at the end of the file)
#
set -e
set -x

echo "current user: $USER, target user: $1"

if [ "$USER" != "root" ]
then
    exec sudo ./provisioning.sh $(whoami)
    exit 1
fi

if [ "$1" = "" ]
then
    echo "USAGE: sudo $0 or $0 <user>"
    exit 1
fi

target_home=$(eval echo ~$1)

function test {
export DEBIAN_FRONTEND=noninteractive
apt-get update && apt-get dist-upgrade -y && apt-get autoremove -y

apt-get install -y \
    curl most unzip tmux emacs24-nox git mercurial man-db locales htop software-properties-common \
    build-essential python-pip pkg-config automake autoconf \
    iptables tcpdump ngrep iotop watch htop \
    gnupg2 gnupg-agent keychain pinentry-curses \
    protobuf-compiler libprotobuf-dev zlib1g-dev openssl libssl-dev libncurses5-dev

# install latest fish version
add-apt-repository -y ppa:fish-shell/nightly-master && apt-get update && apt-get install -y fish
chsh -s /usr/bin/fish guillaume

### Docker deps  ###
apt-get install -y aufs-tools btrfs-tools cgroup-lite libsqlite3-dev libapparmor-dev libcap-dev
# Get and compile LXC 1.0
git clone --no-checkout https://github.com/lxc/lxc.git /usr/local/lxc && cd /usr/local/lxc && git checkout -q lxc-1.0.0
cd /usr/local/lxc && ./autogen.sh && ./configure --disable-docs && make && make install
# Get and compile lvm2 (devmapper)
git clone --no-checkout https://git.fedorahosted.org/git/lvm2.git /usr/local/lvm2 && cd /usr/local/lvm2 && git checkout -q v2_02_103
cd /usr/local/lvm2 && ./configure --enable-static_link && make device-mapper && make install_device-mapper
### /Docker deps ###

# Make sure the locale is C.UTF-8
dpkg-reconfigure locales
locale-gen en_US.UTF-8
/usr/sbin/update-locale LANG=en_US.UTF-8

# Set the timezone
echo America/Los_Angeles > /etc/timezone && dpkg-reconfigure --frontend noninteractive tzdata;

# Install latest mosh
rm -rf /tmp/mosh
git clone https://github.com/keithw/mosh /tmp/mosh
cd /tmp/mosh
./autogen.sh
./configure
make
make install

# Install Golang
cd $target_home
[ -d goroot ] || hg clone https://code.google.com/p/go goroot
cd goroot/src && hg up release && ./all.bash

export GOBIN=$target_home/goroot/bin
export GOPATH=$target_home/go
export PATH=$GOBIN:$PATH
# Install golang common utils
go get -u code.google.com/p/go.tools/cmd/cover
go get -u code.google.com/p/go.tools/cmd/godoc
go get -u code.google.com/p/go.tools/cmd/goimports
go get -u code.google.com/p/go.tools/cmd/oracle
go get -u code.google.com/p/go.tools/cmd/vet
go get -u code.google.com/p/rog-go/exp/cmd/godef
go get -u github.com/axw/gocov/gocov
go get -u github.com/dougm/goflymake
go get -u github.com/golang/lint/golint
go get -u github.com/nsf/gocode


# Get docker sources
go get -u github.com/dotcloud/docker/docker

[ -L $target_home/docker ] || ln -s $target_home/go/src/github.com/dotcloud/docker $target_home/docker
cd docker && git remote add creack https://github.com/creack/docker.git && git fetch --all
addgroup docker && usermod -a -G docker $1
# Install powerline (tmux status bar)
pip install git+git://github.com/Lokaltog/powerline

# Update fish completion (based on mans, so do it last)
fish -c fish_update_completions

#ADD .	/root/.dotfiles

# Install the dotfiles
cd $target_home/.dotfiles
chown -R $1:$1 $target_home
su $1 -c make


# Setup the sudoer file
echo 'root    ALL=(ALL:ALL) ALL' > /etc/sudoers.d/root
echo "$1 ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$1
echo '#includedir /etc/sudoers.d' > /etc/sudoers

# Enable gpg-agent
echo use-agent >> ~/.gnupg/gpg.conf
echo 'pinentry-program /usr/bin/pinentry-curses' >> ~/.gnupg/gpg-agent.conf

# Add user to trusted mercurial users
echo "[trusted]
users = root $1" > /etc/mercurial/hgrc.d/trust.rc
chmod 644 /etc/mercurial/hgrc.d/trust.rc

# Setup my public ssh key
mkdir -p $target_home/.ssh
echo 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCTRpitHLsmeDCT7N7ZJWh+EjDAFdsy9i7a7A32hPBmMFUBgzkj/Dtaivx75RUQGkGKoR0DH1NGYWY0G5sTI1QLCw4rrcfOfFUvDtfkPF2qibecLj5x3DRtwikqjJNNj/3DsWBDZzJZ0nQDixn73G55Dy2QEGTT4ok3MgbWQaWjbfYz2kNsP6F20JkKbz+Z3V8QLXdTtFg0Cnh7Zme3N6RA38lWX2/njT+B2X7Gbhb7LQlzNWf4RSAvokYcpMs/F50dW1E+DFAAgdsjjEMN/uVaI9eVlQytS6R9oUqo867WtLOzq77KIFrRcJRm0U4M+Z4OHQBikkQJ8+TNb/gHeiDf guillaume.charmes' >> $target_home/.ssh/authorized_keys
}

#install golang hg extenstion
echo "
[extensions]
codereview = $target_home/goroot/lib/codereview/codereview.py

[ui]
username = Guillaume J. Charmes <guillaume@charmes.net>" >> $target_home/goroot/.hg/hgrc
