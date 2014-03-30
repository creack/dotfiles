FROM		ubuntu:13.10
MAINTAINER	Guillaume J. Charmes <guillaume@charmes.net>

RUN		apt-get update
RUN		apt-get install -y curl	most unzip tmux		\
				   fish emacs24			\
				   git mercurial		\
				   man-db locales		\
				   build-essential python-pip	\
				   pkg-config automake autoconf	\
				   iptables tcpdump ngrep iotop

### Docker deps  ###
RUN		apt-get install -y aufs-tools btrfs-tools libsqlite3-dev libapparmor-dev libcap-dev
# Get and compile LXC 1.0
RUN   	  	git clone --no-checkout https://github.com/lxc/lxc.git /usr/local/lxc && cd /usr/local/lxc && git checkout -q lxc-1.0.0
RUN		cd /usr/local/lxc && ./autogen.sh && ./configure --disable-docs && make && make install
# Get and compile lvm2 (devmapper)
RUN		git clone --no-checkout https://git.fedorahosted.org/git/lvm2.git /usr/local/lvm2 && cd /usr/local/lvm2 && git checkout -q v2_02_103
RUN		cd /usr/local/lvm2 && ./configure --enable-static_link && make device-mapper && make install_device-mapper
### /Docker deps ###

RUN		dpkg-reconfigure locales
#RUN		locale-gen C.UTF-8
RUN		/usr/sbin/update-locale LANG=C.UTF-8

RUN		echo America/Los_Angeles > /etc/timezone && dpkg-reconfigure --frontend noninteractive tzdata;
RUN		pip install git+git://github.com/Lokaltog/powerline

RUN		chsh -s /usr/bin/fish
RUN		curl -s https://go.googlecode.com/files/go1.2.1.linux-amd64.tar.gz | tar -v -C /root -xz && mv /root/go /root/goroot

ENV		HOME 	/root
ENV		LC_ALL	C.UTF-8
ENV		TERM	xterm-256color

ENV		GOROOT	$HOME/goroot
ENV		GOPATH	$HOME/go
ENV		GOBIN	$GOROOT/bin
ENV		PATH	$GOBIN:$PATH

WORKDIR		/root
CMD		["fish"]


# Install golang common utils
RUN		go get -u code.google.com/p/rog-go/exp/cmd/godef
RUN		go get -u code.google.com/p/go.tools/cmd/cover
RUN		go get -u github.com/nsf/gocode
RUN		go get -u github.com/dougm/goflymake
RUN		go get -u github.com/axw/gocov/gocov

# Get docker sources
RUN		go get -u github.com/dotcloud/docker/docker
RUN		ln -s $HOME/go/src/github.com/dotcloud/docker $HOME/docker

RUN		fish -c fish_update_completions
ADD		.	/root/.dotfiles
RUN		cd /root/.dotfiles && make
