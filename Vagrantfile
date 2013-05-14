# -*- mode: ruby -*-
# vi: set ft=ruby :

BOX_NAME = "docker-dev"
BOX_URI = "http://cloud-images.ubuntu.com/vagrant/raring/current/raring-server-cloudimg-amd64-vagrant-disk1.box"
GIT_USERNAME = ENV['GIT_USERNAME']
GIT_EMAIL = ENV['GIT_EMAIL']

Vagrant::Config.run do |config|
  # Setup virtual machine box. This VM configuration code is always executed.
  config.vm.box = BOX_NAME
  config.vm.box_url = BOX_URI

  ### Running first time
  pkg_cmd = "[ -f /usr/bin/git ] || ("

  # Install basic env, git/mercurial, docker dependencies, emacs, zsh, etc
  pkg_cmd << "apt-get update -qq; export DEBIAN_FRONTEND=noninteractive; apt-get install -q -y build-essential mercurial git lxc bsdtar htop most emacs24 zsh tmux; "

  # Change default shell
  pkg_cmd << "chsh -s /bin/zsh vagrant; "

  # Retrieve the dotfiles config
  pkg_cmd << "git clone https://github.com/creack/dotfiles /home/vagrant/.dotfiles; cd /home/vagrant/.dotfiles; "
  pkg_cmd << "HOME=/home/vagrant make; "
  pkg_cmd << "[[ -n $GIT_USERNAME ]] && git config --global user.name $GIT_USERNAME; "
  pkg_cmd << "[[ -n $GIT_EMAIL ]] && git config --global user.email $GIT_EMAIL; "

  # Checkout golang source and install them
  pkg_cmd << "echo 'Cloning Go repository'; hg clone http://code.google.com/p/go /home/vagrant/goroot; "
  pkg_cmd << "cd /home/vagrant/goroot/src; ./all.bash; "

  # Make sure the GOPATH and GOROOT are correctly set
  pkg_cmd << "echo 'export GOPATH=~/go' >> /home/vagrant/.zshrc; export GOPATH=/home/vagrant/go; "
  pkg_cmd << "echo 'export GOROOT=~/goroot' >> /home/vagrant/.zshrc; export GOROOT=/home/vagrant/goroot; "
  pkg_cmd << "echo 'export GOBIN=$GOROOT/bin' >> /home/vagrant/.zshrc; export GOBIN=$GOROOT/bin; "
  pkg_cmd << "echo 'export PATH=$GOBIN:$PATH' >> /home/vagrant/.zshrc; export PATH=$GOBIN:$PATH; "

  # Install godef for symbol/tags lookup
  pkg_cmd << "go get code.google.com/p/rog-go/exp/cmd/godef; "
  # Install gocode completion
  pkg_cmd << "go get github.com/nsf/gocode; "
  # Install goflymake
  pkg_cmd << "go get github.com/dougm/goflymake; "

  # Checkout docker sources
  pkg_cmd << "go get github.com/dotcloud/docker; ln -s /home/vagrant/go/src/github.com/dotcloud/docker /home/vagrant/docker; "
  pkg_cmd << "cd /home/vagrant/docker; git checkout master; "

  # Make sure we have the correct permissions
  pkg_cmd << "chown -R vagrant:vagrant /home/vagrant; "

  pkg_cmd << "); "

  config.vm.provision :shell, :inline => pkg_cmd
end

Vagrant::VERSION >= "1.1.0" and Vagrant.configure("2") do |config|
  # Specific virtual box config: use only 1 CPU
  config.vm.provider "virtualbox" do |v|
    v.name = BOX_NAME
    v.customize ["modifyvm", :id, "--cpus", "1"]
  end

  config.vm.network  :private_network, ip: "192.168.142.142"
end
