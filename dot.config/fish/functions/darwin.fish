#!/usr/bin/fish

if not test "$PLATFORM" = 'Darwin'
	exit 0
end

function setup_darwin_aliases --description "Setup aliases specific for darwin"
	set -x CLICOLOR 1
	#   set -x LSCOLORS gxBxhxDxfxhxhxhxhxcxcx
	set -x LSCOLORS ExFxCxDxBxegedabagacad
	alias ls="ls -G -lh"
	alias updatedb="/usr/libexec/locate.updatedb"
end

function bootdev --description "Start and connect to dev environment"
	set -l OLDPWD (pwd)

	cd ~/dotfiles
	vagrant up
	cd $OLDPWD

	mosh dev
end
