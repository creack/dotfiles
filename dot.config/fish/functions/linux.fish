#!/usr/bin/fish

if not test "$PLATFORM" = 'Linux'
	exit 0
end

function setup_linux_aliases --description "Setup aliases specific for linux"
	alias ls="ls --color=auto -lh"
	alias emacs="emacsclient -t -c -a=''"
end
