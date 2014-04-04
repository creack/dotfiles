#!/usr/bin/fish

function setup_env --description "Setup global environment"
	# if you call a different shell, this does not happen automatically. WTF?
	set -x SHELL (which fish)

	# available since 4.8.0
	set -x GCC_COLORS 1

	set -x HOSTNAME (hostname)
	set -x PLATFORM (uname -s)
	set -x ARCH (uname -m)
	set -x GOBIN $HOME/goroot/bin
	set -x GOPATH $HOME/go
	set -x PATH $GOBIN $PATH

	set -x PAGER "most"
	set -x EDITOR "emacsclient -c -t -a=''"
	set -x WATCH "all"
	set -x LANG "en_US.UTF-8"
	set -x GPGKEY CB6E3FF3
	#set -x GPG_TTY (tty)
end
