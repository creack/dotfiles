#!/usr/bin/fish

function setup_env --description "Setup global environment"
	# if you call a different shell, this does not happen automatically. WTF?
	set -gx SHELL (which fish)

	# available since 4.8.0
	set -gx GCC_COLORS 1

	set -gx HOSTNAME (hostname)
	set -gx PLATFORM (uname -s)
	set -gx ARCH (uname -m)
	set -gx GOBIN $HOME/goroot/bin
	set -gx GOPATH $HOME/go
	set -gx PATH $GOBIN $PATH

	set -gx PAGER "most"
	set -gx EDITOR "emacsclient -c -t -a=''"
	set -gx WATCH "all"
	set -gx LANG "en_US.UTF-8"
	set -gx GPGKEY CB6E3FF3
	set -x GPG_TTY (tty)
end
