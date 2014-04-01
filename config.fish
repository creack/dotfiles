# Use the config only for interactive mode

set -x HOSTNAME (hostname)
set -x PLATFORM (uname -s)
set -x ARCH (uname -m)
set -x USER (whoami)

function tmuxinit --description "Initialize tmux (check for 256 colors, create or attach to session)"
	 if not status --is-interactive
	    return 0
	 end

	# If we already are in a tmux, skip
	if test $TMUX
		echo "--> Already in tmux"
		return 0
	end

	# Check for 256 colors support

	# TERM TYPE Inside screen/tmux, it should be screen-256color -- this is
	# configured in .tmux.conf.  Outside, it's up to you to make sure your terminal
	# is configured to provide the correct, 256 color terminal type. For putty,
	# it's putty-256color (which fixes a lot of things) and otherwise it's probably
	# something ilike xterm-256color. Most, if not all off the terminals I use
	# support 256 colors, so it's safe to force it as a last resort, but warn.
	if test (tput colors) -ne 256
		set -x TERM xterm-256color
		set_color red
		echo "> TERM '$TERM' is not a 256 colour type! Overriding to xterm-256color. Please set. EG: Putty should have putty-256color."
		set_color normal
	end

	# Set session name
	set -l hostname local
	if test "$HOSTNAME"
		set hostname $HOSTNAME
	end

	set -l session_name $hostname

	# If the session does not exist, create it
	if not tmux has-session -t $session_name 2> /dev/null
	        tmux new-session -d -s $session_name
	end

	# Attach to the session
  	exec tmux attach-session -t $session_name
end
# Initialize tmux (check for 256 colors, create or attach to session)
tmuxinit

if test "$PLATFORM" = 'Darwin'
	set -x CLICOLOR 1
	#   set -x LSCOLORS gxBxhxDxfxhxhxhxhxcxcx
	set -x LSCOLORS ExFxCxDxBxegedabagacad
	alias ls="ls -G -lh"
	alias updatedb="/usr/libexec/locate.updatedb"
else
	test -x /usr/bin/keychain
         and test -r ~/.ssh/id_rsa
         and begin
	        set -l keychain (keychain --nogui --quiet --eval ~/.ssh/id_rsa)
	    	for i in $keychain
	    	    if test "$i" != ""
		       eval (echo $i)
		    else
			continue
		    end
	    	end
	    end
        or echo " ---- Missing keychain ----"
	and begin
	    if not ssh-add -l > /dev/null
	       ssh-add
	    end
        end

    alias ls="ls --color=auto -lh"
    alias emacs="emacsclient -t -c -a=''"
end

# totally worth it
if not test -d ~/.config/fish/generated_completions/
	echo "One moment..."
	fish_update_completions
end

# Make sure we have a resolv conf
if test "$PLATFORM" = 'Linux'
	if not test -d /run/resolvconf/resolv.conf
		sudo bash -c 'echo "nameserver 8.8.8.8" > /run/resolvconf/resolv.conf'
	end
end

# if you call a different shell, this does not happen automatically. WTF?
set -x SHELL (which fish)

# available since 4.8.0
set -x GCC_COLORS 1

set -x GOROOT ~/goroot
set -x GOBIN $GOROOT/bin
set -x GOPATH $HOME/go
set -x PATH $PATH $GOROOT/bin

set -x PAGER "most"
set -x EDITOR "emacsclient -c -t -a=''"
set -x WATCH "all"
set -x LANG "en_US.UTF-8"
set -x GPGKEY CB6E3FF3
set -x GPG_TTY (tty)

alias gocov="sudo -E ~/goroot/bin/gocov test -deps -exclude-goroot . | gocov report"
alias rm="rm -v"
alias grep="grep --color=auto -n"

function clean --description "Remove unwanted temp files"
	set -l directory .
	if set -q argv[1]
		set directory $argv[1]
	end
	find $directory -name 'flymake_*.go' -delete
	find $directory -name '.flymake_*.go' -delete
	find $directory -name '.\#*' -delete
	find $directory -name '*~' -delete
	find $directory -name '*.orig' -delete
	find $directory -name '*.test' -delete
end

function di --description "Build and install docker"
	if not test "$USER" = 'root'
	   sudo -sE di
	   return $status
	end
	set -l OLDPWD (pwd)
	set -l error 1
	set -l VERSION (cat ~/docker/VERSION)

	cd ~/docker
	 and clean
	 and set -lx GOPATH (pwd)/vendor:$GOPATH
  	 and hack/make.sh binary
	 and cp bundles/$VERSION/binary/docker-$VERSION ~/goroot/bin/docker
	 and set -l error 0

	cd $OLDPWD
	return $error
end

function dockerb --description "Start docker daemon with btrfs"
	if not test "$USER" = 'root'
	   sudo -sE dockerb
	   return $status
	end
	if test "$PLATFORM" = 'Linux'
		if not mount | grep docker > /dev/null
       			echo "------------- Mounting /dev/sdb to /var/lib/docker-btrfs -----------"
       			mkdir -p /var/lib/docker-btrfs
       			mount /dev/sdb /var/lib/docker-btrfs
		end
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s btrfs -g /var/lib/docker-btrfs $argv
end

function dockerdm --description "Start docker daemon with devicemapper"
	if not test "$USER" = 'root'
	   sudo -sE dockerdm
	   return $status
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s devicemapper -g /var/lib/docker-dm $argv
end

function dockera --description "Start docker daemon with devicemapper"
	if not test "$USER" = 'root'
	   sudo -sE dockera
	   return $status
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s aufs -g /var/lib/docker-aufs $argv
end

function \\
        eval command $argv
end

function bootdev --description "Start and connect to dev environment"
	if not test "$PLATFORM" = 'Darwin'
		return 1
	end

	set -l OLDPWD (pwd)

	cd ~/dotfiles
	vagrant up
	cd $OLDPWD

	mosh dev
end


## Theme zish from oh-my-zish
# name: Zish

function _is_git_dirty
	echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function fish_prompt
	set_color -o red
	printf "\n"
	printf '┌─<'
	set_color -o blue
	printf '%s ' (whoami)
	set_color $fish_color_autosuggestion[1]
	printf '@ '
	set_color cyan
	printf '%s ' (hostname|cut -d . -f 1)
	set_color $fish_color_autosuggestion[1]
	printf 'in '
	set_color -o green
	printf '%s' (prompt_pwd)
	set_color -o red
	printf '>'

	echo
	set_color -o red
	printf '└─<'
	set_color yellow
	printf '%s' (__fish_git_prompt)
	if [ (_is_git_dirty) ]
		set_color blue
		printf '* '
	end
	set_color -o red
	printf '>──'
	set_color yellow
	printf '» '
	set_color normal
end

function fish_right_prompt
	set -l exit_code $status
	if test $exit_code -ne 0
		set_color red
	else
		set_color green
	end
	echo $exit_code
	set_color yellow
	echo ' < '
	date +'%r'
	set_color normal
end
