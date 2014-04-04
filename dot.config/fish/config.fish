set -x HOSTNAME (hostname)
set -x PLATFORM (uname -s)
set -x ARCH (uname -m)

for script in (command ls $fish_function_path[1])
	source $fish_function_path[1]/$script
end

# Use the config only for interactive mode
if not status --is-interactive
	exit 0
end

# Initialize tmux (check for 256 colors, create or attach to session)
tmuxinit

if test "$PLATFORM" = 'Darwin'
	setup_darwin_aliases
end

if test "$PLATFORM" = 'Linux'
	setup_linux_aliases

	## Update time (assuming paused vm with lost clock)
	ntpdate_autoupdate

	## Override /etc/resolv.conf with 8.8.8.8
	# setup_resolvconf

	## Start and load gpg/ssh agent
	if test -x /usr/bin/keychain; and test -r ~/.ssh/id_rsa
	        set -l keychain (keychain --nogui --quiet --eval ~/.ssh/id_rsa)
	    	for i in $keychain
	    		if test "$i" != ""
				eval (echo $i)
			else
				continue
			end
		end
		and begin
			if not ssh-add -l > /dev/null
				ssh-add -t 76000
			end
		end
	else
		echo " ---- Missing keychain ----"
	end
end

# Update the completion if they do not exists
if not test -d ~/.config/fish/generated_completions/
	echo "One moment..."
	fish_update_completions
end

## Setup global environment
setup_env
