#!/usr/bin/fish

# Initialize tmux (check for 256 colors, create or attach to session)
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
	set -l session_name $hostname

	# If the session does not exist, create it
	if not tmux has-session -t $session_name 2> /dev/null
	        tmux new-session -d -s $session_name
	end

	# Attach to the session
  	exec tmux attach-session -t $session_name
end
