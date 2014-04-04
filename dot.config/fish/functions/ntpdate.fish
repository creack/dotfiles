#!/usr/bin/fish

function ntpdate_autoupdate --description "Start a silent ntpdate in background (only in interactive mode)"
	if not status --is-interactive
		sudo ntpdate time.apple.com > /dev/null ^ /dev/null &
	end
end
