# Use the config only for interactive mode
status --is-interactive; or exit 0

# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme zish

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
set fish_plugins autojump bundler git tmux

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# Set golang env
set -x GOROOT /usr/local/go
set -x GOBIN $GOROOT/bin
set -x GOPATH $HOME/development/gocode

# Set classic needed envs
set -x PAGER="most"
set -x EDITOR="emacsclient -c -t -a=''"
set -x WATCH="all"
set -x LANG="en_US.UTF-8"
set -x GPGKEY=CB6E3FF3
set -x GCC_COLORS 1

# Darwin Specific
set PLATFORM (uname)
if test "$PLATFORM" = 'Darwin'
    set -x CLICOLOR 1
#    set -x LSCOLORS gxBxhxDxfxhxhxhxhxcxcx
    set -x LSCOLORS ExFxCxDxBxegedabagacad
else
# Others
    test -x /usr/bin/keychain
        and test -r ~/.ssh/id_rsa
        and eval (keychain --nogui --quiet --eval ~/.ssh/id_rsa)
	or echo "Missing keychain"
    alias emacs="emacsclient -t -c -a=''"
end

# TERM TYPE Inside screen/tmux, it should be screen-256color -- this is
# configured in .tmux.conf.  Outside, it's up to you to make sure your terminal
# is configured to provide the correct, 256 color terminal type. For putty,
# it's putty-256color (which fixes a lot of things) and otherwise it's probably
# something ilike xterm-256color. Most, if not all off the terminals I use
# support 256 colors, so it's safe to force it as a last resort, but warn.
if begin; test -z $TMUX ; and test (tput colors) -ne 256; end
    set -x TERM xterm-256color
    set_color red
    echo "> TERM '$TERM' is not a 256 colour type! Overriding to xterm-256color. Please set. EG: Putty should have putty-256color."
    set_color normal
end

if command ls --version 1>/dev/null 2>/dev/null
   # This is GNU ls
   function ls --description "List contents of directory"
   	    set -l param --color=auto -lh
	    	if isatty 1
		   	  set param $param --indicator-style=classify
			      end
				command ls $param $argv
				end

				if not set -q LS_COLORS
				   if type -f dircolors >/dev/null
				      	   eval (dircolors -c | sed 's/>&\/dev\/null$//')
					   	end
						end

else
	# BSD, OS X and a few more support colors through the -G switch instead
	if command ls -G / 1>/dev/null 2>/dev/null
	   function ls --description "List contents of directory"
	   	       command ls -Glh $argv
		       	       end
			       end
end

alias gocov="sudo -E ~/goroot/bin/gocov test -deps -exclude-goroot . | gocov report"
alias rm="rm -v"
alias grep="grep --color=auto -n"

function clean --description "Remove unwanted temp files"
    find ./$argv[1] -name 'flymake_*.go' -delete
    find ./$argv[1] -name '.flymake_*.go' -delete
    find ./$argv[1] -name '.\#*' -delete
    find ./$argv[1] -name '*~' -delete
    find ./$argv[1] -name '*.orig' -delete
end
