# Use the config only for interactive mode
status --is-interactive; or exit 0

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

test -z $TMUX
     and exec tmux attach

# Path to your oh-my-fish.
set fish_path $HOME/.oh-my-fish

# Theme
set fish_theme zish

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
set fish_plugins autojump bundler git tmux brew

# Path to your custom folder (default path is $FISH/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# mac bc read the conf file to allow floating point maths
# and load the standard library
set -x BC_ENV_ARGS "$HOME/.bcrc -l"

# Load oh-my-fish configuration.
. $fish_path/oh-my-fish.fish

# Darwin Specific
set PLATFORM (uname)
if test "$PLATFORM" = 'Darwin'
    set -x CLICOLOR 1
#    set -x LSCOLORS gxBxhxDxfxhxhxhxhxcxcx
    set -x LSCOLORS ExFxCxDxBxegedabagacad
    alias ls="ls -G -l"
    alias updatedb="/usr/libexec/locate.updatedb"
else
# Others
    test -x /usr/bin/keychain
        and test -r ~/.ssh/id_rsa
        and eval (keychain --nogui --quiet --eval ~/.ssh/id_rsa)
       or echo "Missing keychain"
    alias ls="ls --color=auto -l"
    alias emacs="emacsclient -t -c -a=''"
end

# totally worth it
if not test -d ~/.config/fish/generated_completions/
   echo "One moment..."
   fish_update_completions
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
set -x TERM "xterm-256color"
set -x GPGKEY CB6E3FF3
set -x GPG_TTY (tty)

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

function di --description "Build and install docker"
    set -l OLDPWD (pwd)
    set -l error 1
    set -l VERSION (cat VERSION)

    cd ~/docker
        and set -lx GOPATH (pwd)/vendor:$GOPATH
   	and sudo -E hack/make.sh binary
	and cp bundles/$VERSION/binary/docker-$VERSION ~/goroot/bin/docker
	and set -l error 0

    cd $OLDPWD;
    return $error
end
