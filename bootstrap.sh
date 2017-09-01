#!/usr/bin/env zsh

autoload -U colors
colors

function assert() {
    echo -n "$1\t"
    eval "$2" >& /dev/null && echo "[$fg_bold[green]OK$reset_color]" || echo "[$fg_bold[red]KO$reset_color]"
}

function dryrun_check() {
    export hasbrew=$(hash brew >& /dev/null                                                                      && echo true || echo false)
    export haspython=$(hash python >& /dev/null                                                                  && echo true || echo false)
    export haspip=$($haspython && [ -f /usr/local/bin/pip ]                                                      && echo true || echo false)
    export hasdockermachine=$(hash docker-machine >& /dev/null                                                   && echo true || echo false)
    export zshwhitelisted=$(cat /etc/shells | \grep /usr/local/bin/zsh >& /dev/null                              && echo true || echo false)
    export zshdefault=$([ "$(dscl . -read /Users/$(whoami) UserShell | sed 's/.*: //')" = "/usr/local/bin/zsh" ] && echo true || echo false)
    export dotfilesuptodate=$(make --dry-run install_dotfiles | \grep "Nothing to be done" >& /dev/null          && echo true || echo false)
    export hassshdir=$([ -d "/Users/$(whoami)/.ssh" ]                                                            && echo true || echo false)

    if $haspip; then
	tput sc
	echo "Checking pip versions..."
	export pipfreeze=$(/usr/local/bin/pip freeze)
	export pipoutdated=$(/usr/local/bin/pip list --outdated --format=columns)
	tput rc; tput el
    fi

    if $hasbrew; then
	tput sc
	echo "Checking brew taps..."
	export brewtaps=$(brew tap)
	tput rc; tput el
    fi
    export hasbrewbundle=$(echo "$brewtaps" | \grep homebrew/bundle >& /dev/null && echo true || echo false)

    export bundle_content=false
    if $hasbrew && $hasbrewbundle; then
	tput sc
	echo "Checking brew bundle content..."
	export bundle_content=$(brew bundle --global check >& /dev/null && echo true || echo false)
	tput rc; tput el
    fi

    if $hasdockermachine; then
	tput sc
	echo "Loading docker-machine name from zshrc..."
	export dockermachine_name=$(/usr/local/bin/zsh -c 'export ZSH_TMUX_AUTOSTART=false; source .zshrc >& /dev/null; echo $DOCKER_MACHINE_NAME')
	tput rc; tput el
    fi
    export hasdockermachinedefault=false
    if $hasdockermachine; then
	export hasdockermachinedefault=$(docker-machine env $dockermachine_name >& /dev/null && echo true || echo false)
    fi

    export haspowerline=false
    if $haspip; then
	export haspowerline=$((echo "$pipfreeze" | \grep powerline-status && echo "$pipfreeze" | \grep psutil && echo "$pipfreeze" | \grep powerline-gitstatus) >& /dev/null && echo true || echo false)
    fi
    export powerlineuptodate=false
    if $haspowerline; then
	export powerlineuptodate=$((! echo "$pipoutdated" | \grep powerline-status && ! echo $pipoutdated | \grep psutil && ! echo $pipoutdated | \grep powerline-gitstatus) >& /dev/null && echo true || echo false)
    fi
}

function dryrun_out() {
      echo "Operation\tResult"
      echo "---------\t------"
      echo ".\t"
      assert "├── Dotfiles"                                      '$dotfilesuptodate'
      assert "├── SSH Dir"                                       "$hassshdir"
      assert "├── ZSH"                                           "$zshwhitelisted && $zshdefault"
      assert "│   ├── Whitelisted"                               "$zshwhitelisted"
      assert "│   └── Default Shell"                             "$zshdefault"
      assert "├── Homebrew"                                      "$hasbrew && $hasbrewbundle && $bundle_content"
      assert "│   ├── Core"                                      "$hasbrew"
      assert "│   └── Bundle"                                    "$hasbrewbundle"
      assert "│       └── Up to date"                            "$bundle_content"
      assert "├── Python"                                        "$haspython"
      assert "│   └── Powerline"                                 "$haspowerline && $powerlineuptodate"
      $haspowerline && assert "│       └── Up to date"           "$powerlineuptodate"
      assert "├── Docker"                                        "$hasdockermachine && $hasdockermachinedefault"
      assert "│   └── Docker-machine binary"                     "$hasdockermachine"
      assert "│       └── Docker-machine '$dockermachine_name'"  "$hasdockermachinedefault"
      assert "└── Better Touch Tools Config"                     "[ -f ~/Library/Application\ Support/BetterTouchTool/bttdata2 ]"
}

function dryrun_out_cols() {
    dryrun_out | column -t -s "$(echo -ne "\t")"
}

function dryrun() {
    dryrun_check
    dryrun_out_cols
}

dryrun_check
out=$(dryrun_out)
dryrun_out_cols
if ! echo $out | \grep KO >& /dev/null; then
    echo "Nothing to do."
    exit 0
fi

if [  "$1" != "-y" ]; then
    echo "Will run the [KO] lines. Are you sure? [y/N]" >& 2
    read REPLY

    if [ "$REPLY" != "Y" ] && [ "$REPLY" != "y" ]; then
	echo "Cancelled" >& 2
	exit 2
    fi
fi

# Install the dotfiles.
if ! $dotfilesuptodate; then
    echo "Dotfiles make not satisfied. Running 'make install_dotfiles'."
    make install_dotfiles
    dotfilesuptodate=true
fi

# Create ssh dir.
if ! $hassshdir; then
    mkdir /Users/$(whoami)/.ssh
    hassshdir=true
fi

# Install brew.
if ! $hasbrew; then
    echo "Homebrew is missing. Installing it."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    hasbrew=$(hash brew >& /dev/null && echo true || echo false)
fi

# Install homebrew bundle.

## Make sure we have python.
if ! $haspython; then
    echo "Python is missing. Installing it."
    brew install python
    haspython=$(hash python >& /dev/null && echo true || echo false)
fi

## Tap bundle.
if ! $hasbrew; then
    echo "Bootstrap failed to install Homebrew." >& 2
    ret=1
elif ! $hasbrewbundle; then
    echo "Missing homebrew/bundle. Tapping it."
    brew tap homebrew/bundle
    hasbrewbundle=$(brew tap | \grep homebrew/bundle >& /dev/null && echo true || echo false)
fi

# Execute the Brewfile bundle.
if ! $haspython; then
    echo "Bootstrap failed to install python." >& 2
    ret=1
elif ! $bundle_content; then
    echo "Homebrew bundle not satisfied. Running bundle."
    brew bundle --global
    bundle_content=true
    hasdockermachine=$(hash docker-machine >& /dev/null && echo true || echo false)
    haspip=$([ -f /usr/local/bin/pip ] && echo true || echo false)
fi

# Install powerline.
if ! $haspip; then
    echo "Brew failed to install pip." >& 2
    ret=1
elif ! $haspowerline; then
    echo "Missing powerline-status and/or psutil. Installing them."
    /usr/local/bin/pip install --upgrade powerline-status psutil powerline-gitstatus
    haspowerline=true
    powerlineuptodate=true
elif ! $powerlineuptodate; then
    echo "Outdated powerline-status and/or psutil. Updating them."
    /usr/local/bin/pip install --upgrade powerline-status psutil powerline-gitstatus
    powerlineuptodate=true
fi

# Install docker VM.
if ! $hasdockermachine; then
    echo "Brew failed to install docker-machine." >&2
    ret=1
elif ! $hasdockermachinedefault; then
    echo "Missing docker-machine '$dockermachine_name'. Creating it."
    docker-machine create --driver=virtualbox --virtualbox-disk-size=60000 --virtualbox-cpu-count=4 $dockermachine_name
    rm -f /tmp/.dockercache /tmp/.dockerip
    eval $(docker-machine env $dockermachine_name)
    export DOCKER_IP=$(docker-machine ip $dockermachine_name)
    hasdockermachinedefault=$(docker-machine env $dockermachine_name >& /dev/null && echo true || echo false)
fi

# Configure bettertouchtools.
if $hasbrewbundle && [[ -d /Applications/BetterTouchTool.app && ! -d ~/Library/Application\ Support/BetterTouchTool ]]; then
    echo "Missing BetterTouchTools settings. Creating it."
    mkdir -p ~/Library/Application\ Support/BetterTouchTool
    cp bttdata2 ~/Library/Application\ Support/BetterTouchTool
fi

# Whitelist zsh.
if ! $zshwhitelisted; then
    echo "Homebrew's zsh not whiltelisted in /etc/shells. Whitelisting it. (Requires sudo password)."
    sudo sh -c 'echo /usr/local/bin/zsh >> /etc/shells'
    zshwhitelisted=$(cat /etc/shells | \grep /usr/local/bin/zsh >& /dev/null && echo true || echo false)
fi

# Set zsh to default.
if [ ! -f /usr/local/bin/zsh ]; then
    echo "Brew failed to install zsh." >& 2
    ret=1
elif ! $zshdefault; then
    echo "User '$(whoami)' default shell is not set to Homebrew's zsh. Setting it. (Requires user password)."
    chsh -s /usr/local/bin/zsh $(whoami)
    zshdefault=$([ "$(dscl . -read /Users/$(whoami) UserShell | sed 's/.*: //')" = "/usr/local/bin/zsh" ] && echo true || echo false)
fi

dryrun

[ -z "$ret" ] && ret=0
exit $ret
