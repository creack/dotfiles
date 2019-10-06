#!/usr/bin/env zsh

function echo2() {
    echo $@ >&2
}

function true-color() {
    echo2 "Color test:"
    bash ~/.dotfiles/truecolor-test.sh >&2
    echo2 "Termcap test:"
    echo2 "$(tput smul 1)underline$(tput rmul 1)"
    echo2 "$(tput blink 1)blink$(tput sgr0)"
    echo2 "$(tput bold 1)bold$(tput sgr0)"
    echo2 "$(tput dim 1)dim$(tput sgr0)"
    echo2 -e "\e[9mstrikethrough\e[0m"
    echo2 "$(tput sitm)italics$(tput ritm)"
    echo2 "regular"
    echo2
    echo2 "image test:"
}

# Run with:
#   ./truecolor-test.sh
# or
#   source truecolor-test.sh && true-color

# Setup with:
#   tic -x -o ~/.terminfo ~/.dotfiles/xterm-truecolor.terminfo

true-color
