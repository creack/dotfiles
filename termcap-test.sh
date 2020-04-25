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

function termcaps2() {
echo -e '\e[1mbold\e[22m'
echo -e '\e[2mdim\e[22m'
echo -e '\e[3mitalic\e[23m'
echo -e '\e[4munderline\e[24m'
echo -e '\e[4:1mthis is also underline (new in 0.52)\e[4:0m'
echo -e '\e[21mdouble underline (new in 0.52)\e[24m'
echo -e '\e[4:2mthis is also double underline (new in 0.52)\e[4:0m'
echo -e '\e[4:3mcurly underline (new in 0.52)\e[4:0m'
echo -e '\e[5mblink (new in 0.52)\e[25m'
echo -e '\e[7mreverse\e[27m'
echo -e '\e[8minvisible\e[28m <- invisible (but copy-pasteable)'
echo -e '\e[9mstrikethrough\e[29m'
echo -e '\e[53moverline (new in 0.52)\e[55m'

echo -e '\e[31mred\e[39m'
echo -e '\e[91mbright red\e[39m'
echo -e '\e[38:5:42m256-color, de jure standard (ITU-T T.416)\e[39m'
echo -e '\e[38;5;42m256-color, de facto standard (commonly used)\e[39m'
echo -e '\e[38:2::240:143:104mtruecolor, de jure standard (ITU-T T.416) (new in 0.52)\e[39m'
echo -e '\e[38:2:240:143:104mtruecolor, rarely used incorrect format (might be removed at some point)\e[39m'
echo -e '\e[38;2;240;143;104mtruecolor, de facto standard (commonly used)\e[39m'

echo -e '\e[46mcyan background\e[49m'
echo -e '\e[106mbright cyan background\e[49m'
echo -e '\e[48:5:42m256-color background, de jure standard (ITU-T T.416)\e[49m'
echo -e '\e[48;5;42m256-color background, de facto standard (commonly used)\e[49m'
echo -e '\e[48:2::240:143:104mtruecolor background, de jure standard (ITU-T T.416) (new in 0.52)\e[49m'
echo -e '\e[48:2:240:143:104mtruecolor background, rarely used incorrect format (might be removed at some point)\e[49m'
echo -e '\e[48;2;240;143;104mtruecolor background, de facto standard (commonly used)\e[49m'

echo -e '\e[21m\e[58:5:42m256-color underline (new in 0.52)\e[59m\e[24m'
echo -e '\e[21m\e[58;5;42m256-color underline (new in 0.52)\e[59m\e[24m'
echo -e '\e[4:3m\e[58:2::240:143:104mtruecolor underline (new in 0.52) (*)\e[59m\e[4:0m'
echo -e '\e[4:3m\e[58:2:240:143:104mtruecolor underline (new in 0.52) (might be removed at some point) (*)\e[59m\e[4:0m'
echo -e '\e[4:3m\e[58;2;240;143;104mtruecolor underline (new in 0.52) (*)\e[59m\e[4:0m'

echo -e '\e]8;;http://askubuntu.com\e\\hyperlink\e]8;;\e\\'
}


true-color
termcaps2
