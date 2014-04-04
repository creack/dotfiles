#!/usr/bin/fish

## Theme zish from oh-my-zish

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
