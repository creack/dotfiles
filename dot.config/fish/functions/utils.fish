#!/usr/bin/fish

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

function \\
        eval command $argv
end
