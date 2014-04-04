#!/usr/bin/fish

function setup_resolvconf --description "Make sure resolv.conf is set on 8.8.8.8"
	if not test -d /run/resolvconf/resolv.conf
		sudo bash -c 'echo "nameserver 8.8.8.8" > /run/resolvconf/resolv.conf'
		sudo bash -c 'echo "nameserver 8.8.4.4" >> /run/resolvconf/resolv.conf'
	end
end
