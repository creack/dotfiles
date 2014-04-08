#!/usr/bin/fish

function di --description "Build and install docker"
	if not test "$USER" = 'root'
		sudo -sE di $argv
		return $status
	end
	set -l OLDPWD (pwd)
	set -l error 1
	set -l VERSION (cat ~/docker/VERSION)

	set -lx DOCKER_BUILDTAGS "apparmor selinux"
	set -lx GOPATH (pwd)/vendor:$GOPATH

	clean
	cd ~/docker

  	if hack/make.sh binary; and cp bundles/$VERSION/binary/docker-$VERSION ~/goroot/bin/docker
		set error 0
	end

	cd $OLDPWD
	return $error
end

function dockerb --description "Start docker daemon with btrfs"
	if not test "$USER" = 'root'
	   sudo -sE dockerb $argv
	   return $status
	end
	if test "$PLATFORM" = 'Linux'
		if not mount | grep docker > /dev/null
       			echo "------------- Mounting /dev/sdb to /var/lib/docker-btrfs -----------"
       			mkdir -p /var/lib/docker-btrfs
       			mount /dev/sdb /var/lib/docker-btrfs
		end
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s btrfs -g /var/lib/docker-btrfs $argv
end

function dockerdm --description "Start docker daemon with devicemapper"
	if not test "$USER" = 'root'
	   sudo -sE dockerdm $argv
	   return $status
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s devicemapper -g /var/lib/docker-dm $argv
end

function dockera --description "Start docker daemon with devicemapper"
	if not test "$USER" = 'root'
	   sudo -sE dockera $argv
	   return $status
	end
	docker -d -H tcp://0.0.0.0:4243 -H unix:///var/run/docker.sock --dns 8.8.8.8 --dns 8.8.4.4 -s aufs -g /var/lib/docker-aufs $argv
end
