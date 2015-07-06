LN	=	ln -fFs
SRCS	=	dot.emacs	\
		dot.emacs.files	\
		dot.zshrc	\
		dot.zsh_go	\
		dot.zsh_docker	\
		dot.zsh_git_prompt \
		dot.gitconfig	\
		dot.gdbinit	\
		dot.tmux.conf	\
		dot.config
OBJS	=	$(subst dot, ${HOME}/, $(SRCS))

all	: $(OBJS) go zsh
	mkdir -p ${HOME}/.emacs.files/tmp
	mkdir -p ${HOME}/.ssh
	touch ${HOME}/.ssh/config
	touch ${HOME}/.ssh/known_hosts

zsh	: ${HOME}/.oh-my-zsh
	sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

${HOME}/goroot:
	git clone https://github.com/golang/go ${HOME}/goroot
	cd ${HOME}/goroot/src && git checkout go1.4.2 && ./make.bash

go	: ${HOME}/goroot
	go get -u code.google.com/p/rog-go/exp/cmd/godef
	go get -u github.com/jstemmer/gotags
	go get -u golang.org/x/tools/cmd/gorename
	go get -u golang.org/x/tools/cmd/oracle
	go get -u golang.org/x/tools/cmd/goimports
	go get -u golang.org/x/tools/cmd/benchcmp
	go get -u golang.org/x/tools/cmd/cover
	go get -u golang.org/x/tools/cmd/godoc
	go get -u golang.org/x/tools/cmd/stringer
	go get -u golang.org/x/tools/cmd/vet

powerline:
	sudo pip install powerline-status

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
