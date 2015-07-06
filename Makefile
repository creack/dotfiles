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

export GOPATH=${HOME}/go
export GOROOT=${HOME}/goroot
export GOBIN=${GOROOT}/bin

all	: $(OBJS) go zsh
	mkdir -p ${HOME}/.emacs.files/tmp
	mkdir -p ${HOME}/.ssh
	touch ${HOME}/.ssh/config
	touch ${HOME}/.ssh/known_hosts

${HOME}/.oh-my-zsh:
	sh -c "$(shell curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

zsh	: ${HOME}/.oh-my-zsh

${HOME}/goroot:
	git clone https://github.com/golang/go ${HOME}/goroot
	cd ${HOME}/goroot/src && git checkout go1.4.2 && ./make.bash

.go	: ${HOME}/goroot
	${GOBIN}/go get -u code.google.com/p/rog-go/exp/cmd/godef
	${GOBIN}/go get -u github.com/jstemmer/gotags
	${GOBIN}/go get -u golang.org/x/tools/cmd/gorename
	${GOBIN}/go get -u golang.org/x/tools/cmd/oracle
	${GOBIN}/go get -u golang.org/x/tools/cmd/goimports
	${GOBIN}/go get -u golang.org/x/tools/cmd/benchcmp
	${GOBIN}/go get -u golang.org/x/tools/cmd/cover
	${GOBIN}/go get -u golang.org/x/tools/cmd/godoc
	${GOBIN}/go get -u golang.org/x/tools/cmd/stringer
	${GOBIN}/go get -u golang.org/x/tools/cmd/vet
	${GOBIN}/go get -u github.com/nsf/gocode
	${GOBIN}/go get -u github.com/golang/lint/golint
	${GOBIN}/go get -u github.com/kisielk/errcheck
	@touch $@
go	: .go

powerline:
	sudo pip install powerline-status

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
