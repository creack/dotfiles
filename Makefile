LN	=	ln -fFs
SRCS	=	dot.emacs	\
		dot.emacs.files	\
		dot.zshrc	\
		dot.oh-my-zsh	\
		dot.zsh_go	\
		dot.zsh_docker	\
		dot.zsh_git_prompt \
		dot.gitconfig	\
		dot.gdbinit	\
		dot.tmux.conf	\
		dot.config
OBJS	=	$(subst dot, ${HOME}/, $(SRCS))

all	: $(OBJS)
	mkdir -p ${HOME}/.emacs.files/tmp
	mkdir -p ${HOME}/.ssh
	touch ${HOME}/.ssh/config
	touch ${HOME}/.ssh/known_hosts

powerline:
	sudo pip install git+git://github.com/Lokaltog/powerline

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
