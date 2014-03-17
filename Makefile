LN	=	ln -fFs
SRCS	=	dot.emacs	\
		dot.emacs.files	\
		dot.zshrc	\
		dot.zsh_go	\
		dot.zsh_docker	\
		dot.zsh_git_prompt \
		dot.gitconfig	\
		dot.gdbinit	\
		dot.tmux.conf
OBJS	=	$(subst dot, ${HOME}/, $(SRCS))

all	: $(OBJS)
	mkdir -p ${HOME}/.emacs.files/tmp
	mkdir -p ${HOME}/.ssh
	mkdir -p ${HOME}/.config/powerline
	touch ${HOME}/.ssh/config
	touch ${HOME}/.ssh/known_hosts
	if [ ! -d ~/.oh-my-fish ]; then curl -L https://github.com/bpinto/oh-my-fish/raw/master/tools/install.sh | sh; fi
	cp config.fish ${HOME}/.config/fish
	cp fish_prompt.fish ${HOME}/.oh-my-fish/themes/zish
	cp -R powerline/ ${HOME}/.config/powerline

powerline:
	sudo pip install git+git://github.com/Lokaltog/powerline

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
