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
	touch ${HOME}/.ssh/config
	touch ${HOME}/.ssh/known_hosts
	curl -L https://github.com/bpinto/oh-my-fish/raw/master/tools/install.sh | sh
	cp config.fish ${HOME}/.config/fish
	cp fish_prompt.fish ${HOME}/.oh-my-fish/themes/zish

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
