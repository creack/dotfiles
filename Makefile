LN	=	ln -fFs
SRCS	=	dot.emacs	\
		dot.emacs.files	\
		dot.zshrc	\
		dot.zsh_go	\
		dot.gitconfig	\
		dot.tmux.conf

OBJS	=	$(subst dot, ${HOME}/, $(SRCS))

all	: $(OBJS)
	mkdir ${HOME}/.emacs.files/tmp

$(OBJS) :
	 $(LN) $(subst ${HOME}/, ${PWD}/dot, $@) $@

clean	:
	$(RM) $(OBJS)

re	:	clean all
