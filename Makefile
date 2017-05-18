HOME           ?= $(shell [ -d "/Users/$$(whoami)" ] && echo /Users/$$(whoami) || echo /home/$$(whoami))
PWD            ?= $(shell pwd)
SRCS            = .Brewfile         \
                  .bcrc             \
                  .dockercloudorg   \
                  .emacs            \
                  .emacs.files      \
                  .config           \
                  .gitconfig        \
                  .gitconfig.perso  \
                  .gpgkey           \
                  .tmux.conf        \
                  .zsh_docker       \
                  .zsh_golang       \
                  .zsh_git_prompt   \
                  .zsh_gitstatus.py \
                  .zshrc
LINKS           = $(addprefix $(HOME)/, $(SRCS))

# Default to install_dotfiles target.
all             : install_dotfiles

# Install oh-my-zsh if not installed.
$(HOME)/.oh-my-zsh:
		@git clone 'https://github.com/robbyrussell/oh-my-zsh' ~/.oh-my-zsh

# One target per link.
$(LINKS)        : $(@F)
		ln -s $(PWD)/$(@F) $@

# Main target for dotfiles.
install_dotfiles: $(HOME)/.oh-my-zsh $(LINKS)

# Uninstall oh-my-zsh.
clean_oh-my-zsh :
		@rm -rf $(HOME)/.oh-my-zsh

# Uninstall dotfiles.
clean_dotfiles  : clean_oh-my-zsh
		@rm -f $(LINKS)

# Main uninstall target.
clean           : clean_dotfiles

# Phony targets.
.PHONY          : all install_dotfiles clean clean_dotfiles clean_oh-my-zsh
