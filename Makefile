USER ?= $(shell whoami)
HOME ?= $(shell [ -d "/Users/${USER}" ] && echo /Users/${USER} || echo /home/${USER})
PWD  ?= $(shell pwd)

LINKS_SRCS    = .editorconfig     \
                .emacs.files      \
                .config           \
                .gitconfig        \
                .gitconfig.perso  \
                .tmux.conf        \
                .zshrc \
                .ssh/config
LINKS_TARGETS = ${LINKS_SRCS:%=${HOME}/%}
LINKS_CLEAN   = ${LINKS_SRCS:%=clean_link_%}

# List of file/dirs to nuke when calling 'make purge'.
PURGE_LIST = .cache .emacs.d .yarn .npm .node-gyp .elinks .apex .terraform.d .parallel \
             .psql_history .python_history .wget-hsts .node_repl_history \
             .yarnrc .zcompdump* .sudo_as_admin_successful \
             ${PWD}/.config/yarn

# Default to install target.
all: install

# Install oh-my-zsh if not installed.
# Use anonymous@ to avoid matching any existing insteadOf url config.
${HOME}/.oh-my-zsh:
	@[ -d $@ ] && (cd $@ && git pull) || git clone https://anonymouse@github.com/robbyrussell/oh-my-zsh $@

# Install nvm so it is around when needed.
${HOME}/.nvm:
	@[ -d $@ ] && (cd $@ && git pull) || git clone https://anonymouse@github.com/nvm-sh/nvm $@

# Don't symlink .emacs as it gets rewritten all the time by emacs and only improts files from .emacs.file.
${HOME}/.emacs: .emacs
	@cp $< $@

# Place symlink from home to here.
${HOME}/%: ${PWD}/%
	ln -f -s $< $@

# Make sure we have a ~/.ssh dir for linkink ~/.ssh/config
# If ~/.ssh/.ssh/config.private does not exist, copy the template.
${HOME}/.ssh/config: ${PWD}/.ssh/config
	@mkdir -p $(dir $@)
	@[ -f ${HOME}/.ssh/config.private ] || cp ${PWD}/.ssh/config.private ${HOME}/.ssh/config.private
	ln -f -s $< $@

# Enable xterm-truecolor support.
${HOME}/.terminfo/x/xterm-truecolor: xterm-truecolor.terminfo
	tic -x -o ${HOME}/.terminfo $<

# Cleanup.
# Not using % to avoid bad things if $* is missing (i.e. rm -rf ${HOME}/).
clean_.oh-my-zsh:
	rm -rf ${HOME}/.oh-my-zsh
clean_.nvm:
	rm -rf ${HOME}/.nvm
clean_file_%: # Safe here to be generic as we don't have '-r'.
	rm -f ${HOME}/$*
# Remove the symlinks only if they are still symlink.
clean_link_%:
	@[ -L ${HOME}/$* ] && rm ${HOME}/$* || true
clean_link_.ssh/config:
	@[ -L ${HOME}/.ssh/config ] && rm ${HOME}/.ssh/config || true
# Remove .terminfo only if xterm-truecolor was the only entry.
clean_.terminfo: clean_file_.terminfo/x/xterm-truecolor
	@mkdir -p ${HOME}/.terminfo/x
	rmdir --ignore-fail-on-non-empty ${HOME}/.terminfo/x ${HOME}/.terminfo
clean_file_.terminfo/x/xterm-truecolor:
	rm -f ${HOME}/.terminfo/x/xterm-truecolor

# Phony targets.
.PHONY: all install clean purge

# Main target for dotfiles.
install: ${HOME}/.oh-my-zsh ${HOME}/.nvm ${HOME}/.emacs ${HOME}/.terminfo/x/xterm-truecolor ${LINKS_TARGETS}

# Main uninstall target.
clean: clean_.oh-my-zsh clean_.nvm clean_file_.emacs clean_.terminfo ${LINKS_CLEAN}

# Purge removes the common cache folder created by various tools.
purge: clean
	cd ${HOME}; rm -rf ${PURGE_LIST}
