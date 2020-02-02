USER ?= $(shell whoami)
HOME ?= $(shell [ -d "/Users/${USER}" ] && echo /Users/${USER} || echo /home/${USER})
PWD  ?= $(shell pwd)

ARCH=amd64
ifeq (${HOME},/home/${USER})
OS=linux
else
OS=darwin
endif

LINKS_SRCS    = .editorconfig     \
                .emacs.files      \
                .config           \
                .gitconfig        \
                .gitconfig.perso  \
                .tmux.conf        \
                .zshrc            \
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
install: ${HOME}/.oh-my-zsh
clean:   clean_.oh-my-zsh
${HOME}/.oh-my-zsh:
	@[ -d $@ ] && (cd $@ && git pull) || git clone "https://anonymouse@github.com/robbyrussell/oh-my-zsh" $@
clean_.oh-my-zsh:
	rm -rf ${HOME}/.oh-my-zsh

# Install nvm so it is around when needed.
install: ${HOME}/.nvm
clean:   clean_.nvm
${HOME}/.nvm:
	@[ -d $@ ] && (cd $@ && git pull) || git clone "https://anonymouse@github.com/nvm-sh/nvm" $@
clean_.nvm:
	rm -rf ${HOME}/.nvm

# On OSX, those are installed via brew.
ifeq (${OS},linux)
install: ${HOME}/goroot
clean:   clean_goroot

install: ${HOME}/.local/bin/docker-compose
clean:   clean_docker-compose
endif

# Install go.
${HOME}/goroot: versions/go
	@rm -rf $@ && mkdir $@
	curl -sSL "https://dl.google.com/go/go$(shell cat $<).${OS}-${ARCH}.tar.gz" | tar -xz -P --transform='s|^go|$@|'
	@touch $@
clean_goroot:
	rm -rf ${HOME}/goroot
	@printf "\nTo cleanup go mod's cache, run:\n\n  sudo rm -rf ${HOME}/go/pkg/\n\n" >&2

# Install docker-compose.
${HOME}/.local/bin/docker-compose: versions/docker-compose
	@mkdir -p $(dir $@)
	curl -sSL "https://github.com/docker/compose/releases/download/$(shell cat $<)/docker-compose-$(shell uname -s)-$(shell uname -m)" -o $@
	@chmod +x $@
clean_docker-compose:
	rm -f ${HOME}/.local/bin/docker-compose
	@rmdir ${HOME}/.local/bin ${HOME}/.local 2> /dev/null || true

# Install golangci-lint.
install: ${HOME}/.local/bin/golangci-lint
clean:   clean_golangci-lint
${HOME}/.local/bin/golangci-lint: versions/golangci-lint
	@mkdir -p $(dir $@)
	curl -sfL "https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh" | sh -s -- -b $(dir $@) v$(shell cat $<)
clean_golangci-lint:
	rm -f ${HOME}/.local/bin/golangci-lint
	@rmdir ${HOME}/.local/bin ${HOME}/.local 2> /dev/null || true

# Don't symlink .emacs as it gets rewritten all the time by emacs and only improts files from .emacs.file.
install: ${HOME}/.emacs
clean:   clean_file_.emacs
${HOME}/.emacs: .emacs
	@cp $< $@
clean_file_%:
	rm -f ${HOME}/$*

# Place symlink from home to here.
install: ${LINKS_TARGETS}
${HOME}/%: ${PWD}/%
	ln -f -s $< $@
# Remove the symlinks only if they are still symlink.
clean: ${LINKS_CLEAN}
clean_link_%:
	@[ -L ${HOME}/$* ] && rm ${HOME}/$* || true

# Make sure we have a ~/.ssh dir for linkink ~/.ssh/config
# If ~/.ssh/.ssh/config.private does not exist, copy the template.
${HOME}/.ssh/config: ${PWD}/.ssh/config
	@mkdir -p $(dir $@)
	@[ -f ${HOME}/.ssh/config.private ] || cp ${PWD}/.ssh/config.private ${HOME}/.ssh/config.private
	ln -f -s $< $@
clean_link_.ssh/config:
	@[ -L ${HOME}/.ssh/config ] && rm ${HOME}/.ssh/config || true

# Enable xterm-truecolor support.
install: ${HOME}/.terminfo
clean:   clean_.terminfo
${HOME}/.terminfo: xterm-truecolor.terminfo
	tic -x -o ${HOME}/.terminfo $<
# Remove .terminfo only if xterm-truecolor was the only entry.
clean_.terminfo:
	@rm -rf ${HOME}/.terminfo

# Main targets.
install:
clean:

# Purge removes the common cache folder created by various tools.
purge: clean
	cd ${HOME}; rm -rf ${PURGE_LIST}

# Phony targets.
.PHONY: all install clean purge update
