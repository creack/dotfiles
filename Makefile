USER ?= $(shell whoami)
HOME ?= $(shell [ -d "/Users/${USER}" ] && echo /Users/${USER} || echo /home/${USER})
PWD  ?= $(shell pwd)
RM   ?= rm -f

ARCH=amd64
ifeq (${HOME},/home/${USER})
OS=linux
else
OS=darwin
endif

LINKS_SRCS    = .editorconfig       \
                .emacs.files        \
                .emacs              \
                .config             \
                .gitconfig          \
                .gitconfig.creack   \
                .gitconfig.zk       \
                .gitconfig.immertec \
                .tmux.conf          \
                .zshrc              \
                .zshenv             \
                .Xresources         \
                .aspell.en.pws      \
                .aspell.en.prepl    \
                .ssh/config         \
                .fluxbox/keys
LINKS_TARGETS = ${LINKS_SRCS:%=${HOME}/%}
LINKS_CLEAN   = ${LINKS_SRCS:%=clean_link_%}

# List of file/dirs to nuke when calling 'make purge'.
PURGE_LIST = .cache .emacs.d .yarn .npm .node-gyp .elinks .apex .terraform.d .parallel \
             .psql_history .python_history .wget-hsts .node_repl_history \
             .yarnrc .zcompdump* .sudo_as_admin_successful .xsession-errors .lesshst \
             .config/yarn .texlive* .java .refresh

# Default to install target.
all: install

# Default the git profile to the .creack one.
install: ${HOME}/.gitconfig.local
${HOME}/.gitconfig.local: ${PWD}/.gitconfig.creack
	ln -f -s $< $@
clean: clean_link_.gitconfig.local

# Install oh-my-zsh if not installed.
# Use anonymous@ to avoid matching any existing insteadOf url config.
# TODO: Migrate to antigen or alike for cleaner zsh plugin management.
install: ${HOME}/.oh-my-zsh/oh-my-zsh.sh
install: ${HOME}/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
install: ${HOME}/.oh-my-zsh/custom/plugins/zsh-completions/zsh-completions.plugin.zsh
install: ${HOME}/.oh-my-zsh/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
clean:   clean_.oh-my-zsh
${HOME}/.oh-my-zsh/oh-my-zsh.sh:
	@[ -d $(dir $@) ] && (cd $(dir $@) && git pull) || git clone "https://anonymouse@github.com/robbyrussell/oh-my-zsh" $(dir $@)
${HOME}/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh: ${HOME}/.oh-my-zsh/oh-my-zsh.sh
	@[ -d $(dir $@) ] && (cd $(dir $@) && git pull) || git clone "https://github.com/zsh-users/zsh-syntax-highlighting.git" $(dir $@)
	@touch $@
${HOME}/.oh-my-zsh/custom/plugins/zsh-completions/zsh-completions.plugin.zsh: ${HOME}/.oh-my-zsh/oh-my-zsh.sh
	@[ -d $(dir $@) ] && (cd $(dir $@) && git pull) || git clone https://github.com/zsh-users/zsh-completions $(dir $@)
	@touch $@
${HOME}/.oh-my-zsh/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh: ${HOME}/.oh-my-zsh/oh-my-zsh.sh
	@[ -d $(dir $@) ] && (cd $(dir $@) && git pull) || git clone https://github.com/zsh-users/zsh-autosuggestions $(dir $@)
	@touch $@
clean_.oh-my-zsh:
	${RM} -r ${HOME}/.oh-my-zsh

# Install tpm (tmux plugin manager)
install: ${HOME}/.tmux/plugins/tpm/tpm
${HOME}/.tmux/plugins/tpm/tpm:
	@[ -d $(dir $@) ] && (cd $(dir $@) && git pull) || git clone https://github.com/tmux-plugins/tpm $(dir $@)
clean: clean_tpm
clean_tpm:
	${RM} -r ${HOME}/.tmux/plugins/tpm

# Install nvm so it is around when needed.
install: ${HOME}/.nvm
clean:   clean_.nvm
${HOME}/.nvm:
	@[ -d $@ ] && (cd $@ && git pull) || git clone "https://anonymouse@github.com/nvm-sh/nvm" $@
clean_.nvm:
	${RM} -r ${HOME}/.nvm

# On OSX, those are installed via brew.
ifeq (${OS},linux)
install: ${HOME}/goroot
clean:   clean_goroot

install: ${HOME}/.local/bin/docker-compose
clean:   clean_docker-compose
endif

# Install go.
${HOME}/goroot: versions/go
	@${RM} -r $@ && mkdir $@
	curl -sSL "https://dl.google.com/go/go$(shell cat $<).${OS}-${ARCH}.tar.gz" | tar -xz -P --transform='s|^go|$@|'
	@touch $@
clean_goroot:
	${RM} -r ${HOME}/goroot
	@printf "\nTo cleanup go mod's cache, run:\n\n  sudo rm -rf ${HOME}/go/pkg/\n\n" >&2

# Install docker-compose.
${HOME}/.local/bin/docker-compose: versions/docker-compose
	@mkdir -p $(dir $@)
	curl -sSL "https://github.com/docker/compose/releases/download/$(shell cat $<)/docker-compose-$(shell uname -s)-$(shell uname -m)" -o $@
	@chmod +x $@
clean_docker-compose:
	${RM} ${HOME}/.local/bin/docker-compose
	@rmdir ${HOME}/.local/bin ${HOME}/.local 2> /dev/null || true

# Install golangci-lint.
install: ${HOME}/.local/bin/golangci-lint
clean:   clean_golangci-lint
${HOME}/.local/bin/golangci-lint: versions/golangci-lint
	@mkdir -p $(dir $@)
	curl -sfL "https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh" | sh -s -- -b $(dir $@) v$(shell cat $<)
clean_golangci-lint:
	${RM} ${HOME}/.local/bin/golangci-lint
	@rmdir ${HOME}/.local/bin ${HOME}/.local 2> /dev/null || true

clean_file_%:
	${RM} ${HOME}/$*

# Place symlink from home to here.
install: ${LINKS_TARGETS}
${HOME}/%: ${PWD}/%
	ln -f -s $< $@
# Remove the symlinks only if they are still symlink.
clean: ${LINKS_CLEAN}
clean_link_%:
	@[ -L ${HOME}/$* ] && ${RM} ${HOME}/$* || true

# Make sure we have a ~/.ssh dir for linkink ~/.ssh/config
# If ~/.ssh/.ssh/config.private does not exist, copy the template.
${HOME}/.ssh/config: ${PWD}/.ssh/config
	@mkdir -p $(dir $@)
	@[ -f ${HOME}/.ssh/config.private ] || cp ${PWD}/.ssh/config.private ${HOME}/.ssh/config.private
	ln -f -s $< $@
clean_link_.ssh/config:
	@[ -L ${HOME}/.ssh/config ] && ${RM} ${HOME}/.ssh/config || true

${HOME}/.fluxbox/keys: ${PWD}/.fluxbox/keys
	@mkdir -p $(dir $@)
	ln -f -s $< $@
clean_link_.fluxbox/keys:
	@[ -L ${HOME}/.fluxbox/keys ] && ${RM} ${HOME}/.fluxbox/keys || true


# Enable xterm-truecolor support.
install: ${HOME}/.terminfo
clean:   clean_.terminfo
${HOME}/.terminfo: xterm-truecolor.terminfo
	tic -x -o ${HOME}/.terminfo $<
# Remove .terminfo only if xterm-truecolor was the only entry.
clean_.terminfo:
	@${RM} -r ${HOME}/.terminfo

# Main targets.
install:
clean:

# Purge removes the common cache folder created by various tools.
purge: clean
	cd ${HOME}; ${RM} -r ${PURGE_LIST}

# Phony targets.
.PHONY: all install clean purge update
