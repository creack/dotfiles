SHELL=zsh
PWD=$(shell pwd)

all: install

~/.oh-my-zsh:
	@git clone 'https://github.com/robbyrussell/oh-my-zsh' ~/.oh-my-zsh

install: ~/.oh-my-zsh
	@for f in $(shell ls -a); do \
	  [ "$$f" = "." ] || [ "$$f" = ".." ] || [ "$$f" = ".git" ] || [ "$$f" = ".gitignore" ] && continue; \
          [[ "$$f" != "."* ]] && continue; \
	  [ -e "$(PWD)/$$f" ] || ln -s $(PWD)/$$f ~/$$f; \
	done

clean:
	@rm -rf ~/.oh-my-zsh
	@for f in $(shell ls -a); do \
	  [ "$$f" = "." ] || [ "$$f" = ".." ] || [ "$$f" = ".git" ] || [ "$$f" = ".gitignore" ] && continue; \
          [[ "$$f" != "."* ]] && continue; \
	  [ -h "$$HOME/$$f" ] && rm "$$HOME/$$f"; \
	done

.PHONY: install clean
