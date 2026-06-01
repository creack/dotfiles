.DEFAULT_GOAL := install

HOME    ?= $(shell echo $$HOME)
PWD     ?= $(shell pwd)
UNAME_S := $(shell uname -s)

# Files symlinked directly into $HOME.
HOME_LINKS = \
	.zshrc \
	.zshenv \
	.zprofile \
	.tmux.conf \
	.gitconfig \
	.gitignore.global \
	.editorconfig

# Files symlinked into $HOME/.config/.
CONFIG_LINKS = starship.toml

# Directories symlinked into $HOME (entire tree).
DIR_LINKS = .emacs.d

# -----------------------------------------------------------------------------
# Top-level targets
# -----------------------------------------------------------------------------

.PHONY: install
install: packages links ## Install system packages and symlink dotfiles.

.PHONY: links
links: $(addprefix $(HOME)/, $(HOME_LINKS)) \
       $(addprefix $(HOME)/.config/, $(CONFIG_LINKS)) \
       $(addprefix $(HOME)/, $(DIR_LINKS))

.PHONY: packages
packages: ## Install system packages (brew on macOS, apt on Debian/Ubuntu).
ifeq ($(UNAME_S),Darwin)
	@command -v brew >/dev/null || { \
	  echo "Homebrew not installed. Install from https://brew.sh"; exit 1; }
	brew bundle --file=$(PWD)/Brewfile
else
	$(PWD)/scripts/install-linux.sh
endif

.PHONY: brew
brew: packages ## Alias for `packages` (legacy).

.PHONY: clean
clean: ## Remove symlinks pointing to this repo.
	@for f in $(HOME_LINKS) $(DIR_LINKS); do \
	  target="$(HOME)/$$f"; \
	  if [ -L "$$target" ]; then \
	    src="$$(readlink "$$target")"; \
	    case "$$src" in $(PWD)/*) rm "$$target" && echo "rm $$target" ;; esac; \
	  fi; \
	done
	@for f in $(CONFIG_LINKS); do \
	  target="$(HOME)/.config/$$f"; \
	  if [ -L "$$target" ]; then \
	    src="$$(readlink "$$target")"; \
	    case "$$src" in $(PWD)/*) rm "$$target" && echo "rm $$target" ;; esac; \
	  fi; \
	done

.PHONY: status
status: ## Show which dotfiles are linked, missing, or shadowed by a real file.
	@for f in $(HOME_LINKS) $(DIR_LINKS); do \
	  target="$(HOME)/$$f"; src="$(PWD)/$$f"; \
	  if [ -L "$$target" ] && [ "$$(readlink "$$target")" = "$$src" ]; then \
	    printf "  \033[32mOK\033[0m   %s\n" "$$f"; \
	  elif [ -e "$$target" ]; then \
	    printf "  \033[31mFILE\033[0m %s (not a symlink to this repo)\n" "$$f"; \
	  else \
	    printf "  \033[33m--\033[0m   %s (missing)\n" "$$f"; \
	  fi; \
	done
	@for f in $(CONFIG_LINKS); do \
	  target="$(HOME)/.config/$$f"; src="$(PWD)/.config/$$f"; \
	  if [ -L "$$target" ] && [ "$$(readlink "$$target")" = "$$src" ]; then \
	    printf "  \033[32mOK\033[0m   .config/%s\n" "$$f"; \
	  elif [ -e "$$target" ]; then \
	    printf "  \033[31mFILE\033[0m .config/%s (not a symlink)\n" "$$f"; \
	  else \
	    printf "  \033[33m--\033[0m   .config/%s (missing)\n" "$$f"; \
	  fi; \
	done

.PHONY: help
help: ## Show this help.
	@awk 'BEGIN {FS = ":.*##"} /^[a-zA-Z_-]+:.*##/ {printf "  \033[36m%-12s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# -----------------------------------------------------------------------------
# Symlink rules — refuse to clobber existing files; only overwrite stale links.
# -----------------------------------------------------------------------------

$(HOME)/.config/%: $(PWD)/.config/%
	@mkdir -p $(dir $@)
	@if [ -e "$@" ] && [ ! -L "$@" ]; then \
	  echo "  \033[31mskip\033[0m $@ (exists, not a symlink — back up and remove first)"; \
	else \
	  ln -sfn $< $@ && echo "  link $@ -> $<"; \
	fi

$(HOME)/%: $(PWD)/%
	@if [ -e "$@" ] && [ ! -L "$@" ]; then \
	  echo "  \033[31mskip\033[0m $@ (exists, not a symlink — back up and remove first)"; \
	else \
	  ln -sfn $< $@ && echo "  link $@ -> $<"; \
	fi
