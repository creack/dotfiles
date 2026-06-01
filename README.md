# dotfiles

Personal dev environment. macOS (Apple Silicon) primary, Linux secondary.

## Layout

| File / dir         | Symlinked to               | Purpose                                    |
| ------------------ | -------------------------- | ------------------------------------------ |
| `.zshrc`           | `~/.zshrc`                 | Interactive shell                          |
| `.zshenv`          | `~/.zshenv`                | Always-loaded shell env (XDG, PATH)        |
| `.zprofile`        | `~/.zprofile`              | Login shell (brew shellenv)                |
| `.tmux.conf`       | `~/.tmux.conf`             | tmux                                       |
| `.gitconfig`       | `~/.gitconfig`             | git (single identity)                      |
| `.gitignore.global`| `~/.gitignore.global`      | Global git excludes                        |
| `.editorconfig`    | `~/.editorconfig`          | Indent / EOL rules                         |
| `.config/starship.toml` | `~/.config/starship.toml` | Prompt                              |
| `.emacs.d/`        | `~/.emacs.d/`              | Modern use-package / straight.el config    |
| `Brewfile`         | —                          | Packages installed via `brew bundle`       |
| `Makefile`         | —                          | `install` / `clean` / `status`             |

## Install

```sh
git clone git@github.com:creack/dotfiles ~/projects/dotfiles
cd ~/projects/dotfiles
make install
chsh -s "$(brew --prefix)/bin/zsh"
```

`make install` runs `brew bundle` then symlinks the files above. Existing
non-symlink files are left alone — back them up and re-run.

## Targets

- `make install` — brew bundle + symlinks
- `make links`   — symlinks only
- `make brew`    — `brew bundle` only
- `make status`  — report which dotfiles are linked / shadowed / missing
- `make clean`   — remove symlinks pointing at this repo

## Local overrides

Untracked, per-host:

- `~/.zshrc.local`   — sourced at end of `.zshrc`
- `~/.gitconfig.local` — included by `~/.gitconfig` (signing key, alt identity)

## Prompt

[Starship](https://starship.rs). Config in `.config/starship.toml`.

## Editor

Emacs daemon. `emacs` is aliased to `emacsclient -a '' -c -t` (start if not
running, attach in terminal). First launch will bootstrap straight.el and
fetch all packages — give it a minute.

## SSH key import

```sh
github_user=creack
curl -fsSL "https://github.com/$github_user.keys" >> ~/.ssh/authorized_keys
curl -fsSL "https://github.com/$github_user.gpg"  | gpg --import
```
