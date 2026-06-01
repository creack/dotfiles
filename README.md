# dotfiles

Personal dev environment. Works on macOS (Apple Silicon / Intel) and Linux
(Debian / Ubuntu via apt, or any distro with `linuxbrew`).

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
chsh -s "$(command -v zsh)"
```

`make install` installs system packages (brew on macOS, apt on Debian/Ubuntu)
then symlinks the files above. Existing non-symlink files are left alone —
back them up and re-run.

### macOS

Packages from `Brewfile`. Install Homebrew first: <https://brew.sh>.

### Linux

`make install` runs `scripts/install-linux.sh`:

- `apt-get install` of zsh, tmux, emacs-nox, ripgrep, fd-find, fzf,
  zsh-autosuggestions, zsh-syntax-highlighting, most, gnupg, …
- Installs `starship` via the official installer into `~/.local/bin`
  (not in apt).

Not in the script (install manually if you want them):

- `gh` — see <https://cli.github.com/manual/installation>
- Newer Go / Node — apt versions lag; use [go.dev](https://go.dev/dl/),
  [nvm](https://github.com/nvm-sh/nvm), or [fnm](https://github.com/Schniz/fnm).
- `vivid` — for `LS_COLORS`. `cargo install vivid` if you want the gruvbox
  palette; otherwise the shell falls back to default colors.

For non-Debian distros, install the apt list's equivalents yourself, then
run `make links`.

## Targets

- `make install`  — packages + symlinks
- `make links`    — symlinks only
- `make packages` — brew bundle (macOS) or apt install (Linux)
- `make status`   — report which dotfiles are linked / shadowed / missing
- `make clean`    — remove symlinks pointing at this repo

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
