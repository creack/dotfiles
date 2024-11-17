# Use 24bit term.
#export TERM=xterm-truecolor

# Set the path for pip/yarn/golang.
export PATH=~/.local/bin:~/go/bin:~/goroot/bin:/usr/local/bin:/usr/local/sbin:~/.yarn/bin:$PATH

# Use most as pager (for things like man, git diff, etc).
export PAGER=most

# Use emacs as default editor.
export EDITOR="emacs -q"

# Enable go mod.
export GO111MODULE=on

# Enable nvm when using vscode.
if [ -n "$VSCODE_IPC_HOOK_CLI" ]; then
  export NVM_DIR="$HOME/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
fi
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi
