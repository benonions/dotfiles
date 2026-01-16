# Environment variables and PATH configuration

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"
export SOPS_AGE_KEY_FILE="$HOME/.sops/key.txt"

# Security check for sensitive files
if [[ -f "$SOPS_AGE_KEY_FILE" ]]; then
  # Use BSD stat on macOS, GNU stat elsewhere
  if [[ "$OSTYPE" == darwin* ]]; then
    perms=$(/usr/bin/stat -f '%Lp' "$SOPS_AGE_KEY_FILE" 2>/dev/null)
  else
    perms=$(stat -c '%a' "$SOPS_AGE_KEY_FILE" 2>/dev/null)
  fi
  if [[ "$perms" != "600" ]]; then
    echo "WARNING: $SOPS_AGE_KEY_FILE has insecure permissions. Run: chmod 600 $SOPS_AGE_KEY_FILE" >&2
  fi
fi

export ZK_NOTEBOOK_DIR="$HOME/Dropbox/MyBrain"
export XDG_CONFIG_HOME="$HOME/.config"

# Go environment
export GOPATH="$HOME/go"
export GOROOT=/usr/local/go
export TERM="screen-256color"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"

# NVM
export NVM_DIR="$HOME/.nvm"

# PATH configuration
typeset -U PATH path

path=(
  $HOME/.local/bin
  /usr/local/bin
  $HOME/.ansible/scripts
  $HOME/.krew/bin
  $GOPATH/bin
  $GOROOT/bin
  $HOME/.rd/bin
  $HOME/development/flutter/bin
  $HOME/.emacs.d/bin
  $HOME/.asdf/shims
  $PNPM_HOME
  $path
)
