# Environment variables and PATH configuration

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"
export SOPS_AGE_KEY_FILE="$HOME/.sops/key.txt"

# Security check for sensitive files
if [[ -f "$SOPS_AGE_KEY_FILE" ]]; then
  if [[ "$(stat -f '%Lp' "$SOPS_AGE_KEY_FILE" 2>/dev/null)" != "600" ]]; then
    echo "WARNING: $SOPS_AGE_KEY_FILE has insecure permissions. Run: chmod 600 $SOPS_AGE_KEY_FILE" >&2
  fi
fi

export ZK_NOTEBOOK_DIR="/Users/ben/Dropbox/MyBrain"
export XDG_CONFIG_HOME="/Users/ben/.config"

# Go environment
export GOPATH="$HOME/go"
export GOROOT=/usr/local/go
export TERM="screen-256color"

# PATH configuration
# Ensure PATH doesn't contain duplicates
typeset -U PATH path

export PATH="$HOME/.local/bin/:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.ansible/scripts/:$PATH"
export PATH="$HOME/.krew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/.rd/bin:$PATH"
export PATH="$HOME/development/flutter/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH=$PATH:/usr/local/go/bin
export PATH=$GOROOT/bin:$PATH

# pnpm
export PNPM_HOME="/Users/ben/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# NVM
export NVM_DIR="$HOME/.nvm"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ben/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)