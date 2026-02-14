# Modular ZSH configuration
# Load all configuration modules from the dotfiles directory

DOTFILES_ZSH_DIR="$HOME/.dotfiles/zsh"

for config in environment aliases functions completions integrations; do
  [[ -f "$DOTFILES_ZSH_DIR/$config.zsh" ]] && source "$DOTFILES_ZSH_DIR/$config.zsh"
done

# Decrypt and source work config (SOPS encrypted)
if [[ -f "$DOTFILES_ZSH_DIR/work.zsh" ]] && command -v sops &>/dev/null; then
  source <(sops -d "$DOTFILES_ZSH_DIR/work.zsh" 2>/dev/null)
fi

# Load local configuration (not tracked in git)
[[ -f "$DOTFILES_ZSH_DIR/local.zsh" ]] && source "$DOTFILES_ZSH_DIR/local.zsh"

export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/opt/local/bin:$PATH"

# NVM - lazy loaded for performance
export NVM_DIR="$HOME/.config/nvm"
if [[ -s "$NVM_DIR/nvm.sh" ]]; then
  alias nvm='unalias nvm node npm; source "$NVM_DIR/nvm.sh"; nvm'
  alias node='unalias nvm node npm; source "$NVM_DIR/nvm.sh"; node'
  alias npm='unalias nvm node npm; source "$NVM_DIR/nvm.sh"; npm'
fi

# opencode
export PATH=/Users/ben/.opencode/bin:$PATH
export GPG_TTY=$(tty)

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ben/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
