# Modular ZSH configuration
# Load all configuration modules from the dotfiles directory

DOTFILES_ZSH_DIR="$HOME/.dotfiles/zsh"

for config in environment aliases functions completions integrations work; do
  [[ -f "$DOTFILES_ZSH_DIR/$config.zsh" ]] && source "$DOTFILES_ZSH_DIR/$config.zsh"
done

# Load local configuration (not tracked in git)
[[ -f "$DOTFILES_ZSH_DIR/local.zsh" ]] && source "$DOTFILES_ZSH_DIR/local.zsh"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ben/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# AsyncAPI CLI Autocomplete

ASYNCAPI_AC_ZSH_SETUP_PATH=/Users/ben/Library/Caches/@asyncapi/cli/autocomplete/zsh_setup && test -f $ASYNCAPI_AC_ZSH_SETUP_PATH && source $ASYNCAPI_AC_ZSH_SETUP_PATH; # asyncapi autocomplete setup
