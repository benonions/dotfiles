# Modular ZSH configuration
# Load all configuration modules from the dotfiles directory

DOTFILES_ZSH_DIR="$HOME/.dotfiles/zsh"

for config in environment aliases functions completions integrations work; do
  [[ -f "$DOTFILES_ZSH_DIR/$config.zsh" ]] && source "$DOTFILES_ZSH_DIR/$config.zsh"
done

# Load local configuration (not tracked in git)
[[ -f "$DOTFILES_ZSH_DIR/local.zsh" ]] && source "$DOTFILES_ZSH_DIR/local.zsh"