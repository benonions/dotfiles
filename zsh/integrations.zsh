# Third-party tool integrations

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

# Zsh autosuggestions
[[ -f "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] && \
  source "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

# The Fuck - command correction
command -v thefuck >/dev/null 2>&1 && eval "$(command thefuck --alias)"

# Starship prompt
command -v starship >/dev/null 2>&1 && eval "$(command starship init zsh)"

# Zoxide - smart cd
command -v zoxide >/dev/null 2>&1 && eval "$(command zoxide init zsh)"

# Direnv - environment variables per directory
command -v direnv >/dev/null 2>&1 && eval "$(command direnv hook zsh)"

# # NVM - Node Version Manager (lazy loaded for performance)
# # To use nvm, just type 'nvm' and it will load on first use
# alias nvm='unalias nvm; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; nvm'
# alias node='unalias node; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; node'
# alias npm='unalias npm; [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"; npm'

# Google Cloud SDK path
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then . "$HOME/google-cloud-sdk/path.zsh.inc"; fi

# opam configuration
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
