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

# Prompt: dir + git branch/dirty state + jobs + error color
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' %F{green}(%b%u%c)%f'
zstyle ':vcs_info:git:*' actionformats ' %F{yellow}(%b|%a%u%c)%f'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '*'
zstyle ':vcs_info:*' stagedstr '+'
setopt PROMPT_SUBST
PROMPT='%F{blue}%~%f${vcs_info_msg_0_}%(1j. %F{yellow}[%j]%f.)%(?.%F{cyan}.%F{red})$%f '

# Zoxide - smart cd
command -v zoxide >/dev/null 2>&1 && eval "$(command zoxide init zsh)"

# Direnv - environment variables per directory
command -v direnv >/dev/null 2>&1 && eval "$(command direnv hook zsh)"

# Google Cloud SDK path
if [[ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]]; then
  source "$HOME/google-cloud-sdk/path.zsh.inc"
fi

# opam configuration
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null
