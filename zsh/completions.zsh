# Completions and autoload

# Single compinit with daily cache refresh
autoload -Uz compinit
if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C  # skip security check, use cache
fi

# Enable completion caching for better performance
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Completion options
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Cache kubectl completions (regenerate with: rm ~/.zsh/cache/kubectl-completion.zsh)
if command -v kubectl &>/dev/null; then
  local kubectl_comp="$HOME/.zsh/cache/kubectl-completion.zsh"
  if [[ ! -f "$kubectl_comp" ]]; then
    mkdir -p "$HOME/.zsh/cache"
    kubectl completion zsh > "$kubectl_comp" 2>/dev/null
  fi
  [[ -f "$kubectl_comp" ]] && source "$kubectl_comp"
fi

# Google Cloud SDK completions
if [[ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]]; then
  source "$HOME/google-cloud-sdk/completion.zsh.inc"
fi
