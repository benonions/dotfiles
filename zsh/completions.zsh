# Completions and autoload

autoload -Uz compinit
compinit

# Enable completion caching for better performance
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Completion options
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Kubectl completion
source <(kubectl completion zsh)

# NVM completions
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

# Google Cloud SDK completions
if [ -f '/Users/ben/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/ben/google-cloud-sdk/completion.zsh.inc'; fi