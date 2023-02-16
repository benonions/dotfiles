autoload -Uz compinit
compinit
source <(kubectl completion zsh)

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"

  #import aliases
. $DOTFILES/zsh/aliases
  #import pomodoro
. $DOTFILES/zsh/pomodoro

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt

export PATH="${PATH}:${HOME}/.krew/bin"
source /Users/ben/.config/broot/launcher/bash/br
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.rd/bin

