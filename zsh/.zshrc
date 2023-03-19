# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

autoload -Uz compinit
compinit
source <(kubectl completion zsh)

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"

  #import aliases
. $DOTFILES/zsh/aliases.sh
  #import pomodoro
. $DOTFILES/zsh/pomodoro

  #import nnn cd on exit
. $DOTFILES/zsh/nnn.sh

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt

export PATH="${PATH}:${HOME}/.krew/bin"
source /Users/ben/.config/broot/launcher/bash/br
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.rd/bin
export PATH=$PATH:"$HOME/.local/bin":$PATH

