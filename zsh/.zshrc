# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
export LOCALE_ARCHIVE="$(nix-env --installed --no-name --out-path --query glibc-locales)/lib/locale/locale-archive"
# End Nix

autoload -Uz compinit
compinit
source <(kubectl completion zsh)

export DOTFILES="$HOME/.dotfiles"
export EDITOR="lvim"
export SOPS_AGE_KEY_FILE=$HOME/.sops/key.txt

  #import aliases
. $DOTFILES/zsh/aliases.sh

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt
eval "$(zoxide init zsh)"

export PATH="${PATH}:${HOME}/.krew/bin"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.rd/bin
export PATH=$PATH:"$HOME/.local/bin":$PATH
export TERM=screen-256color

##janky function to browse my bash history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

