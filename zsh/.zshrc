# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

autoload -Uz compinit
compinit
source <(kubectl completion zsh)

export DOTFILES="$HOME/.dotfiles"
export EDITOR="lvim"
export SOPS_AGE_KEY_FILE=$HOME/.sops/key.txt

  #import aliases
. $DOTFILES/zsh/aliases.sh
  #import pomodoro
. $DOTFILES/zsh/pomodoro

  #import nnn cd on exit
. $DOTFILES/zsh/nnn.sh

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt
eval "$(zoxide init zsh)"

export PATH="${PATH}:${HOME}/.krew/bin"
source /Users/ben/.config/broot/launcher/bash/br
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$HOME/.rd/bin
export PATH=$PATH:"$HOME/.local/bin":$PATH
export TERM=screen-256color

n ()
{
    # Block nesting of nnn in subshells
    if [[ "${NNNLVL:-0}" -ge 1 ]]; then
        echo "nnn is already running"
        return
    fi

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
    # see. To cd on quit only on ^G, remove the "export" and make sure not to
    # use a custom path, i.e. set NNN_TMPFILE *exactly* as follows:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    # The backslash allows one to alias n to nnn if desired without making an
    # infinitely recursive alias
    \nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

##janky function to browse my bash history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

