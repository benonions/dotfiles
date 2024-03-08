# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

autoload -Uz compinit
compinit
source <(kubectl completion zsh)

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"
export SOPS_AGE_KEY_FILE="$HOME/.sops/key.txt"

export PATH="$HOME/.local/bin/:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.ansible/scripts/:$PATH"
export PATH="$HOME/.krew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/.rd/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/development/flutter/bin:$PATH"

export JIRA_API_TOKEN=$(ansible-vault view --vault-password-file=~/.ansible/vault_password_file ~/.ansible/auth_codes/jira_token)



# Import aliases
source "$DOTFILES/zsh/aliases"
alias killtmux="tmux kill-session -t \$(tmux list-sessions -F '#S' | fzf)"  
alias glp='git log --pretty=format:"%h - %cn, %cr : %s"'

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt
eval "$(zoxide init zsh)"

export GOPATH="$HOME/go"
export TERM="screen-256color"

if [[ -x "$(command -v zellij)" ]];
then
    eval "$(zellij setup --generate-completion zsh | grep "^function")"
fi;


# Janky function to browse bash history
fh() {
  print -z "$(({ [ -n "$ZSH_NAME" ] && fc -l 1 || history; } | fzf +s --tac) | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')"
}

openconnectnlPass() {
  security find-generic-password -w -a 'Benjamin.Onions' -s 'NL VPN' | pbcopy
}

openconnectnl() {
  openconnect remote.nepworldwide.nl --passwd-on-stdin --protocol=nc --user=Benjamin.Onions
}

whatreposwillibreakifichange() {
  SEARCH_STRING="$1"
  
  if [ -z "$SEARCH_STRING" ]; then
    echo "Usage: blah <SEARCH_STRING>"
    return 1
  fi

  gh search code --owner=nepgpe --language=go "$SEARCH_STRING" -L 1000 --json repository | jq '.[].repository.url' | sort | uniq
}

restart_pulse() {
  sudo launchctl unload -w /Library/LaunchDaemons/net.pulsesecure.AccessService.plist
  sudo launchctl load -w /Library/LaunchDaemons/net.pulsesecure.AccessService.plist
}

pprof_allocs() {
  curl -o "allocs.pprof" http://localhost:6060/debug/pprof/allocs
  go tool pprof -http :8082 ./allocs.pprof
}

pprof_mutex() {
  curl -o "mutex.pprof" http://localhost:6060/debug/pprof/mutex
  go tool pprof -http :8082 ./mutex.pprof
}

pprof_blocks() {
  curl -o "blocks.pprof" http://localhost:6060/debug/pprof/blocks
  go tool pprof -http :8082 ./blocks.pprof
}

pprof_profile() {
  curl -o "profile.pprof" http://localhost:6060/debug/pprof/profile
  go tool pprof -http :8082 ./profile.pprof
}

pprof_heap() {
  curl -o "heap.pprof" http://localhost:6060/debug/pprof/heap
  go tool pprof -http :8082 ./heap.pprof
}

pprof_goroutine() {
  curl -o "goroutine.pprof" http://localhost:6060/debug/pprof/goroutine
  go tool pprof -http :8082 ./goroutine.pprof
}

pprof_funcs=("pprof_allocs" "pprof_blocks" "pprof_profile" "pprof_heap" "pprof_goroutine" "pprof_mutex")

ppp() {
  local chosen_func
  chosen_func=$(printf "%s\n" "${pprof_funcs[@]}" | fzf)
  
  # Check if chosen_func is not empty and is a valid function
  if [[ -n "$chosen_func" && " ${pprof_funcs[@]} " =~ " $chosen_func " ]]; then
    eval "$chosen_func"
  else
    echo "Invalid selection"
  fi
}

zjgtt() {
  tab=$(zellij action query-tab-names | fzf)

   if [[ -z "$tab" ]]; then
    echo "No tab selected."
    return 1
  fi

  zellij action go-to-tab-name "$tab"
}

mysqueel() {
 mysql -h 127.0.0.1 -P 3306 --protocol=TCP -u root -p
}

source "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

autoload -U compinit; compinit

# opam configuration
[[ ! -r /Users/ben/.opam/opam-init/init.zsh ]] || source /Users/ben/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ben/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
