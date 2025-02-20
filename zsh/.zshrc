# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

autoload -Uz compinit
source <(kubectl completion zsh)
source "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"

export DOTFILES="$HOME/.dotfiles"
export EDITOR="nvim"
export SOPS_AGE_KEY_FILE="$HOME/.sops/key.txt"
export ZK_NOTEBOOK_DIR="/Users/ben/Dropbox/MyBrain"
export PATH="$HOME/.local/bin/:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.ansible/scripts/:$PATH"
export PATH="$HOME/.krew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="$HOME/.rd/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/development/flutter/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH=$PATH:/usr/local/go/bin
export XDG_CONFIG_HOME="/Users/ben/.config"
# Import aliases
source "$DOTFILES/zsh/aliases"
alias killtmux="tmux kill-session -t \$(tmux list-sessions -F '#S' | fzf)"  
alias glp='git log --pretty=format:"%h - %cn, %cr : %s"'
alias pip="pip3"

bulk-replace() {
    rg --files-with-matches "$1" -g '*.go' | xargs sed -i '' "s/$1/$2/g"
}

#reload zshrc
alias zsrc="source ~/.zshrc"

# perform "git pull" for all directories under the CWD
alias git_pull_all="ls | xargs -P10 -I{} git -C {} pull"

# run go test on CWD and open report in html
alias gotestall='go test -v -coverprofile cover.out ./...'
alias goshowcoverage='go tool cover -html=cover.out -o cover.html && open cover.html'


# too lazy to type "lazygit"
alias lg="lazygit"

#some kubectl shortcuts
alias kgp="kubectl get pods"
alias kgn="kubectl get nodes"
alias kgd="kubectl get deployments"
alias kdp="kubectl describe pod"
alias kdn="kubectl describe node"
alias kdd="kubectl describe deployment"
alias stern="kubectl stern"

alias get-deployment='mkdir -p deploy && kubectl get $(kubectl get deployments -o name | fzf) -o yaml > deploy/deployment.yaml'
# install/sync nix packages with home-manager
alias hms="home-manager switch"

#zellij
alias zj="zellij"

alias addip="sudo ifconfig en0 add 10.10.121.2 255.255.255.0"
alias delip="sudo ifconfig en0 delete 10.10.121.2"

#nerdctl 
alias fixrd="sudo ln -s ~$USER/.rd/docker.sock /var/run/docker.sock"
alias wabbit="nerdctl run -p 5672:5672 -d --hostname wabbit --name wabbit  rabbitmq:3-management"

eval "$(thefuck --alias)"   # Magnificent app which corrects your previous console command.
eval "$(starship init zsh)" # using starship for prompt
eval "$(zoxide init zsh)"

export GOPATH="$HOME/go"
export TERM="screen-256color"


# Janky function to browse bash history
fh() {
  print -z "$(({ [ -n "$ZSH_NAME" ] && fc -l 1 || history; } | fzf +s --tac) | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')"
}

uuid() {
 uuidgen | tr '[:upper:]' '[:lower:]'
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

gwj () {
  local out
  out=$(git worktree list | fzf | awk '{print $1}')
  cd $out
}

oatmeal-sessions() {
    (
        cd "$(oatmeal sessions dir)"
        id=$(rg --color always -n . | fzf --ansi | awk -F ':' '{print $1}' | head -n1 | awk -F '.' '{print $1}')
        oatmeal sessions open --id "$id"
    )
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

ppatch() {
kubectl patch deployment tfc-routestate-graph --type='merge' -p '{
  "spec": {
    "template": {
      "metadata": {
        "annotations": {
          "profiles.grafana.com/cpu.port": "6060",
          "profiles.grafana.com/cpu.scrape": "true",
          "profiles.grafana.com/goroutine.port": "6060",
          "profiles.grafana.com/goroutine.scrape": "true",
          "profiles.grafana.com/memory.port": "6060",
          "profiles.grafana.com/memory.scrape": "true",
          "profiles.grafana.com/mutex.port": "6060",
          "profiles.grafana.com/mutex.scrape": "true",
          "profiles.grafana.com/block.port": "6060",
          "profiles.grafana.com/block.scrape": "true",
          "profiles.grafana.com/trace.scrape": "true",
          "profiles.grafana.com/trace.port": "6060"
        }
        }
      }
    }
  }'
}

getCiscoPass() {
security find-generic-password -s cisco_switch  -w
}

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}


# opam configuration
[[ ! -r /Users/ben/.opam/opam-init/init.zsh ]] || source /Users/ben/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ben/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# pnpm
export PNPM_HOME="/Users/ben/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/ben/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/ben/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/ben/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/ben/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
