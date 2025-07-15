# Aliases

# Import external aliases file if it exists
[[ -f "$DOTFILES/zsh/aliases" ]] && source "$DOTFILES/zsh/aliases"

# General aliases
alias killtmux="tmux kill-session -t \$(tmux list-sessions -F '#S' | fzf)"  
alias glp='git log --pretty=format:"%h - %cn, %cr : %s"'
alias pip="pip3"
alias zsrc="source ~/.zshrc"
alias git_pull_all="ls | xargs -P10 -I{} git -C {} pull"
alias lg="lazygit"

# Testing and coverage
alias gotestall='go test -v -coverprofile cover.out ./...'
alias goshowcoverage='go tool cover -html=cover.out -o cover.html && open cover.html'
alias zf='fd -e pdf | fzf | xargs -r zathura'

# Kubectl shortcuts
alias kgp="kubectl get pods"
alias kgn="kubectl get nodes"
alias kgd="kubectl get deployments"
alias kdp="kubectl describe pod"
alias kdn="kubectl describe node"
alias kdd="kubectl describe deployment"
alias stern="kubectl stern"
alias get-deployment='mkdir -p deploy && kubectl get $(kubectl get deployments -o name | fzf) -o yaml > deploy/deployment.yaml'

# Home Manager
alias hms="home-manager switch"

# Zellij
alias zj="zellij"

# Network
alias addip="sudo ifconfig en0 add 10.10.121.2 255.255.255.0"
alias delip="sudo ifconfig en0 delete 10.10.121.2"

# Docker/nerdctl
alias fixrd="sudo ln -s ~$USER/.rd/docker.sock /var/run/docker.sock"
alias wabbit="nerdctl run -p 5672:5672 -d --hostname wabbit --name wabbit  rabbitmq:3-management"