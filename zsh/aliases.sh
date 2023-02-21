#reload zshrc
alias zsrc="source ~/.zshrc"

# perform "git pull" for all directories under the CWD
alias git_pull_all="ls | xargs -P10 -I{} git -C {} pull"

# run go test on CWD and open report in html
alias gotestall='go test -v -coverprofile cover.out . && go tool cover -html=cover.out -o cover.html && open cover.html'

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

# exa, a modern replacement for 'ls'
alias ls="exa"

# install/sync nix packages with home-manager
alias hms="home-manager switch"

#zellij
alias zj="zellij"

#nerdctl 
alias wabbit="nerdctl run -p 15672:15672 -d --hostname wabbit --name wabbit  rabbitmq:3-management"