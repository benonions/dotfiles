# Custom functions

# Bulk replace in Go files
bulk-replace() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "Usage: bulk-replace <search> <replace>" >&2
        return 1
    fi
    
    # Check if we're in a git repo
    if git rev-parse --git-dir > /dev/null 2>&1; then
        # Create a backup branch with timestamp
        local backup_branch="backup-bulk-replace-$(date +%s)"
        echo "Creating backup branch: $backup_branch" >&2
        git checkout -b "$backup_branch" 2>/dev/null || echo "Warning: Could not create backup branch" >&2
        git checkout - >/dev/null 2>&1
    fi
    
    # Perform the replacement
    rg --files-with-matches "$1" -g '*.go' | xargs sed -i '' "s/$1/$2/g"
    echo "Replaced '$1' with '$2' in Go files" >&2
}

# Browse bash history
fh() {
  print -z "$(({ [ -n "$ZSH_NAME" ] && fc -l 1 || history; } | fzf +s --tac) | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')"
}

# Generate lowercase UUID
uuid() {
 uuidgen | tr '[:upper:]' '[:lower:]'
}

# Git worktree jump
gwj () {
  local out
  out=$(git worktree list | fzf | awk '{print $1}')
  if [[ -n "$out" ]]; then
    cd "$out"
  fi
}

# Oatmeal sessions
oatmeal-sessions() {
    (
        cd "$(oatmeal sessions dir)"
        id=$(rg --color always -n . | fzf --ansi | awk -F ':' '{print $1}' | head -n1 | awk -F '.' '{print $1}')
        oatmeal sessions open --id "$id"
    )
}

# Zellij tab jump
zjgtt() {
  tab=$(zellij action query-tab-names | fzf)

   if [[ -z "$tab" ]]; then
    echo "No tab selected."
    return 1
  fi

  zellij action go-to-tab-name "$tab"
}

# MySQL connection helper
mysqueel() {
 mysql -h 127.0.0.1 -P 3306 --protocol=TCP -u root -p
}

# Yazi with cwd change
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

# pprof functions
pprof_allocs() {
  curl -sSfL -o "allocs.pprof" http://localhost:6060/debug/pprof/allocs || { echo "Failed to fetch allocs profile" >&2; return 1; }
  go tool pprof -http :8082 ./allocs.pprof
}

pprof_mutex() {
  curl -sSfL -o "mutex.pprof" http://localhost:6060/debug/pprof/mutex || { echo "Failed to fetch mutex profile" >&2; return 1; }
  go tool pprof -http :8082 ./mutex.pprof
}

pprof_blocks() {
  curl -sSfL -o "blocks.pprof" http://localhost:6060/debug/pprof/blocks || { echo "Failed to fetch blocks profile" >&2; return 1; }
  go tool pprof -http :8082 ./blocks.pprof
}

pprof_profile() {
  curl -sSfL -o "profile.pprof" http://localhost:6060/debug/pprof/profile || { echo "Failed to fetch CPU profile" >&2; return 1; }
  go tool pprof -http :8082 ./profile.pprof
}

pprof_heap() {
  curl -sSfL -o "heap.pprof" http://localhost:6060/debug/pprof/heap || { echo "Failed to fetch heap profile" >&2; return 1; }
  go tool pprof -http :8082 ./heap.pprof
}

pprof_goroutine() {
  curl -sSfL -o "goroutine.pprof" http://localhost:6060/debug/pprof/goroutine || { echo "Failed to fetch goroutine profile" >&2; return 1; }
  go tool pprof -http :8082 ./goroutine.pprof
}

pprof_funcs=("pprof_allocs" "pprof_blocks" "pprof_profile" "pprof_heap" "pprof_goroutine" "pprof_mutex")

# pprof menu
ppp() {
  local chosen_func
  chosen_func=$(printf "%s\n" "${pprof_funcs[@]}" | fzf)
  
  # Check if chosen_func is not empty and is a valid function
  if [[ -n "$chosen_func" && " ${pprof_funcs[@]} " =~ " $chosen_func " ]]; then
    "$chosen_func"
  else
    echo "Invalid selection"
  fi
}

# Kubectl patch for profiling
ppatch() {
  if ! kubectl patch deployment tfc-driver-manifold --type='merge' -p '{
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
  }'; then
    echo "Failed to patch deployment tfc-driver-manifold" >&2
    return 1
  fi
}

# Profile ZSH startup time
zsh-profile() {
  time ZSH_DEBUGRC=1 zsh -i -c exit
}

# Clean duplicate PATH entries
clean-path() {
  export PATH=$(echo $PATH | tr ':' '\n' | awk '!x[$0]++' | tr '\n' ':' | sed 's/:$//')
  echo "PATH cleaned. Removed duplicate entries." >&2
}

# Show PATH entries one per line
show-path() {
  echo $PATH | tr ':' '\n' | nl
}