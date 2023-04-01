#!/bin/bash

# # Validate script arguments
# if [ "$#" -ne 2 ]; then
#     echo "Usage: $0 <log_file> <repo_name>"
#     exit 1
# fi

log_file="$1"
# repo_name="$2"

# Initialize the log file selection loop
while true; do
    selected_line="$(sed 's/INF/\x1b[32m&\x1b[0m/g; s/DBG/\x1b[34m&\x1b[0m/g; s/ERR/\x1b[31m&\x1b[0m/g' "$log_file" | bat -l log -p | fzf --ansi)"

    # Exit the loop if the user selects the "quit" option
    if [[ -z $selected_line || "$selected_line" == "quit" ]]; then
        break
    fi

    # Parse the file path and line number from the selected log line
    file_path=$(echo "$selected_line" | grep -o '\S*\.go:[0-9]*' | cut -d ':' -f 1)
    line_number=$(echo "$selected_line" | grep -o '\S*\.go:[0-9]*' | cut -d ':' -f 2)

    # Open lvim in a new pane and jump to the selected line
    # tmux split-window -h "cd ~/code/nep/$repo_name && lvim +edit +$line_number $file_path"
done

