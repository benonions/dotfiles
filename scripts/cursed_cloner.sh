#!/bin/bash

CACHE_FILE="repo_list.cache"

if [[ "$1" == "-u" ]]; then
  echo "Updating repository list cache..."
  REPO_LIST=$(gh repo list nepworldwide -l go -L 1000 --json name,url | jq -r '.[] | "\(.name) \(.url)"')
  echo "$REPO_LIST" > "$CACHE_FILE"
  echo "Repository list cache updated."
  exit 0
fi

if [[ -e "$CACHE_FILE" ]]; then
  REPO_LIST=$(cat "$CACHE_FILE")
else
  REPO_LIST=$(gh repo list nepworldwide -l go -L 1000 --json name,url | jq -r '.[] | "\(.name) \(.url)"')
  echo "$REPO_LIST" > "$CACHE_FILE"
fi

REPO=$(echo "$REPO_LIST" | fzf --reverse -m)
if [[ -z "$REPO" ]]; then
  echo "No repository selected."
  exit 0
fi

URL=$(echo "$REPO" | awk '{print $2}')
NAME=$(echo "$REPO" | awk '{print $1}')

if [ -e "$NAME" ]; then
  echo "Repository $NAME already exists, skipping..."
  exit 0
fi

gh repo clone "$URL"

