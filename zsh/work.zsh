# Work-specific functions

# OpenConnect VPN helpers
openconnectnlPass() {
  security find-generic-password -w -a 'Benjamin.Onions' -s 'NL VPN' | pbcopy
  echo "Password copied to clipboard (will clear in 45 seconds)" >&2
  ( sleep 45 && echo -n | pbcopy && echo "Clipboard cleared" >&2 ) &
}

openconnectnl() {
  openconnect remote.nepworldwide.nl --passwd-on-stdin --protocol=nc --user=Benjamin.Onions
}

# GitHub repository search
whatreposwillibreakifichange() {
  SEARCH_STRING="$1"
  
  if [ -z "$SEARCH_STRING" ]; then
    echo "Usage: blah <SEARCH_STRING>"
    return 1
  fi

  gh search code --owner=nepgpe --language=go "$SEARCH_STRING" -L 1000 --json repository | jq '.[].repository.url' | sort | uniq
}

# Pulse Secure restart
restart_pulse() {
  sudo launchctl unload -w /Library/LaunchDaemons/net.pulsesecure.AccessService.plist
  sudo launchctl load -w /Library/LaunchDaemons/net.pulsesecure.AccessService.plist
}

# Cisco switch password helper
getCiscoPass() {
  security find-generic-password -s cisco_switch -w | pbcopy
  echo "Password copied to clipboard (will clear in 45 seconds)" >&2
  ( sleep 45 && echo -n | pbcopy && echo "Clipboard cleared" >&2 ) &
}