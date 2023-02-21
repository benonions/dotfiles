{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "ben";
  home.homeDirectory = "/home/ben";

  home.packages = [
		#kubernetes
        pkgs.k9s #a very good boy
        pkgs.kubectl #kube cuddle
        pkgs.krew #package manager for kubectl plugins

        #languages
        pkgs.go #GO!
        pkgs.gopls
        pkgs.delve
        pkgs.nodejs-18_x #node
        pkgs.rustup

        # development tools
        pkgs.lazygit # a nice git TUI
        pkgs.docker
        pkgs.colima
        pkgs.doctl # digital ocean cli
        pkgs.gh #github cli
        pkgs.helix
        pkgs.neovim
        pkgs.alacritty

        # charm.sh tools
        pkgs.gum
        pkgs.glow
        pkgs.skate
        pkgs.soft-serve

        # general tools
        pkgs.tmux
        pkgs.starship #multi-shell customizable prompt
        pkgs.exa #more modern 'ls'
        pkgs.ripgrep #more modern 'grep'
        pkgs.fzf # nifty fuzzy finder
        pkgs.btop #better than top, better than htop
        pkgs.stow # dotfile manager
        pkgs.rclone
        pkgs.broot
        pkgs.zellij

        #fun
        pkgs.thefuck # type 'fuck' after making a typo on a shell command.
        pkgs.spotifyd #spotify daemon, needed for spotify TUI
        pkgs.spotify-tui #run spotify in the terminal, I only use this on Arch, not macOS
        pkgs.element-desktop
        pkgs.lolcat
  ];


  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
