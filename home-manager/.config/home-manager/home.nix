{ config, pkgs, ... }: {

  home =
    {
      username = "ben";
      homeDirectory = "/home/ben";
      stateVersion = "22.05";

      packages = with pkgs; [
        #kubernetes
        k9s
        kubectl
        pkgs.krew

        #languages
        gopls
        delve
        nodejs-18_x
        rustup
        go

        # development tools
        lazygit
        docker
        doctl
        gh
        helix

        # charm.sh tools
        gum
        glow
        skate
        soft-serve

        # general tools
        starship
        exa
        ripgrep
        fzf
        btop
        stow
        rclone
        ranger
        zoxide
        ansible
        fd

        #fun
        thefuck # type 'fuck' after making a typo on a shell command.
        spotifyd #spotify daemon, needed for spotify TUI
        spotify-tui #run spotify in the terminal, I only use this on Arch, not macOS
        lolcat
      ];
    };

  # programs that we will manage the configuration for
  programs = {
    home-manager.enable = true;

    # bat, a cat clone with wings
    bat = {
      enable = true;
      config = {
        theme = "Dracula";
        italic-text = "always";
      };
    };

  };



}

