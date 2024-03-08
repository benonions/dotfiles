{ config, pkgs, ... }: {

  home =
    {
      username = "ben";
      homeDirectory = "/Users/ben";
      stateVersion = "22.05";

      packages = with pkgs; [
        #kubernetes
        k9s
        kubectl
        pkgs.krew

        #languages
        # gopls
        # delve
        rustup
        # go
        tmux
        elixir
        mysql80
        # development tools
        lazygit
        docker
        doctl
        gh
        helix
        git-lfs

        # charm.sh tools
        gum
        glow
        skate
        soft-serve

        # general tools
        starship
        ripgrep
        fzf
        btop
        stow
        ranger
        zoxide
        ansible
        fd
        zellij
        eza
        lf
        nnn
        parallel

        #fun
        thefuck # type 'fuck' after making a typo on a shell command.
        spotifyd #spotify daemon, needed for spotify TUI
        spotify-tui #run spotify in the terminal, I only use this on Arch, not macOS
        lolcat
        taskwarrior-tui
        postgresql
        hexyl
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

