{
  description = "Ben work laptop";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # emacs-overlay-url = "github:nix-community/emacs-overlay";
    # emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };



  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
    let
      configuration = { pkgs, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget

        system.primaryUser = "ben";
 
      services.emacs = {
         enable = true;
         package = pkgs.emacs;
      };
         

        environment.systemPackages = with pkgs;
          [
          emacs
            vim
            cbonsai
            k9s
            kubectl
            krew
            rustup
            mysql80
            # development tools
            lazygit
            docker
            doctl
            gh
            helix
            neovim
            git-lfs
            elixir
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
            # spotify-tui #run spotify in the terminal, I only use this on Arch, not macOS
            lolcat
            taskwarrior-tui
            postgresql
            hexyl
            nodejs_24

            # Ported from Homebrew
            direnv
            argocd
            autoconf
            cmake
            coreutils
            curl
            diffutils
            # gcc # Keep in Homebrew for emacs-plus
            gnused
            gnutar
            # gnutls # Keep in Homebrew for emacs-plus
            gnugrep
            jq
            libtool
            ninja
            nmap
            openssh
            pkg-config
            tmux
            tree
            # tree-sitter # Keep in Homebrew for emacs-plus
            speedtest-cli
            mu
            qemu
          ];

        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        # Create /etc/zshrc that loads the nix-darwin environment.
        programs.zsh.enable = true; # default shell on catalina
        # programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 5;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";

        system.defaults = {
          # dock 
          dock.autohide = true;
          dock.mru-spaces = false;
          dock.orientation = "bottom";
          # finder 
          finder.AppleShowAllExtensions = true;
          finder.AppleShowAllFiles = true;
          finder.FXPreferredViewStyle = "clmv";
        };

        # HomeBrew = 
        homebrew.enable = true;
        homebrew.onActivation = {
          autoUpdate = true;
          cleanup = "uninstall";
          upgrade = true;
        };

        homebrew.brewPrefix = "/opt/homebrew/bin";

        homebrew.brews =
          [
            # "direnv" # Moved to Nix
            "amp"
            # "argocd" # Moved to Nix
            "argocd-autopilot"
            "argocd-vault-plugin"
            # "autoconf" # Moved to Nix
            "awk" # macOS specific version
            "brotli" # Library dependency
            "ca-certificates" # System dependency
            "cairo" # Library dependency
            "capstone" # Library dependency
            # "cmake" # Moved to Nix
            # "coreutils" # Moved to Nix
            # "curl" # Moved to Nix
            # "diffutils" # Moved to Nix
            "docker" # Keep for Docker Desktop integration
            "docker-completion"
            "doctl" # Already in Nix above
            "dtc" # Library dependency
            "fontconfig" # Library dependency
            "freetype" # Library dependency
            "fribidi" # Library dependency
            "gcc" # Required by emacs-plus@31
            "gdk-pixbuf" # Library dependency
            "gettext" # Library dependency
            "glib" # Library dependency
            "gmp" # Library dependency
            # "gnu-sed" # Moved to Nix (gnused)
            # "gnu-tar" # Moved to Nix (gnutar)
            "gnutls" # Required by emacs-plus@31
            "graphite2" # Library dependency
            # "grep" # Moved to Nix (gnugrep)
            "harfbuzz" # Library dependency
            "icu4c" # Library dependency
            "jansson" # Library dependency
            "jpeg-turbo" # Library dependency
            # "jq" # Moved to Nix
            "ldns" # Library dependency
            "libcbor" # Library dependency
            "libevent" # Library dependency
            "libfido2" # Library dependency
            "libidn2" # Library dependency
            "liblinear" # Library dependency
            "libmpc" # Library dependency
            "libnghttp2" # Library dependency
            "libpng" # Library dependency
            "librsvg" # Library dependency
            "libslirp" # Library dependency
            "libssh" # Library dependency
            "libssh2" # Library dependency
            "libtasn1" # Library dependency
            "libtiff" # Library dependency
            # "libtool" # Moved to Nix
            "libunistring" # Library dependency
            "libusb" # Library dependency
            "libx11" # Library dependency
            "libxau" # Library dependency
            "libxcb" # Library dependency
            "libxdmcp" # Library dependency
            "libxext" # Library dependency
            "libxrender" # Library dependency
            "libyaml" # Library dependency
            "lima" # Keep for VM management
            "little-cms2" # Library dependency
            "lz4" # Library dependency
            "lzo" # Library dependency
            "m4" # Build dependency
            "make" # Build dependency
            "mpdecimal" # Library dependency
            "mpfr" # Library dependency
            "ncurses" # Library dependency
            "nettle" # Library dependency
            # "ninja" # Moved to Nix
            # "nmap" # Moved to Nix
            "oniguruma" # Library dependency
            "openldap" # Library dependency
            # "openssh" # Moved to Nix
            "openssl@3" # System dependency
            "p11-kit" # Library dependency
            "pango" # Library dependency
            "pcre2" # Library dependency
            "pipx" # Python package manager
            "pixman" # Library dependency
            # "pkg-config" # Moved to Nix
            "python-argcomplete" # Python dependency
            "python@3.11" # Keep specific Python versions
            "python@3.12" # Keep specific Python versions
            # "qemu" # Moved to Nix
            "readline" # Library dependency
            "rtmpdump" # Library dependency
            "snappy" # Library dependency
            # "speedtest-cli" # Moved to Nix
            "sqlite" # Library dependency
            "texinfo" # Build dependency
            # "tmux" # Moved to Nix
            # "tree" # Moved to Nix
            "tree-sitter" # Required by emacs-plus@31
            "unbound" # Library dependency
            "unixodbc" # Library dependency
            "utf8proc" # Library dependency
            "vde" # Library dependency
            "wxwidgets" # Library dependency
            "xorgproto" # Library dependency
            "xz" # Library dependency
            "yubikey-agent" # Keep for Yubikey support
            "zstd" # Library dependency
            # "mu" # Moved to Nix
            # "emacs-plus@31" # Keep for special Emacs build
            "acli" # Atlassian CLI
            "container-compose" # Docker compose alternative
          ];

        homebrew.taps = [
          "nikitabobko/tap"
          "d12frosted/emacs-plus"
          "atlassian/homebrew-acli"
        ];

	homebrew.casks =
		[
		"aerospace"
		"flutter"
		"ghostty"
		"raycast"
                "jordanbaird-ice"
		];
      };


    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Benjamins-MacBook-Pro-2
      darwinConfigurations."Benjamins-MacBook-Pro-2" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };

      darwinConfigurations."Bens-BlackBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Benjamins-MacBook-Pro-2".pkgs;

      formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
    };

}
