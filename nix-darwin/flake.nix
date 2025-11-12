{
  description = "Ben work laptop";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
    let
      configuration = { pkgs, config, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget

        system.primaryUser = "ben";

        # ========================================
        # HOME-MANAGER CONFIGURATION
        # ========================================
        # Uncomment the following to enable home-manager
        users.users.ben = {
          name = "ben";
          home = "/Users/ben";
        };

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.ben = { pkgs, ... }: {
          # Home Manager configuration
          home.stateVersion = "25.05";

          # User-specific packages
          home.packages = with pkgs; [
            # Text editors
            vim
            helix
            neovim

            # Development tools
            nixd # Nix language server
            nixfmt
            nil # Alternative Nix language server
            lazygit
            gh
            git-lfs
            rustup
            elixir
            nodejs
            python312 # Use Python 3.12 as primary
            # python311  # Commented out to avoid collision
            pipx
            direnv
            starship
            wget
            # Charm.sh tools
            gum
            glow
            skate
            soft-serve
            pandoc

            # Terminal utilities
            ripgrep
            fzf
            btop
            ranger
            zoxide
            fd
            zellij
            eza
            lf
            nnn
            hexyl
            parallel
            stow
            tree
            bat
            lnav

            # Container/Cloud tools
            k9s
            kubectl
            krew
            doctl
            argocd
            argocd-vault-plugin

            # Database tools
            mysql80
            postgresql
            sqlite

            # Fun stuff
            thefuck
            spotifyd
            lolcat
            taskwarrior-tui
            cbonsai

            # Security
            yubikey-agent

            # Virtualization
            lima
            qemu

            # Network tools
            speedtest-cli
            nmap

            # Email
            mu
            davmail
          ];

          # Git configuration
          # programs.git = {
          #   enable = true;
          #   lfs.enable = true;
          #   # userName = "Your Name";
          #   # userEmail = "your.email@example.com";
          #   extraConfig = {
          #     init.defaultBranch = "main";
          #     pull.rebase = true;
          #     push.autoSetupRemote = true;
          #   };
          # };

          # Shell program configs commented out - user manages their own shell
          # programs.starship = {
          #   enable = true;
          #   enableZshIntegration = true;
          # };

          programs.direnv = {
            enable = true;
            enableZshIntegration = true;
            nix-direnv.enable = true;
          };

          # programs.fzf = {
          #   enable = true;
          #   enableZshIntegration = true;
          # };

        };

        # ========================================
        # SERVICES
        # ========================================
        services.emacs = {
          enable = true;
          package = pkgs.emacs;
        };

        # Additional services
        # services.nix-daemon.enable = true;
        # services.tailscale.enable = true;  # VPN service

        environment.systemPackages = with pkgs; [
          # Core system packages
          emacs
          docker
          tmux
          ansible

          # Shell & system tools
          coreutils
          gnused
          gnutar
          gnugrep
          gawk # GNU AWK
          gnumake # GNU Make
          jq
          curl
          openssh

          # Build tools
          autoconf
          cmake
          libtool
          m4 # Macro processor
          ninja
          pkg-config
          texinfo # Documentation system
          diffutils

          # Security & certificates
          cacert # CA certificates
          openssl # OpenSSL tools
          libfido2 # FIDO2/WebAuthn tools

          # Compression utilities
          brotli # Compression algorithm
          lz4 # Fast compression
          xz # High-ratio compression
          zstd # Fast compression with good ratio

          # Network tools (system-level)
          unbound # DNS resolver
          openldap # LDAP client tools
        ];

        # ========================================
        # NIX CONFIGURATION
        # ========================================
        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        # Additional Nix settings
        # nix.settings = {
        #   experimental-features = ["nix-command" "flakes"];
        #   sandbox = true;
        #   auto-optimise-store = true;
        #   trusted-users = ["ben"];
        #   # Build settings
        #   max-jobs = 8;  # Adjust based on your CPU
        #   cores = 0;  # Use all available cores
        #   # Substituters (binary caches)
        #   substituters = [
        #     "https://cache.nixos.org"
        #     "https://nix-community.cachix.org"
        #   ];
        #   trusted-public-keys = [
        #     "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        #     "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        #   ];
        # };

        # Garbage collection
        # nix.gc = {
        #   automatic = true;
        #   interval = { Hour = 3; Minute = 0; };  # Run at 3am
        #   options = "--delete-older-than 7d";
        # };

        # ========================================
        # SHELL CONFIGURATION
        # ========================================
        # Create /etc/zshrc that loads the nix-darwin environment.
        programs.zsh.enable = true; # default shell on catalina

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 5;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";

        # ========================================
        # MACOS SYSTEM DEFAULTS
        # ========================================
        system.defaults = {
          # Dock configuration
          dock = {
            autohide = true;
            mru-spaces = false;
            orientation = "bottom";
            # autohide-delay = 0.0;
            # autohide-time-modifier = 0.5;
            # showhidden = true;  # Show hidden apps as translucent
            # show-recents = false;
            # mineffect = "scale";  # or "genie"
            # minimize-to-application = true;
            # launchanim = false;  # Don't animate opening applications
            # static-only = false;  # Show only open applications
            # tilesize = 48;  # Icon size
            # largesize = 64;  # Magnified icon size
            # magnification = false;  # Enable magnification
            # persistent-apps = [  # Pin apps to dock
            #   "/Applications/Ghostty.app"
            #   "/System/Applications/Safari.app"
            # ];
          };

          # Finder configuration
          finder = {
            AppleShowAllExtensions = true;
            AppleShowAllFiles = true;
            FXPreferredViewStyle = "clmv"; # Column view
            # ShowPathbar = true;
            # ShowStatusBar = true;
            # FXDefaultSearchScope = "SCcf";  # Search current folder by default
            # FXEnableExtensionChangeWarning = false;
            # _FXShowPosixPathInTitle = true;  # Show full path in title
            # CreateDesktop = true;  # Show icons on desktop
            # QuitMenuItem = true;  # Allow quitting Finder
          };

          # Global macOS settings (NSGlobalDomain)
          # NSGlobalDomain = {
          #   # Keyboard
          #   "com.apple.keyboard.fnState" = true;  # Use F keys as standard
          #   AppleKeyboardUIMode = 3;  # Full keyboard navigation
          #   ApplePressAndHoldEnabled = false;  # Disable press-and-hold for accents
          #   InitialKeyRepeat = 15;  # Faster initial key repeat
          #   KeyRepeat = 2;  # Faster key repeat
          #   NSAutomaticCapitalizationEnabled = false;
          #   NSAutomaticDashSubstitutionEnabled = false;
          #   NSAutomaticPeriodSubstitutionEnabled = false;
          #   NSAutomaticQuoteSubstitutionEnabled = false;
          #   NSAutomaticSpellingCorrectionEnabled = false;
          #
          #   # Mouse/Trackpad
          #   "com.apple.mouse.tapBehavior" = 1;  # Tap to click
          #   "com.apple.trackpad.enableSecondaryClick" = true;
          #   "com.apple.trackpad.scaling" = 1.5;  # Tracking speed
          #   "com.apple.swipescrolldirection" = true;  # Natural scrolling
          #
          #   # UI/UX
          #   AppleShowScrollBars = "Always";  # or "Automatic" or "WhenScrolling"
          #   AppleInterfaceStyle = "Dark";  # Dark mode
          #   AppleInterfaceStyleSwitchesAutomatically = false;
          #   NSNavPanelExpandedStateForSaveMode = true;
          #   NSNavPanelExpandedStateForSaveMode2 = true;
          #   PMPrintingExpandedStateForPrint = true;
          #   PMPrintingExpandedStateForPrint2 = true;
          #   NSDocumentSaveNewDocumentsToCloud = false;
          #   AppleMeasurementUnits = "Centimeters";  # or "Inches"
          #   AppleMetricUnits = 1;  # Use metric
          #   AppleTemperatureUnit = "Celsius";  # or "Fahrenheit"
          #
          #   # Sound
          #   "com.apple.sound.beep.feedback" = 0;  # Disable feedback sound
          #   "com.apple.sound.uiaudio.enabled" = 0;  # Disable UI sounds
          # };

          # Trackpad settings
          # trackpad = {
          #   Clicking = true;  # Tap to click
          #   TrackpadRightClick = true;  # Two-finger right click
          #   TrackpadThreeFingerDrag = true;  # Three-finger drag
          #   ActuationStrength = 0;  # Light force click
          #   FirstClickThreshold = 0;  # Light first click
          #   SecondClickThreshold = 0;  # Light second click
          # };

          # Screenshot settings
          # screencapture = {
          #   location = "~/Pictures/Screenshots";
          #   type = "png";  # or "jpg"
          #   disable-shadow = false;
          # };

          # Screensaver and security
          # screensaver = {
          #   askForPassword = true;
          #   askForPasswordDelay = 0;  # Require password immediately
          # };

          # Login window
          # loginwindow = {
          #   GuestEnabled = false;  # Disable guest account
          #   DisableConsoleAccess = true;
          #   SHOWFULLNAME = true;  # Show full name instead of username
          # };

          # Spaces
          # spaces.spans-displays = false;  # Each display has separate spaces

          # Activity Monitor
          # ActivityMonitor = {
          #   IconType = 5;  # CPU usage
          #   OpenMainWindow = true;
          #   ShowCategory = 0;  # All processes
          #   SortColumn = "CPUUsage";
          #   SortDirection = 0;  # Descending
          # };

          # Custom user preferences
          # CustomUserPreferences = {
          #   "com.apple.Safari" = {
          #     IncludeDevelopMenu = true;
          #     WebKitDeveloperExtrasEnabledPreferenceKey = true;
          #     "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" = true;
          #   };
          #   "com.apple.finder" = {
          #     ShowExternalHardDrivesOnDesktop = true;
          #     ShowHardDrivesOnDesktop = true;
          #     ShowMountedServersOnDesktop = true;
          #     ShowRemovableMediaOnDesktop = true;
          #   };
          #   "com.apple.TimeMachine" = {
          #     DoNotOfferNewDisksForBackup = true;
          #   };
          #   "com.apple.AdLib" = {
          #     allowApplePersonalizedAdvertising = false;
          #   };
          # };
        };

        # ========================================
        # SECURITY SETTINGS
        # ========================================
        # Enable TouchID for sudo
        # security.pam.services.sudo_local.touchIdAuth = true;

        # ========================================
        # NETWORKING
        # ========================================
        # networking = {
        #   hostName = "bens-macbook";
        #   # dns = ["1.1.1.1" "8.8.8.8"];  # Custom DNS servers
        #   # knownNetworkServices = ["Wi-Fi" "Ethernet"];
        # };

        # ========================================
        # FONTS
        # ========================================
        # fonts = {
        #   fontDir.enable = true;
        #   fonts = with pkgs; [
        #     # Programming fonts
        #     fira-code
        #     fira-code-symbols
        #     jetbrains-mono
        #     source-code-pro
        #     hack-font
        #     cascadia-code
        #
        #     # System fonts
        #     inter
        #     roboto
        #     open-sans
        #
        #     # Nerd fonts (for terminal icons)
        #     (nerdfonts.override { fonts = [
        #       "FiraCode"
        #       "JetBrainsMono"
        #       "Hack"
        #       "Meslo"
        #     ]; })
        #   ];
        # };

        # ========================================
        # LAUNCHD SERVICES
        # ========================================
        # Custom user launch agents
        # launchd.user.agents = {
        #   # Example: Auto-commit dotfiles
        #   dotfiles-auto-commit = {
        #     serviceConfig = {
        #       ProgramArguments = [
        #         "/bin/sh" "-c"
        #         "cd ~/.dotfiles && git add -A && git commit -m 'Auto-update' || true"
        #       ];
        #       StartInterval = 3600;  # Every hour
        #       StandardErrorPath = "/tmp/dotfiles-commit.error.log";
        #       StandardOutPath = "/tmp/dotfiles-commit.out.log";
        #     };
        #   };
        # };

        # HomeBrew =
        homebrew.enable = true;
        homebrew.onActivation = {
          autoUpdate = true;
          cleanup = "uninstall";
          upgrade = true;
        };

        homebrew.brewPrefix = "/opt/homebrew/bin";

        homebrew.brews = [
          # Packages that need to stay in Homebrew
          "amp" # Text editor - not in main nixpkgs
          "argocd-autopilot" # Not confirmed in nixpkgs
          "acli" # Atlassian CLI - not in nixpkgs
          "container-compose" # Docker compose alternative

          # Keep for Docker Desktop integration
          "docker"
          "docker-completion"

          # Required by emacs-plus@31
          "gcc"
          "gnutls"
          "tree-sitter"

          # Python version not yet verified in nixpkgs
          "python@3.14"
          "python-argcomplete"
        ];

        homebrew.taps = [
          "nikitabobko/tap"
          "d12frosted/emacs-plus"
          "atlassian/homebrew-acli"
        ];

        homebrew.casks = [
          "aerospace"
          "flutter"
          "ghostty"
          "raycast"
          "jordanbaird-ice"
          "hammerspoon"
          "librewolf"
          "element"
          "signal"
        ];
      };

    in {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Benjamins-MacBook-Pro-2
      darwinConfigurations."Benjamins-MacBook-Pro-2" =
        nix-darwin.lib.darwinSystem {
          modules = [ configuration home-manager.darwinModules.home-manager ];
        };

      darwinConfigurations."Bens-BlackBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ configuration home-manager.darwinModules.home-manager ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Benjamins-MacBook-Pro-2".pkgs;

      formatter.aarch64-darwin =
        nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
    };

}
