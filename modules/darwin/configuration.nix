{ config, pkgs, ... }:

{
  # System-level packages (available to all users)
  environment.systemPackages = with pkgs; [
    emacs

    # Shell & system tools
    coreutils
    gnused
    gnutar
    gnugrep
    gawk
    gnumake
    openssh

    # Build tools
    autoconf
    cmake
    libtool
    m4
    ninja
    pkg-config
    texinfo
    diffutils

    # Security & certificates
    cacert
    openssl
    libfido2

    # Compression utilities
    brotli
    lz4
    xz
    zstd

    # Network tools
    unbound
    openldap
  ];

  # Services
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  # Nix settings
  nix.settings.experimental-features = "nix-command flakes";

  # Shell
  programs.zsh.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # macOS system defaults
  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "bottom";
    };

    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      FXPreferredViewStyle = "clmv";
    };
  };

  # Homebrew
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true;
    cleanup = "uninstall";
    upgrade = true;
  };

  homebrew.brewPrefix = "/opt/homebrew/bin";

  homebrew.brews = [
    # Text editors
    "amp"

    # Containers & GitOps
    "docker"
    "docker-completion"
    "container-compose"
    "argocd-autopilot"

    # Languages
    "python@3.14"
    "python-argcomplete"

    # Emacs dependencies
    "gcc"
    "gnutls"
    "tree-sitter"
    "pngpaste"
    "grip"
    "php"
    "composer"

    # Email
    "isync"
    "mailutils"

    # CLI tools
    "acli"
    "firefoxpwa"
    "spotify_player"

    # AI tools
    "block-goose-cli"
  ];

  homebrew.taps = [
    "nikitabobko/tap"
    "d12frosted/emacs-plus"
    "atlassian/homebrew-acli"
  ];

  homebrew.casks = [
    # Window management & productivity
    "aerospace"
    "raycast"
    "jordanbaird-ice"
    "hammerspoon"

    # Terminal
    "ghostty"

    # Browsers
    "librewolf"

    # Messaging
    "element"
    "signal"

    # Development
    "flutter"
    "bruno"

    # AI tools
    "claude-code"
    "codex"

    # Security
    "1password-cli"
  ];
}
