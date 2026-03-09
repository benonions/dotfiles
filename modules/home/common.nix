{ lib, config, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
  localBin = "${config.home.homeDirectory}/.local/bin";
  gpgProgram = "${localBin}/gpg";
in {
  # Stable, user-local command path to avoid per-machine binary drift.
  home.file.".local/bin/gpg".source = if isDarwin then
    config.lib.file.mkOutOfStoreSymlink "/usr/local/MacGPG2/bin/gpg"
  else
    "${pkgs.gnupg}/bin/gpg";

  home.file.".local/bin/gpgconf".source = if isDarwin then
    config.lib.file.mkOutOfStoreSymlink "/usr/local/MacGPG2/bin/gpgconf"
  else
    "${pkgs.gnupg}/bin/gpgconf";

  home.file.".local/bin/gpg-connect-agent".source = if isDarwin then
    config.lib.file.mkOutOfStoreSymlink
    "/usr/local/MacGPG2/bin/gpg-connect-agent"
  else
    "${pkgs.gnupg}/bin/gpg-connect-agent";

  home.packages = with pkgs; [
    # Text editors
    vim
    helix
    neovim
    entr
    # docs
    tldr
    mermaid-cli
    d2
    gnuplot

    # Development tools
    nixd
    nixfmt
    nil
    lazygit
    gh
    git-lfs
    rustup
    elixir
    nodejs
    nodePackages.pnpm
    yarn
    python312
    pipx
    direnv
    starship
    wget
    age
    mb2md
    marksman

    # Charm.sh tools
    gum
    glow
    skate
    soft-serve
    pandoc
    w3m-full

    # Lua
    luarocks
    fennel
    luajit
    lua-language-server

    # Terminal utilities
    ripgrep
    fzf
    btop
    ranger
    zoxide
    fd
    zellij
    nushell
    eza
    lf
    nnn
    hexyl
    parallel
    stow
    tree
    bat
    lnav
    tmux
    jq
    curl

    # Container/Cloud tools
    k9s
    kubectl
    krew
    doctl
    argocd
    argocd-vault-plugin
    docker
    ansible

    # Database tools
    mysql80
    postgresql
    sqlite

    # Fun stuff
    thefuck
    lolcat
    taskwarrior-tui
    cbonsai

    # Security
    yubikey-agent
    gitleaks
    semgrep

    # Network tools
    speedtest-cli
    nmap
    poppler

    # Git hooks
    # pre-commit  # temporarily disabled - dotnet-sdk-8.0 broken upstream

    # Emacs/Doom tooling
    shellcheck
    shfmt
    editorconfig-core-c
    imagemagick
    clang-tools
    terraform
    zig
    clj-kondo
    dockfmt
    cljfmt
    erlfmt
    rebar3
    plantuml
    gitleaks
  ];

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  # Keep git signing config stable across hosts by using ~/.local/bin/gpg.
  home.activation.configureGitGpgProgram =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -x "${gpgProgram}" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.program "${gpgProgram}"
        $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.format openpgp
      fi
    '';

  programs.home-manager.enable = true;
}
