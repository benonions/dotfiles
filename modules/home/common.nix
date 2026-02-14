{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Text editors
    vim
    helix
    neovim

    # docs
    tldr
    mermaid-cli

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

  programs.home-manager.enable = true;
}
