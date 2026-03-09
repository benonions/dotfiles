{ lib, config, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;
  isLinux = pkgs.stdenv.isLinux;
  homeDir = config.home.homeDirectory;
  localBin = "${homeDir}/.local/bin";
  gpgProgram = "${localBin}/gpg";
  dotfilesDir = "${homeDir}/.dotfiles";
  orgDir = "${homeDir}/org";
  codexPath =
    "$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin";
  runJiraIntake = "${dotfilesDir}/scripts/run-jira-intake.sh";
  runFanin = "${dotfilesDir}/scripts/run-fanin.sh";
in lib.mkMerge [
  {
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

  (lib.mkIf isLinux {
    # Cron-like schedules on Linux via systemd user timers.
    systemd.user.services.codex-jira-intake = {
      Unit.Description = "Run Codex Jira intake";
      Service = {
        Type = "oneshot";
        WorkingDirectory = dotfilesDir;
        ExecStart = runJiraIntake;
        Environment = [
          "HOME=${homeDir}"
          "PATH=${codexPath}"
          "ORG_DIR=${orgDir}"
        ];
      };
    };

    systemd.user.timers.codex-jira-intake = {
      Unit.Description = "Schedule Codex Jira intake";
      Timer = {
        OnCalendar = "Mon..Fri *:0/30:00";
        Persistent = true;
        Unit = "codex-jira-intake.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };

    systemd.user.services.codex-fanin = {
      Unit.Description = "Run Codex fan-in";
      Service = {
        Type = "oneshot";
        WorkingDirectory = dotfilesDir;
        ExecStart = runFanin;
        Environment = [
          "HOME=${homeDir}"
          "PATH=${codexPath}"
          "ORG_DIR=${orgDir}"
        ];
      };
    };

    systemd.user.timers.codex-fanin = {
      Unit.Description = "Schedule Codex fan-in";
      Timer = {
        OnCalendar = "*-*-* *:0/15:00";
        Persistent = true;
        Unit = "codex-fanin.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  })

  (lib.mkIf isDarwin {
    # Cron-like schedules on macOS via launchd user agents.
    launchd.agents.codex-jira-intake = {
      enable = true;
      config = {
        ProcessType = "Background";
        ProgramArguments = [ runJiraIntake ];
        WorkingDirectory = dotfilesDir;
        EnvironmentVariables = {
          HOME = homeDir;
          PATH = codexPath;
          ORG_DIR = orgDir;
        };
        StartCalendarInterval = [
          {
            Weekday = 1;
            Minute = 0;
          }
          {
            Weekday = 2;
            Minute = 0;
          }
          {
            Weekday = 3;
            Minute = 0;
          }
          {
            Weekday = 4;
            Minute = 0;
          }
          {
            Weekday = 5;
            Minute = 0;
          }
          {
            Weekday = 1;
            Minute = 30;
          }
          {
            Weekday = 2;
            Minute = 30;
          }
          {
            Weekday = 3;
            Minute = 30;
          }
          {
            Weekday = 4;
            Minute = 30;
          }
          {
            Weekday = 5;
            Minute = 30;
          }
        ];
        StandardOutPath = "${homeDir}/Library/Logs/codex-jira-intake.log";
        StandardErrorPath = "${homeDir}/Library/Logs/codex-jira-intake.log";
      };
    };

    launchd.agents.codex-fanin = {
      enable = true;
      config = {
        ProcessType = "Background";
        ProgramArguments = [ runFanin ];
        WorkingDirectory = dotfilesDir;
        EnvironmentVariables = {
          HOME = homeDir;
          PATH = codexPath;
          ORG_DIR = orgDir;
        };
        StartInterval = 900;
        RunAtLoad = true;
        StandardOutPath = "${homeDir}/Library/Logs/codex-fanin.log";
        StandardErrorPath = "${homeDir}/Library/Logs/codex-fanin.log";
      };
    };
  })
]
