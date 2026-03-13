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
  cronMarkerStart = "# >>> home-manager codex jobs >>>";
  cronMarkerEnd = "# <<< home-manager codex jobs <<<";
  darwinCronLogDir = "${homeDir}/Library/Logs";
  garageConfigDir = "${homeDir}/.config/garage";
  garageConfigFile = "${garageConfigDir}/config.toml";
  garageStateDir = "${homeDir}/.local/share/garage";
  garageMetadataDir = "${garageStateDir}/meta";
  garageDataDir = "${garageStateDir}/data";
  garageSecretsDir = "${homeDir}/.local/state/garage";
  garageRpcSecretFile = "${garageSecretsDir}/rpc_secret";
  garageLogFile = if isDarwin then "${homeDir}/Library/Logs/garage.log" else "${garageSecretsDir}/garage.log";
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

  # Keep sqlite3 stable across mixed package-manager PATHs.
  home.file.".local/bin/sqlite3".source = "${pkgs.sqlite}/bin/sqlite3";

  home.file.".config/garage/config.toml".text = ''
    metadata_dir = "${garageMetadataDir}"
    data_dir = "${garageDataDir}"
    db_engine = "sqlite"

    replication_factor = 1

    rpc_bind_addr = "127.0.0.1:3901"
    rpc_public_addr = "127.0.0.1:3901"
    rpc_secret_file = "${garageRpcSecretFile}"

    [s3_api]
    s3_region = "garage"
    api_bind_addr = "127.0.0.1:3900"
    root_domain = ".s3.garage.localhost"

    [s3_web]
    bind_addr = "127.0.0.1:3902"
    root_domain = ".web.garage.localhost"
    index = "index.html"

    [k2v_api]
    api_bind_addr = "127.0.0.1:3904"

    [admin]
    api_bind_addr = "127.0.0.1:3903"
  '';

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
    hk
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
    google-cloud-sdk
    argocd
    argocd-vault-plugin
    docker
    garage_2
    ansible

    # Database tools
    mysql80
    postgresql
    sqlite
    pkgs."sqlite-vec"

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

  home.sessionVariables = {
    GARAGE_CONFIG_FILE = garageConfigFile;
  };

  # Keep git signing config stable across hosts by using ~/.local/bin/gpg.
  home.activation.configureGitGpgProgram =
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ -x "${gpgProgram}" ]; then
        $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.program "${gpgProgram}"
        $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.format openpgp
      fi
    '';

  home.activation.setupGarage = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ${pkgs.coreutils}/bin/mkdir -p \
      "${garageMetadataDir}" \
      "${garageDataDir}" \
      "${garageSecretsDir}" \
      "${if isDarwin then "${homeDir}/Library/Logs" else garageSecretsDir}"

    if [ ! -s "${garageRpcSecretFile}" ]; then
      umask 077
      ${pkgs.openssl}/bin/openssl rand -hex 32 > "${garageRpcSecretFile}"
    fi

    ${pkgs.coreutils}/bin/chmod 600 "${garageRpcSecretFile}"
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

    systemd.user.services.garage = {
      Unit.Description = "Garage object store";
      Service = {
        ExecStart = "${pkgs.garage_2}/bin/garage -c ${garageConfigFile} server";
        WorkingDirectory = garageStateDir;
        Restart = "on-failure";
        RestartSec = 5;
        LimitNOFILE = 42000;
        Environment = [
          "RUST_LOG=garage=info"
        ];
      };
      Install.WantedBy = [ "default.target" ];
    };
  })

  (lib.mkIf isDarwin {
    launchd.agents.garage = {
      enable = true;
      config = {
        ProcessType = "Background";
        ProgramArguments = [
          "${pkgs.garage_2}/bin/garage"
          "-c"
          garageConfigFile
          "server"
        ];
        WorkingDirectory = garageStateDir;
        KeepAlive = true;
        RunAtLoad = true;
        EnvironmentVariables = {
          GARAGE_CONFIG_FILE = garageConfigFile;
          RUST_LOG = "garage=info";
        };
        StandardOutPath = garageLogFile;
        StandardErrorPath = garageLogFile;
      };
    };

    # macOS scheduling via user crontab, managed declaratively through Home Manager activation.
    home.activation.installCodexCron = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      crontab_cmd=/usr/bin/crontab
      tmp="$(${pkgs.coreutils}/bin/mktemp)"
      cleaned="$(${pkgs.coreutils}/bin/mktemp)"

      if [ ! -x "$crontab_cmd" ]; then
        echo "codex cron: /usr/bin/crontab not found; skipping"
      else
        if ! "$crontab_cmd" -l >"$tmp" 2>/dev/null; then
          : >"$tmp"
        fi

        ${pkgs.gnused}/bin/sed '/^${lib.escapeRegex cronMarkerStart}$/, /^${lib.escapeRegex cronMarkerEnd}$/d' "$tmp" >"$cleaned"

        if [ -s "$cleaned" ]; then
          printf '\n' >>"$cleaned"
        fi

        cat >>"$cleaned" <<'EOF'
${cronMarkerStart}
MAILTO=""
SHELL=/bin/sh
PATH=${codexPath}
0,30 * * * 1-5 ${runJiraIntake} >> ${darwinCronLogDir}/codex-jira-intake.cron.log 2>&1
*/15 * * * * ${runFanin} >> ${darwinCronLogDir}/codex-fanin.cron.log 2>&1
${cronMarkerEnd}
EOF

        if ! ${pkgs.diffutils}/bin/cmp -s "$tmp" "$cleaned"; then
          $DRY_RUN_CMD "$crontab_cmd" "$cleaned"
        fi
      fi

      ${pkgs.coreutils}/bin/rm -f "$tmp" "$cleaned"
    '';
  })
]
