{ lib, config, pkgs, ... }:

{
  # Linux-specific home-manager config

  home.packages = with pkgs; [
    # Window manager (i3 config managed via stow)
    i3
    i3status
    dmenu
    rofi

    # Linux utilities
    xclip
    xsel

    # Keep Linux on Nix-managed GPG tooling.
    gnupg
    pinentry-curses
  ];

  # Set only the Git signing binary, without replacing full ~/.gitconfig.
  home.activation.configureGitGpgProgram = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ -x ${pkgs.gnupg}/bin/gpg ]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.program ${pkgs.gnupg}/bin/gpg
      $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.format openpgp
    fi
  '';
}
