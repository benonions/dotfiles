{ lib, config, pkgs, ... }:

{
  # macOS-specific home-manager config
  # Most macOS stuff is handled at system level (homebrew, system.defaults)

  # Keep macOS on a single MacGPG toolchain.
  home.sessionPath = [ "/usr/local/MacGPG2/bin" ];

  home.packages = with pkgs; [
    # Virtualization
    qemu
    # lima  # Disabled due to CVEs - re-enable when fixed
  ];

  # Set only the Git signing binary, without replacing full ~/.gitconfig.
  home.activation.configureGitGpgProgram = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ -x /usr/local/MacGPG2/bin/gpg ]; then
      $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.program /usr/local/MacGPG2/bin/gpg
      $DRY_RUN_CMD ${pkgs.git}/bin/git config --global gpg.format openpgp
    fi
  '';
}
