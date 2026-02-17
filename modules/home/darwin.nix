{ config, pkgs, ... }:

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

}
