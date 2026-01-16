{ config, pkgs, ... }:

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
  ];
}
