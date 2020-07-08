{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "miguelsm";
  home.homeDirectory = "/home/miguelsm";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";

  home.packages = [
    pkgs.acpi
    pkgs.cascadia-code
    pkgs.cksfv
    pkgs.cmake
    pkgs.comic-relief
    pkgs.dunst
    pkgs.gcc
    pkgs.imagemagick
    pkgs.libnotify
    pkgs.libtool
    pkgs.libvterm
    pkgs.pencil
    pkgs.python3
    pkgs.scrcpy
    pkgs.teams
    pkgs.usbutils
    pkgs.wmctrl
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
    ];
  };
}
