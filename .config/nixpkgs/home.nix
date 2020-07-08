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

  home.packages = with pkgs; [
    abiword
    android-studio
    calibre
    cascadia-code
    chromium
    cksfv
    clojure
    comic-relief
    conky
    dunst
    emacs
    feh
    firefox
    gimp
    gitFull
    gnome3.adwaita-icon-theme
    go
    gocryptfs
    google-chrome
    imagemagick
    inkscape
    keynav
    leiningen
    maven
    mpv
    mupdf
    nodejs-10_x
    pandoc
    pcmanfm
    pencil
    pgcli
    postgresql
    pwsafe
    rclone
    redshift
    rofi-unwrapped
    rxvt_unicode
    scaleway-cli
    scrcpy
    supercollider
    teams
    texmacs
    vim
    zathura
  ];
}
