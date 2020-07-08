# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];

  networking.hostName = "ul30a"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
  networking.defaultGateway = "192.168.1.1";
  # networking.nameservers = [ "192.168.1.2" ];
  networking.extraHosts = ''
    192.168.1.2    rpi
    127.0.0.1      localhost
  '';

  security.wrappers.slock.source = "${pkgs.slock.out}/bin/slock";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    packages = [
      pkgs.terminus_font
    ];
    font = "ter-132n";
    useXkbConfig = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    abiword
    ack
    arandr
    bluez
    brightnessctl
    caddy
    calibre
    chromium
    clojure
    conky
    docker
    docker_compose
    emacs
    fd
    feh
    firefox
    firefox-beta-bin
    gimp
    gitFull
    gnome3.adwaita-icon-theme
    go
    gocryptfs
    google-chrome
    htop
    inkscape
    jack2Full
    keynav
    leiningen
    lsof
    maven
    mesa
    mksh
    moreutils
    mpv
    mupdf
    moreutils
    ncdu
    nodejs-10_x
    ntfs3g
    openbox
    openvpn
    p7zip
    pandoc
    pavucontrol
    pcmanfm
    pgcli
    pkgs.gnumake
    postgresql
    pulsemixer
    pwsafe
    python
    qjackctl
    rclone
    ripgrep
    rlwrap
    rofi-unwrapped
    rxvt_unicode
    scaleway-cli
    scrot
    silver-searcher
    slock
    sshfs
    sshuttle
    steam
    steam-run
    stress
    supercollider
    texmacs
    tigervnc
    tmux
    tree
    unzip
    urxvt_perls
    vagrant
    vanilla-dmz
    vim
    virtualbox
    wget
    x2goclient
    xbanish
    xclip
    xorg.xbacklight
    xorg.xmodmap
    xorg.xrandr
    zathura
    zile
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.adb.enable = true;
  programs.bash.enableCompletion = true;
  programs.java.enable = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  # programs.zsh.enable = true;

  fonts = {
    fonts = with pkgs; [
      cascadia-code
      dejavu_fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
    ];
    fontconfig = {
      dpi = 0;
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        sansSerif = [ "Source Sans Pro" ];
        serif = [ "Source Serif Pro" ];
      };
      penultimate = {
        enable = true;
      };
    };
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Bluetooth
  hardware.bluetooth.enable = true;

  # Steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  # Optimus
  hardware.bumblebee.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  location.latitude = 51.5074;
  location.longitude = 0.1278;

  services = {
    tlp = {
      enable = true;
    };

    # Enable the OpenSSH daemon.
    # openssh.enable = true;

    openvpn = {
      servers = {
        #  netherlands  = { config = '' config /root/nixos/openvpn/AirVPN_Netherlands_UDP-443.ovpn ''; };
        #  spain        = { config = '' config /root/nixos/openvpn/AirVPN_Spain_UDP-443.ovpn ''; };
        #  uk           = { config = '' config /root/nixos/openvpn/AirVPN_United-Kingdom_UDP-443.ovpn ''; };
      };
    };

    # Enable CUPS to print documents.
    # printing.enable = true;

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "colemak";
      # dpi = 96;
      # dpi = 120;

      displayManager = {
        defaultSession = "none+openbox";
        lightdm.background = "black";
        sessionCommands = ''
          source $HOME/.xsession
        '';
      };

      windowManager = {
        openbox = {
          enable = true;
        };
        # default = "exwm";
        # exwm = {
        #   enable = true;
        # };
      };

      libinput = {
        enable = true;
        naturalScrolling = true;
      };
    };

    compton = {
      enable = true;
      backend = "glx";
      vSync = true;
    };

    logind = {
      lidSwitch = "suspend";
    };

    redshift = {
      enable = true;
      brightness = {
        day = "1.0";
        night = "0.8";
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.miguelsm = {
    isNormalUser = true;
    home = "/home/miguelsm";
    shell = pkgs.mksh;
    extraGroups = [
      "adbusers"
      "audio"
      "pulse"
      "disk"
      "docker"
      "input"
      "messagebus"
      "networkmanager"
      "systemd-journal"
      "video"
      "wheel"
      "vboxsf"
    ];
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
