# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  networking.hostName = "mi"; # Define your hostname.
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
    ack
    acpi
    arandr
    bluez
    brightnessctl
    caddy
    cmake
    docker
    docker_compose
    fd
    gcc
    htop
    jack2Full
    libnotify
    lsof
    mesa
    mksh
    moreutils
    ncdu
    ntfs3g
    openbox
    openvpn
    p7zip
    pavucontrol
    pkgs.gnumake
    pulsemixer
    python
    python3
    qjackctl
    ripgrep
    rlwrap
    scrot
    silver-searcher
    slock
    sshfs
    steam
    steam-run
    stress
    tigervnc
    tmux
    tree
    unzip
    urxvt_perls
    usbutils
    vagrant
    vanilla-dmz
    virtualbox
    wget
    wmctrl
    xbanish
    xclip
    xorg.xbacklight
    xorg.xmodmap
    xorg.xrandr
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
    cron = {
      enable = true;
      systemCronJobs = [
        "*/5 * * * *      miguelsm    source /etc/profile && /etc/cron.d/battery-check.sh >> /tmp/cron.log 2>&1"
      ];
    };

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
  system.stateVersion = "20.09"; # Did you read the comment?

}
