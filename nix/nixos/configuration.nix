{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.timeout = 10;

  nix.settings.experimental-features=["nix-command" "flakes"];

  networking.hostName = "zeus-nixos";
  networking.networkmanager.enable = true;
 
  time.timeZone = "Asia/Taipei";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.zeus = {
    isNormalUser = true;
    description = "sudhanv";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
     vim
     wget
  ];

  programs.niri.enable = true;
  programs.fish.enable = true;
  services.greetd = {
   enable = true;
   settings.default_session = {command="niri-session"; user="zeus";};
  };

  fonts.packages = with pkgs; [
   nerd-fonts.jetbrains-mono
   nerd-fonts.meslo-lg
   nerd-fonts.fira-code
  ];

  xdg.portal = {enable=true; extraPortals=[pkgs.xdg-desktop-portal-gnome];};

  system.stateVersion = "25.11"; # Did you read the comment?
}
