{ pkgs, system, sf-mono-liga-src, claude-code, ... }:
let sf-mono-liga = pkgs.stdenvNoCC.mkDerivation {
    name = "sf-mono-liga";
    src = sf-mono-liga-src;
    installPhase = ''
      mkdir -p $out/share/fonts/opentype
      cp -r $src/*.otf $out/share/fonts/opentype/
    ''; 
};
in
{
  home.username = "zeus";
  home.homeDirectory = "/home/zeus";
  home.sessionVariables = {
  XDG_DATA_DIRS = "$HOME/.nix-profile/share:$HOME/.local/share:/run/current-system/sw/share:/usr/share:/usr/local/share";
};

  nixpkgs.config.allowUnfree = true;
  fonts.fontconfig.enable = true;

  services.emacs = {
   enable = true;
   package = pkgs.emacs-pgtk;
   defaultEditor = true;
};


  programs.dank-material-shell = {
    enable = true;
    systemd.enable = true;
  };

  programs.fish = {
    enable = true;
    plugins = [
      { name = "pure"; src = pkgs.fishPlugins.pure.src; }
    ];
    shellAliases = {
      ls = "eza --icons -G";
      dust = "dust -rb -d 1 -o mb";
      hms = "home-manager switch --flake ~/code/dotfiles/nix#zeus";
      nrs = "sudo nixos-rebuild switch --flake ~/code/dotfiles/nix#zeus-nixos";
    };
  };

  programs.foot = {
    enable = true;
    settings.main.font = "Liga SFMono Nerd Font:size=10";
  };

  programs.yazi = { enable = true; shellWrapperName = "y"; };
  programs.zellij.enable = true;
  programs.lazygit.enable = true;
  programs.eza = {
    enable = true;
  };

  home.packages = with pkgs; [
    firefox
    brave
    
    git
    ripgrep 
    fd
    dust
    jujutsu
    jjui
    foot
    emacs-pgtk
    wl-clipboard
    claude-code
    gh

    kdePackages.okular

    sf-mono-liga
    noto-fonts-cjk-sans
  ];
  home.stateVersion = "25.11";
}
