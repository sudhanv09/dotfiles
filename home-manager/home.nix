{ config, pkgs, ... }:

{
  home.username = "zeus";
  home.homeDirectory = "/home/zeus";
  home.stateVersion = "25.05"; # Dont CHANGE !!!

  home.packages = with pkgs; [
    # APPS
    emacs
    zed-editor
    calibre
    
    # CLI
    git
    jujutsu
    awesome
    rofi
    ripgrep
    lazygit
    yazi
    dust
    fd	
    fish
    fishPlugins.tide
    gh
    eza
    mpv
    fzf
    zip
    btop
    zoxide
    
    bun
    python313
    go
    gcc
    gnumake
    uv
        
    # FONTS
    fira-code
    hack-font
  ];
  

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".config/awesome/rc.lua".source = ../.config/awesome/rc.lua;
    ".config/kitty".source = ../.config/kitty;
    ".config/rofi".source = ../.config/rofi;
  };

  home.sessionVariables = {
    EDITOR = "vi";
  };
  
  home.shellAliases = {
    ls="eza -G --icons";
    dust="dust -rbd 1";
    lg="lazygit";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.fish = {
    enable = true;
    plugins = [{name = "tide"; src = pkgs.fishPlugins.tide.src;}];
  };
  
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };
  
  programs.git = {
    enable = true;
    userName = "sudhanv09";
    userEmail = "mystogun125@gmail.com";
  };
  
  programs.gh = {
    enable = true;
    gitCredentialHelper = {
      enable = true;
    };
  };

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-chewing
      fcitx5-rime
    ];
  };

}
