{ config, pkgs, ... }:

{
  home.username = "zeus";
  home.homeDirectory = "/home/zeus";

  home.stateVersion = "25.05"; # Dont CHANGE !!!

  # The home.packages option allows you to install Nix packages into your
  # environment.
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
    
    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];
  

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
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

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-chewing
      fcitx5-rime
    ];
  };

}
