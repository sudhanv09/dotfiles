{
  description = "Home nix flake";
  inputs = {
      nixpkgs.url = "github:NixOs/nixpkgs/nixos-unstable";
      nix-stable.url = "github:NixOs/nixpkgs/nixos-25.05";
      home-manager = {
          url = "github:nix-community/home-manager";
          inputs.nixpkgs.follows = "nixpkgs";
      };
      
      hardware.url = "github:nixos/nixos-hardware"; 
  };
        
  outputs = {self, nixpkgs, home-manager, ...}@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      
      sfmonoFont = pkgs.stdenv.mkDerivation {
        pname = "sf-mono-font";
        src = pkgs.fetchFromGithub {
          owner = "shaunsingh";
          repo = "SFMono-Nerd-Font-Ligaturized";
          sha256 = "0qibyikzi7iwdzqi3wv0hgzimj5cm43c3v6f9znw53dwa4r9m26a";
        };
        installPhase = ''
        mkdir -p $out/share/fonts/truetype
        cp -r $src $out/share/fonts/truetype/
      '';
      };
    in {
      nixosConfigurations = {
        home-nix = nixpkgs.lib.nixosSystem {
            specialArgs = {inherit inputs;};
            modules = [
              ./nixos/configuration.nix
            ];
        };
      };
          
      homeConfigurations = {
        "zeus@home-nix" = home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
            extraSpecialArgs = {inherit inputs sfmonoFont;};
            modules = [
                ./home-manager/home.nix
            ];
        };  
      };
  };
}
