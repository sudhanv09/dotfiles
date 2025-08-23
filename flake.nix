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
        
    outputs = {self, nixpkgs, home-manager, ...}@inputs: {
        nixosConfigurations = {
            home-nix = nixpkgs.lib.nixosSystem {
                specialArgs = {inherit inputs;};
                modules = [
                    ./nixos/configuration.nix
                ];
            };
        };
            
        homeConfigurations = {
            zeus@home-nix = home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.x86_64-linux;
                extraSpecialArgs = {inherit inputs;};
                modules = [
                    ./home-manager/home.nix
                ];
            };  
        };
    };
}
