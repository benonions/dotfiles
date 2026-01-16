{
  description = "Ben's multi-platform dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, nix-darwin, home-manager, ... }:
    let
      username = "ben";

      # Helper to create home-manager config for any system
      mkHomeConfig = { system, extraModules ? [] }: home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        modules = [
          ./modules/home/common.nix
          {
            home = {
              username = username;
              homeDirectory = if (builtins.match ".*-darwin" system != null)
                then "/Users/${username}"
                else "/home/${username}";
              stateVersion = "25.05";
            };
          }
        ] ++ extraModules;
      };

      # Darwin system configuration
      darwinConfig = { pkgs, config, ... }: {
        system.primaryUser = username;

        users.users.${username} = {
          name = username;
          home = "/Users/${username}";
        };

        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${username} = { pkgs, ... }: {
          imports = [
            ./modules/home/common.nix
            ./modules/home/darwin.nix
          ];
          home.stateVersion = "25.05";
        };

        imports = [ ./modules/darwin/configuration.nix ];

        system.configurationRevision = self.rev or self.dirtyRev or null;
        system.stateVersion = 5;
        nixpkgs.hostPlatform = "aarch64-darwin";
      };

    in {
      # macOS configurations
      darwinConfigurations."Benjamins-MacBook-Pro-2" = nix-darwin.lib.darwinSystem {
        modules = [ darwinConfig home-manager.darwinModules.home-manager ];
      };

      darwinConfigurations."Bens-BlackBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [ darwinConfig home-manager.darwinModules.home-manager ];
      };

      # Standalone home-manager configurations (for Ubuntu/Linux without NixOS)
      homeConfigurations."${username}" = mkHomeConfig {
        system = "x86_64-linux";
        extraModules = [ ./modules/home/linux.nix ];
      };

      homeConfigurations."${username}@aarch64" = mkHomeConfig {
        system = "aarch64-linux";
        extraModules = [ ./modules/home/linux.nix ];
      };

      # Expose packages for convenience
      darwinPackages = self.darwinConfigurations."Benjamins-MacBook-Pro-2".pkgs;

      # Formatter
      formatter.aarch64-darwin = nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
    };
}
