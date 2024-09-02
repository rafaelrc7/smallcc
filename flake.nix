{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default-linux";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { config, pkgs, ... }: {
        devShells.default = import ./shell.nix { inherit pkgs; };

        packages = rec {
          default = smallcc;
          smallcc = pkgs.callPackage ./default.nix { };
        };

        apps.default = {
          type = "app";
          program = "${config.packages.smallcc}/bin/smallcc";
        };

        overlayAttrs = {
          inherit (config.packages) smallcc;
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            stylish-haskell.enable = true;
          };
        };
      };
    };
}
