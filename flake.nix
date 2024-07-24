{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default-linux";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { self', ... }: {
        haskellProjects.default = {
          projectFlakeName = "smallcc";

          devShell = {
            enable = true;
            hlsCheck.enable = true;
            tools = hp: {
              inherit (hp) cabal-install haskell-language-server;
            };
          };
        };

        packages.default = self'.packages.smallcc;

        overlayAttrs = {
          inherit (self'.packages) smallcc;
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
