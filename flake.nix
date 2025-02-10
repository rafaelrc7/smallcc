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

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.flake-parts.flakeModules.easyOverlay
      ];

      perSystem =
        { config, pkgs, ... }:
        let
          # GHC version, change to `pkgs.haskell.packages.ghcXYZ`
          # Available version list can be querried with `nix-env -f "<nixpkgs>" -qaP -A haskell.compiler`
          haskellPackages = pkgs.haskellPackages;
        in
        {
          packages = rec {
            default = smallcc;
            smallcc = pkgs.callPackage ./default.nix { inherit haskellPackages; };
          };

          apps.default = {
            type = "app";
            program = "${config.packages.smallcc}/bin/smallcc";
          };

          overlayAttrs = {
            inherit (config.packages) smallcc;
          };

          devShells.default = import ./shell.nix { inherit pkgs haskellPackages; };

          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              cabal-fmt.enable = true;
              nixfmt.enable = true;
              prettier.enable = true;
              stylish-haskell.enable = true;
            };
          };
        };
    };
}
