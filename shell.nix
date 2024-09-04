{ pkgs ? import <nixpkgs> { }, devTools ? true }:
let
  haskell = pkgs.haskellPackages.extend
    (final: prev: { smallcc = pkgs.callPackage ./default.nix { }; });
in
haskell.shellFor {
  packages = p: [ p.smallcc ];
  nativeBuildInputs = with pkgs;
    [ ghc cabal-install ] ++ lib.optional devTools [
      hlint
      ormolu
      (ghc.withPackages (p: [ p.haskell-language-server ]))
    ];
}

