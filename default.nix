{ lib
, haskell
, haskellPackages
}:
let hscompose = haskell.lib.compose;
in lib.pipe
  (haskellPackages.callCabal2nix "smallcc" (lib.cleanSource ./.) { })
  [ hscompose.dontHaddock ]

