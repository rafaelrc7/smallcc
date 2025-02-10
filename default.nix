{
  lib,
  haskell,
  haskellPackages,
}:
lib.pipe (haskellPackages.callCabal2nix "smallcc" (lib.cleanSource ./.) { }) (
  with haskell.lib.compose; [ dontHaddock ]
)
