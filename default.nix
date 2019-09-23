{ compiler ? "ghc865"
, pkgs ? (import ./pinned-packages.nix).pkgs1909
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {}; # Overrides here if necessary
  };
in
  overriddenPackages.callCabal2nix "taskwarrior-habitica-bridge" ./. {}
