{ compiler ? null
, pkgs ? import <nixpkgs> {}
}:

let
  haskellPackages =
    if builtins.isNull compiler
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {}; # Overrides here if necessary
  };
in
  overriddenPackages.callCabal2nix "taskwarrior-habitica-bridge" ./. {}
