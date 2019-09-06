{ compiler ? "ghc863", pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
      req = self.req_2_0_1;
      retry = self.retry_0_8_0_0;
    };
  };
in
  overriddenPackages.callCabal2nix "taskwarrior-habitica-bridge" ./. {}
