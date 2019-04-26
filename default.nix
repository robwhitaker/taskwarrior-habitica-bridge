{ compiler ? "ghc863", pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
      req = self.req_2_0_1;
      retry = self.retry_0_8_0_0;
    };
  };
  drv = overriddenPackages.callCabal2nix "taskbitica" ./taskbitica.cabal {};
in
  {
    taskbitica = drv;
    taskbitica-shell = overriddenPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ cabal-install ];
    };
  }
