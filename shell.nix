let
  pkgs = (import ./pinned-packages.nix).pkgs1903;
  drv = import ./. { inherit pkgs; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      stylish-haskell
      hlint
    ];
  }
