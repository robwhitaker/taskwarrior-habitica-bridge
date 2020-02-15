let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) {};
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
