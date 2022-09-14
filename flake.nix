{
  description = "Sync Taskwarrior tasks with Habitica";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskell.packages.ghc902;
        taskwarrior-habitica-bridge = haskellPackages.callCabal2nix "taskwarrior-habitica-bridge" self {};
        projectGhc = haskellPackages.ghcWithHoogle (_:
          taskwarrior-habitica-bridge.getBuildInputs.haskellBuildInputs
        );
      in
      {
        packages = { inherit taskwarrior-habitica-bridge; };
        defaultPackage = taskwarrior-habitica-bridge;

        apps.taskwarrior-habitica-bridge = {
          type = "app";
          program = "${taskwarrior-habitica-bridge}/bin/task2habitica";
        };
        defaultApp = self.apps.${system}.taskwarrior-habitica-bridge;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            # Haskell tooling
            projectGhc
            cabal2nix
            cabal-install
            haskell-language-server
          ];
        };
      }
    );
}
