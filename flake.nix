{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle-exe = {
      url = "github:3noch/nix-bundle-exe";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-bundle-exe, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        patchedHaskellPackages = pkgs.haskell.packages.ghc94.override {
          overrides = self: super: {
            # disable tests in optics
            optics = pkgs.haskell.lib.dontCheck super.optics;
          };
        };

        lens-profiling = with pkgs; lib.pipe
          (patchedHaskellPackages.callCabal2nix "lens-profiling" ./. {})
          [
            (haskell.lib.compose.disableCabalFlag "static")
          ];
      in rec {
        packages.default = pkgs.hello;
        devShell = with pkgs;
          patchedHaskellPackages.shellFor {
            packages = _: [ lens-profiling ];
            shellHook = "hpack";
            buildInputs = with patchedHaskellPackages; [
              hpack
              hlint
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
      }
    );
}
