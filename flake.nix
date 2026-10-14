{
  description = "BLP format decoder/encoder over JuicyPixels library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # GHC versions we support and test in CI. Keep in sync with
        # `tested-with` in JuicyPixels-blp.cabal and .github/workflows/ci.yml.
        supportedGhcs = [ "ghc94" "ghc96" "ghc98" "ghc910" "ghc912" "ghc914" ];

        # Plain compiler + cabal shells: dependencies come from Hackage, so any
        # GHC from the list works without building a whole nix package set.
        mkShellFor = ghc: pkgs.mkShell {
          packages = [
            pkgs.haskell.compiler.${ghc}
            pkgs.cabal-install
            pkgs.zlib
            pkgs.pkg-config
          ];
        };

        hsPkgs = pkgs.haskellPackages;
        JuicyPixels-blp = hsPkgs.callCabal2nix "JuicyPixels-blp" self { };
      in {
        packages = {
          inherit JuicyPixels-blp;
          default = JuicyPixels-blp;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = pkgs.haskell.lib.justStaticExecutables JuicyPixels-blp;
          exePath = "/bin/blp2any";
        };

        devShells = pkgs.lib.genAttrs supportedGhcs mkShellFor // {
          default = hsPkgs.shellFor {
            packages = _: [ JuicyPixels-blp ];
            nativeBuildInputs = [
              pkgs.cabal-install
              hsPkgs.haskell-language-server
            ];
          };
        };

        checks.default = JuicyPixels-blp;
      });
}
