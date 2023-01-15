{
  description = "nixpkgs-bot";
  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (
      system: let
        ghc-version = "90";
        inherit (nixpkgs.legacyPackages.${system}) lib haskell pkgs;
        hlib = haskell.lib.compose;
        inherit (hlib) doJailbreak dontCheck markUnbroken appendPatch;
        haskellPackages = haskell.packages."ghc${ghc-version}".override {
          overrides = final: prev: {
            matrix-client =
              appendPatch (pkgs.fetchpatch {
                url = "https://github.com/softwarefactory-project/matrix-client-haskell/commit/97cb1918fcdf9b0249c6c8e70c7bfc664d718022.patch";
                sha256 = "sha256-YyxgfNO5RtqpKJ9UOYPlRple0FuNmjAB1iy9vYy0HOE=";
                relative = "matrix-client";
              })
              prev.matrix-client;
            aeson-schemas = markUnbroken (dontCheck prev.aeson-schemas);
          };
        };
        cleanSelf = lib.sourceFilesBySuffices self [
          ".hs"
          ".cabal"
          "LICENSE"
          "CHANGELOG.md"
        ];
      in rec {
        packages = {
          default =
            lib.pipe {}
            [
              (haskellPackages.callCabal2nix "nixpkgs-bot" cleanSelf)
              haskellPackages.buildFromCabalSdist
              hlib.justStaticExecutables
            ];
        };
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              alejandra.enable = true;
              statix.enable = true;
              cabal-fmt.enable = true;
              shellcheck.enable = true;
              fourmolu.enable = true;
            };
          };
        };
        devShells.default = haskellPackages.shellFor {
          packages = _: [packages.default];
          buildInputs = [
            pre-commit-hooks.defaultPackage.${system}
            haskellPackages.haskell-language-server
            haskellPackages.weeder
            pkgs.haskellPackages.cabal-install
            pkgs.sqlite
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
