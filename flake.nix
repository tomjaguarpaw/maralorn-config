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
        haskellPackages = haskell.packages."ghc${ghc-version}";
        hlib = haskell.lib.compose;
        inherit (hlib) doJailbreak dontCheck;
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
          ];
          withHoogle = true;
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        };
      }
    );
}
