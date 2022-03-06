self: super:
let
  unstable = import super.sources.nixos-unstable { };
in
{
  inherit unstable;
  inherit (unstable) cachix cabal2nix;
  nix = self.nix_2_4;
  unstableHaskellPackages = unstable.haskellPackages;
  unstableGhc = unstable.ghc;
  home-assistant = unstable.home-assistant;
  vscode-extensions = unstable.vscode-extensions;
  vscode = unstable.vscode;
  nix-output-monitor = unstable.nix-output-monitor.overrideAttrs (old: {
    src = super.sources.nix-output-monitor;
    buildInputs = old.buildInputs ++ (with unstable.haskellPackages; [ streamly optics generic-optics extra safe ]);
  });
}
