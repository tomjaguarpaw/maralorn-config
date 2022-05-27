{
  description = "wizards-dialog";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system: rec {

    packages.default = (nixpkgs.legacyPackages.${system}.haskellPackages.extend haskellPackagesOverlay).wizards-dialog;

    haskellPackagesOverlay = final: prev: {
      wizards-dialog = final.callCabal2nix "wizards-dialog" self {};
    };

  });
}
