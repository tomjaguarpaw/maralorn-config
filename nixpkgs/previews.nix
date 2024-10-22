final: _prev: {
  unstable = final.flake-inputs'.nixos-unstable.legacyPackages;
  unstableHaskellPackages = final.unstable.haskellPackages;
  nix-output-monitor = final.flake-inputs'.nix-output-monitor.packages.default;
  inherit (final.flake-inputs'.nixos-oldstable.legacyPackages) ddcutil;
  inherit (final.unstable) jujutsu;
  factorio =
    (import (builtins.getFlake "github:NixOS/nixpkgs/ed88329f5f751972acf4309e084e66ccff62066d") {
      config.allowUnfree = true;
      system = "x86_64-linux";
    }).factorio-space-age;
  helix =
    # PR https://github.com/helix-editor/helix/pull/5063
    (builtins.getFlake "github:helix-editor/helix/3e838fff106f9dc5bef37b691ade97fb4cd885b4")
    .packages.x86_64-linux.default;
}
