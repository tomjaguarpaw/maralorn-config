{ pkgs, defaultCrateOverrides, makeDesktopItem, ... }:
((pkgs.callPackage ./Cargo.nix {}).habitask_0_1_0 {}).override {
  crateOverrides = defaultCrateOverrides // {
    openssl-sys = attrs: {
      buildInputs = [
        pkgs.pkgconfig
        pkgs.openssl
      ];
    };
    habitask = attrs: {
      postInstall = ''
        rm $out/lib/link
      '';
    };
  };
}
