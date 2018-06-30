{ pkgs, defaultCrateOverrides, makeDesktopItem, ... }:
((pkgs.callPackage ./Cargo.nix {}).rust_scripts_0_1_0 {}).override {
  crateOverrides = defaultCrateOverrides // {
    rust-scripts = attrs: {
		  postInstall = ''
			rm $out/lib/link
		  '';
		};
  };
}
