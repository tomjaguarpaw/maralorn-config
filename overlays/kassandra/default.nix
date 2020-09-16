self: super: {
  kassandra = self.callPackage
    ({ lib, rustPlatform, pkgconfig, openssl, fetchgit }:
      rustPlatform.buildRustPackage {
        pname = "kassandra";
        version = "no-version";
        src = super.sources.kassandra;
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ openssl ];
        cargoSha256 = "0aqyaz4kzp93l8mzqjgcnamh0xyhv4g3rv3dfvlkd2w55bz9fmj5";
      }) { };
}
