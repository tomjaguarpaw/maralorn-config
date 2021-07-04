self: super: {
  kassandra = self.callPackage
    ({ lib, rustPlatform, pkgconfig, openssl, fetchgit }:
      rustPlatform.buildRustPackage {
        pname = "kassandra";
        version = "no-version";
        src = super.sources.kassandra;
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ openssl ];
        cargoSha256 = "068x6f6bxb36vxskr75g5d5yfhpc1fmnw5hv13mk3v1ibny5hrka";
        doCheck = false;
      })
    { };
}
