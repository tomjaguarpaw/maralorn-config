self: super: {
  kassandra = self.callPackage
    ({ lib, rustPlatform, pkgconfig, openssl, fetchgit }:
      rustPlatform.buildRustPackage {
        pname = "kassandra";
        version = "no-version";
        src = super.sources.kassandra;
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ openssl ];
        cargoSha256 = "0nlc09sh679vfq7n08836mnjsax2pnskm64jk3c6k0l2spina3nd";
        doCheck = false;
      })
    { };
}
