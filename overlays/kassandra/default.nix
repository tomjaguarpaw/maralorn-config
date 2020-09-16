self: super: {
  kassandra = self.callPackage
    ({ lib, rustPlatform, pkgconfig, openssl, fetchgit }:
      rustPlatform.buildRustPackage {
        pname = "kassandra";
        version = "no-version";
        src = fetchgit {
          url = "git@hera.m-0.eu:kassandra";
          fetchSubmodules = true;
          sha256 = "0000000000000000000000000000000000000000000000000000000000000000";
        };
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ openssl ];
        cargoSha256 = "1ilpw4pzm8fqim29jzwlfgz1jyblragalm50vqyj1n11piapxzlk";
        CARGO_NET_GIT_FETCH_WITH_CLI = true;
      }) { };
}
