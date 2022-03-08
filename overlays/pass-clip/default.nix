self: super: {
  pass-clip = self.callPackage
  ({
    stdenv,
    fetchFromGitHub,
    pass-wayland,
  }:
    stdenv.mkDerivation rec {
      name = "${pname}-${version}";
      pname = "pass-clip";
      version = "0.3";
      src = fetchFromGitHub {
        owner = "ibizaman";
        repo = "pass-clip";
        rev = "v${version}";
        sha256 = "0myyyw0azci95jp8lwh6bm7xi171fv9vg8j6lzf9m9n7282cmy33";
      };
      dontBuild = true;
      installPhase = "PREFIX=$out make install";
      propagatedBuildInputs = [pass-wayland];
    })
  {};
}
