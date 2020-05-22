{ stdenv, fetchFromGitHub, pass-wayland }:
stdenv.mkDerivation rec {
  name = "${pname}-${version}";
  pname = "pass-clip";
  version = "0.2";
  src = fetchFromGitHub {
    owner = "ibizaman";
    repo = "pass-clip";
    rev = "v${version}";
    sha256 = "1jx5xl5fna9iw9325qiyn6nzfg46dxvksiq7nb52zwjb6zgv7niq";
  };
  buildPhase = "echo no build!";
  installPhase = ''
    echo "Tada!";
    PREFIX=$out make install
  '';
  propagatedBuildInputs = [ pass-wayland ];
}
