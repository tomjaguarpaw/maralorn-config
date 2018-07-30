with import <unstable> {};
stdenv.mkDerivation {
  name = "habitask";
  buildInputs = [ pkgs.openssl pkgs.pkgconfig ];
}
