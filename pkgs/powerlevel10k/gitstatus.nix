{ stdenv, fetchFromGitHub, makeWrapper, libgit2 }:

stdenv.mkDerivation {
  name = "gitstatus";
  version = "2019-05-04";

  src = fetchFromGitHub {
    owner = "romkatv";
    repo = "gitstatus";
    rev = "9c791f93c23c04dadfab8b4309a863b62a6ee424";
    sha256 = "0jbdrgl62x6j920h72n2q6304fb6gdgnmllpv4aa76m13b9qhgq6";
  };

  buildInputs = [ libgit2 ];

  installPhase = ''
    mkdir -p $out/bin
    cp gitstatusd $out/bin/
  '';
}
