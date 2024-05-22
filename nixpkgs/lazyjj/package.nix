{ stdenv, fetchzip }:

stdenv.mkDerivation {
  name = "lazyjj";
  src = fetchzip {
    url = "https://github.com/Cretezy/lazyjj/releases/download/v0.1.1/lazyjj-v0.1.1-x86_64-unknown-linux-musl.tar.gz";
    hash = "sha256-c9c/syZs5JX3v8qnB5h+Al+AFPLWtKoSp5lp3WWuYgs=";
  };
  phases = [
    "unpackPhase"
    "installPhase"
  ];
  installPhase = ''
    mkdir -p $out/bin
    mv lazyjj $out/bin

  '';
}
