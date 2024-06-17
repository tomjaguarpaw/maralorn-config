{ stdenv, fetchzip }:

stdenv.mkDerivation {
  name = "lazyjj";
  src = fetchzip {
    url = "https://github.com/Cretezy/lazyjj/releases/download/v0.2.1/lazyjj-v0.2.1-x86_64-unknown-linux-musl.tar.gz";
    hash = "sha256-yMfGWuzsl94elFxRvGaLA61KBopBnBT3j5pxbCrKl0w=";
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
