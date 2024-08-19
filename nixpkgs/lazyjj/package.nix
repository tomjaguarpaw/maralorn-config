{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "lazyjj";
  version = "0.3.1";
  src = fetchzip {
    url = "https://github.com/Cretezy/lazyjj/releases/download/v${version}/lazyjj-v${version}-x86_64-unknown-linux-musl.tar.gz";
    hash = "sha256-6R4W6uyq8sns8WLoJxp06xAYaJ0Zn+pZLtwhVIPobmc=";
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
