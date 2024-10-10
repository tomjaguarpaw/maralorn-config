{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "lazyjj";
  version = "0.4.2";
  src = fetchzip {
    url = "https://github.com/Cretezy/lazyjj/releases/download/v${version}/lazyjj-v${version}-x86_64-unknown-linux-musl.tar.gz";
    hash = "sha256-IltbHskfb5oDBZe3eFtakpTrbbP9etF1C3D/stogLjg=";
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
