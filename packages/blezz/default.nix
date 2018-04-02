{ fetchFromGitHub, stdenv, libxcb, xcbutil, xcbutilwm  }:
stdenv.mkDerivation rec {
  name = "blezz";
  version = "8643772";
  src = fetchFromGitHub {
    owner = "Blezzing";
    repo = "blezz";
    rev = version;
    sha256 = "0kgbzkx49018wxli4agf2vwyq9lnin1qvh1hs6wr59384hmvrbnv";
  };
  buildInputs = [ libxcb xcbutil xcbutilwm ];
   patchPhase = ''
    grep -v /usr/lib makefile > makefile1
    mv makefile1 makefile
    sed s,/usr/bin/,$prefix/usr/bin/, -i makefile
    mkdir -p $prefix/usr/bin
  '';
}
