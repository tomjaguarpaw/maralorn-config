final: prev:
let
  inherit (prev) lib;
in
{

  teamviewer = prev.teamviewer.overrideAttrs (old: rec {
    version = "15.51.5";
    buildInputs = old.buildInputs ++ [ final.libsForQt5.qt5.qtquickcontrols2 ];
    src = prev.fetchurl {
      url = "https://dl.tvcdn.de/download/linux/version_${lib.versions.major version}x/teamviewer_${version}_amd64.deb";
      sha256 = "sha256-AxvAzkKz9+0euvtM69mQH4xo0i+YFw3ss4hURSTRgX8=";
    };
    configurePhase = "mkdir -p $out/share/teamviewer/tv_bin/xdg-utils";
  });
}
