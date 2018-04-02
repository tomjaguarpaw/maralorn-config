{ rustPlatform, gnome3, atk, cairo, gdk_pixbuf, glib, pango, makeDesktopItem, fetchFromGitHub }:
let
   desktopItem = makeDesktopItem {
    name = "Tasktree";
    exec = "tasktree";
    icon = "tasktree";
    comment = "A taskwarrior UI";
    desktopName = "Tasktree";
    genericName = "Tasktree";
    categories = "Office;";
  };
in
with rustPlatform; buildRustPackage rec {
  name = "tasktree";
  version = "abb312f";
  src = fetchFromGitHub {
    rev = version;
    owner = "maralorn";
    repo = "tasktree";
    sha256 = "139xjvi7b62k3075b4md9hdkb1xafhhiyz2yhbb96d73j1gkqs77";
  };
  depsSha256 = "14acvigygrrqyvxra2n01vpadc3mcf8981jrggpvwfbz58jrsa7h";
  cargoSha256 = "14acvigygrrqyvxra2n01vpadc3mcf8981jrggpvwfbz58jrsa7h";

  propagatedBuildInputs = [ gnome3.gtk atk cairo gdk_pixbuf glib pango ];
  postInstall = ''
    function installIcon () {
        mkdir -p $out/share/icons/hicolor/$1/apps/
        cp icons/$1.png $out/share/icons/hicolor/$1/apps/tasktree.png
    }
    installIcon "16x16"
    installIcon "32x32"
    installIcon "64x64"

    mkdir -p $out/share/applications
    ln -s ${desktopItem}/share/applications/* $out/share/applications/
  '';
}
