{ pkgs, fetchFromGitHub, defaultCrateOverrides, makeDesktopItem, atk, pango
, gnome3, cairo, gdk_pixbuf, glib, ... }:
((pkgs.callPackage ./Cargo.nix { }).tasktree_0_1_0 { }).override {
  crateOverrides = defaultCrateOverrides // {
    atk-sys = attr: { buildInputs = [ atk ]; };
    pango-sys = attr: { buildInputs = [ pango glib ]; };
    gio = attr: { buildInputs = [ glib ]; };
    gdk-sys = attr: { buildInputs = [ gdk_pixbuf glib cairo pango ]; };
    gtk-sys = attr: {
      buildInputs = [ gdk_pixbuf glib cairo pango atk gnome3.gtk ];
    };
    gdk = attr: { buildInputs = [ cairo gnome3.gtk gdk_pixbuf pango ]; };
    gtk = attr: { buildInputs = [ cairo atk gnome3.gtk gdk_pixbuf pango ]; };
    tasktree = attrs:
      let
        desktopItem = makeDesktopItem {
          name = "tasktree";
          exec = "tasktree";
          icon = "tasktree";
          comment = "A taskwarrior UI";
          desktopName = "Tasktree";
          genericName = "Tasktree";
          categories = "Office;";
        };
        version = "abb312f";
      in {
        src = fetchFromGitHub {
          rev = version;
          owner = "maralorn";
          repo = "tasktree";
          sha256 = "139xjvi7b62k3075b4md9hdkb1xafhhiyz2yhbb96d73j1gkqs77";
        };
        depsSha256 = "14acvigygrrqyvxra2n01vpadc3mcf8981jrggpvwfbz58jrsa7h";
        cargoSha256 = "14acvigygrrqyvxra2n01vpadc3mcf8981jrggpvwfbz58jrsa7h";

        postInstall = ''
          mkdir -p $out/share/applications
          ln -s ${desktopItem}/share/applications/* $out/share/applications/
          rm $out/lib/link
        '';
        buildInputs = [ cairo atk gnome3.gtk gdk_pixbuf pango ];
      };
  };
}
