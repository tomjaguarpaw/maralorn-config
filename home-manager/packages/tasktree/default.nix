{ pkgs, defaultCrateOverrides, makeDesktopItem, atk , pango, gnome3, cairo, gdk_pixbuf, glib, ... }:
((pkgs.callPackage ./Cargo.nix {}).tasktree_0_1_0 {}).override {
  crateOverrides = defaultCrateOverrides // {
    atk-sys = attr: { buildInputs = [ atk ]; };
    pango-sys = attr: { buildInputs = [ pango glib ]; };
    gio = attr: { buildInputs = [ glib ]; };
    gdk-sys = attr: { buildInputs = [ gdk_pixbuf glib cairo pango ]; };
    gtk-sys = attr: { buildInputs = [ gdk_pixbuf glib cairo pango atk gnome3.gtk ]; };
    gdk = attr: { buildInputs = [ cairo gnome3.gtk gdk_pixbuf pango ]; };
    gtk = attr: { buildInputs = [ cairo atk gnome3.gtk gdk_pixbuf pango ]; };
    tasktree = attrs:
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
		in {

		  postInstall = ''
			mkdir -p $out/share/applications
			ln -s ${desktopItem}/share/applications/* $out/share/applications/
			rm $out/lib/link
		  '';
		};
  };
}


  #propagatedBuildInputs = [ gnome3.gtk atk cairo gdk_pixbuf glib pango ];
  #postInstall = ''
    #function installIcon () {
        #mkdir -p $out/share/icons/hicolor/$1/apps/
        #cp icons/$1.png $out/share/icons/hicolor/$1/apps/tasktree.png
    #}
    #installIcon "16x16"
    #installIcon "32x32"
    #installIcon "64x64"

    #mkdir -p $out/share/applications
    #ln -s ${desktopItem}/share/applications/* $out/share/applications/
  #'';
  #doCheck = false;
#}
