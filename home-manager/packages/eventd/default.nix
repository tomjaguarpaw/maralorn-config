{ fetchFromGitHub, intltool, stdenv, ninja, meson, pkgconfig, glib, cairo, gdk_pixbuf, glib_networking, pango, libudev, xorg, libxslt, docbook_xml_xslt, git, libuuid, dbus, libsoup, docbook_xml_dtd_45, docbook5_xsl, gettext, autoconf, libtool, utillinux, libxkbcommon }:
stdenv.mkDerivation rec {
  name = "eventd";
  version = "v0.24.1";
  src = fetchFromGitHub {
    owner = "sardemff7";
    repo = "eventd";
    rev = version;
    sha256 = "1xjkary1lq8yk1nqw039hrxwax1h0l0vi1dmcyqrgvzb1igsfa6y";
    fetchSubmodules = true;
  };
  buildInputs = [
    ninja
    meson
    pkgconfig
    glib
    cairo
    gdk_pixbuf
    glib_networking
    pango
    libudev
    xorg.libxcb
    xorg.xcbutil
    xorg.xcbutilwm
    libxkbcommon
    libxslt
    docbook_xml_xslt
    docbook_xml_dtd_45
    libuuid
    intltool
    dbus
  ];
  preConfigure = ''
    export mesonFlags="-Dsystemd=true -Dintrospection=false -Dnd-wayland=false -Dim=false -Dsound=false -Ddbussessionservicedir=$prefix/share/dbus-1/services -Dsystemduserunitdir=$prefix/lib/systemd/user -Dsystemdsystemunitdir=$prefix/lib/systemd/system"
  '';
}
