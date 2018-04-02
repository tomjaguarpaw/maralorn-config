{ fetchFromGitHub, stdenv, ninja, meson, pkgconfig, glib, cairo, gdk_pixbuf, glib_networking, pango, libudev, xorg, libxslt, docbook_xml_xslt, git, libuuid, dbus, libsoup, docbook_xml_dtd_45, docbook5_xsl, gettext, autoconf, libtool, utillinux, libxkbcommon }:
stdenv.mkDerivation rec {
  name = "eventd";
  version = "279d3c3";
  src = fetchFromGitHub {
    owner = "sardemff7";
    repo = "eventd";
    rev = version;
    sha256 = "162gr3agmjn6d0wdj3lixv8qfvgfm9qg3wphbvwywdp4qcwvnjz8";
    fetchSubmodules = true;
  };
  patchPhase = ''
    sed s/0.44.1/0.43.0/ -i meson.build
  '';
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
    dbus
  ];
  preConfigure = ''
    export mesonFlags="-Denable-systemd=true -Denable-introspection=false -Denable-nd-wayland=false -Denable-im=false -Denable-sound=false -Ddbussessionservicedir=$prefix/share/dbus-1/services -Dsystemduserunitdir=$prefix/lib/systemd/user -Dsystemdsystemunitdir=$prefix/lib/systemd/system"
  '';
}
