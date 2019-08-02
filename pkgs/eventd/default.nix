{ fetchFromGitHub, stdenv, ninja, meson, pkgconfig, glib, cairo, gdk_pixbuf
, glib_networking, pango, libudev, xorg, libxslt, docbook_xml_xslt, git, libuuid
, dbus, libsoup, docbook_xml_dtd_45, docbook5_xsl, gettext, autoconf, libtool
, utillinux, libxkbcommon }:
stdenv.mkDerivation rec {
  name = "eventd";
  version = "d7c7ba59aa6b225b3e2b8aebdd853137c05d8445";
  src = fetchFromGitHub {
    owner = "sardemff7";
    repo = "eventd";
    rev = version;
    sha256 = "0b32dwx2ngdm57pdhq1wr7h7i12lnhnm81bs9bbs60dqc01ry6jq";
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
    dbus
  ];
  preConfigure = ''
    export mesonFlags="-Dsystemd=true -Dintrospection=false -Dnd-wayland=false -Dim=false -Dsound=false -Ddbussessionservicedir=$prefix/share/dbus-1/services -Dsystemduserunitdir=$prefix/lib/systemd/user -Dsystemdsystemunitdir=$prefix/lib/systemd/system"
  '';
}
