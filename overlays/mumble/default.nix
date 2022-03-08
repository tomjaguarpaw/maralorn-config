final: prev: {
  inherit
    (final.callPackage ./package.nix {
      avahi = final.avahi-compat;
      jackSupport = false;
      speechdSupport = false;
      pulseSupport = true;
    })
    mumble
    ;
}
