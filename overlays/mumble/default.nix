final: prev: {
  inherit (final.unstable.callPackage ./package.nix {
    avahi = final.unstable.avahi-compat;
    jackSupport = false;
    speechdSupport = false;
    pulseSupport = true;
  }) mumble;
}
