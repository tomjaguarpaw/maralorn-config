self: super:
let
  unfree = import self.sources."${self.nixpkgs-channel}" { config.allowUnfree = true; };
  unstableUnfree =
    import self.sources.nixos-unstable { config.allowUnfree = true; };
in {
  inherit (unfree) discord factorio steam zoom-us skypeforlinux google-chrome minecraft;
}
