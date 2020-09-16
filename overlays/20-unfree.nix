self: super:
let
  unfree = import self.sources.nixpkgs { config.allowUnfree = true; };
  unstableUnfree = import self.sources.unstable { config.allowUnfree = true; };
  releaseUnfree =
    import self.sources.nixpkgs-release { config.allowUnfree = true; };
in { inherit (unfree) discord factorio steam zoom-us skypeforlinux google-chrome; }
