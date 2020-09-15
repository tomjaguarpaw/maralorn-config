self: super:
if super.config.allowUnfree or false then
  { }
else {
  unfree = import self.sources.nixpkgs { config.allowUnfree = true; };
  unstableUnfree = import self.sources.unstable { config.allowUnfree = true; };
  releaseUnfree = import self.sources.nixpkgs-release { config.allowUnfree = true; };
}
