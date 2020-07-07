self: super:
if super.config.allowUnfree or false then
  { }
else {
  unfree = import self.sources.nixpkgs { config.allowUnfree = true; };
  unstableUnfree = import self.sources.unstable { config.allowUnfree = true; };
}
