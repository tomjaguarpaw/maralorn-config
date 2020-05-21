self: super:
if super.config.allowUnfree or false then
  { }
else {
  unfree = import <nixpkgs> { config.allowUnfree = true; };
}
