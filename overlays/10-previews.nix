self: super:
let
  unstable = import self.sources.unstable { };
  nixpkgs-master = import self.sources.nixpkgs-master { };
in {
  inherit (unstable) gomuks;
}
