let
  nixos-stable = {
    nixpkgs-channel = "nixos-stable";
    home-manager-channel = "home-manager-stable";
  };
in rec {
  hera = nixos-stable;
  apollo = nixos-stable;
  zeus = nixos-stable;
  fluffy = nixos-stable;
  chor-cloud = hera;
}
