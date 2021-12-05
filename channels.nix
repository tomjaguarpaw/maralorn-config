let
  nixos-21-11 = {
    nixpkgs-channel = "nixos-21.11";
    home-manager-channel = "home-manager-21.11";
  };
  unstable = {
    nixpkgs-channel = "nixos-unstable";
    home-manager-channel = "home-manager-master";
  };
in
rec {
  hera = nixos-21-11;
  apollo = nixos-21-11;
  zeus = nixos-21-11;
  fluffy = nixos-21-11;
  chor-cloud = hera;
}
