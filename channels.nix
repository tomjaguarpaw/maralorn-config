let
  nixos-20-09 = {
    nixpkgs-channel = "nixos-20.09";
    home-manager-channel = "home-manager-20.09";
  };
  unstable = {
    nixpkgs-channel = "nixos-unstable";
    home-manager-channel = "home-manager-master";
  };
in
rec {
  hera = nixos-20-09;
  apollo = nixos-20-09;
  cloud = hera;
  chor-cloud = hera;
  mail = hera;
}
