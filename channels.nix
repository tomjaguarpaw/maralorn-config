let
  nixos-21-05 = {
    nixpkgs-channel = "nixos-21.05";
    home-manager-channel = "home-manager-21.05";
  };
  unstable = {
    nixpkgs-channel = "nixos-unstable";
    home-manager-channel = "home-manager-master";
  };
in
rec {
  hera = nixos-21-05;
  apollo = nixos-21-05;
  zeus = nixos-21-05;
  chor-cloud = hera;
}
