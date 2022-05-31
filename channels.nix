let
  nixos-21-11 = {
    nixpkgs-channel = "nixos-21.11";
    home-manager-channel = "home-manager-21.11";
  };
  nixos-22-05 = {
    nixpkgs-channel = "nixos-22.05";
    home-manager-channel = "home-manager-22.05";
  };
in rec {
  hera = nixos-21-11;
  apollo = nixos-22-05;
  zeus = nixos-21-11;
  fluffy = nixos-21-11;
  chor-cloud = hera;
}
