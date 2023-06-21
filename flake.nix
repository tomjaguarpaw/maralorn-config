{
  description = "maralorns configuration";

  inputs = {
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-stable.url = "nixpkgs/nixos-23.05";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos-stable";
    };
    secrets = {
      url = "git+ssh://git@hera.m-0.eu/config-secrets";
      inputs.nixpkgs.follows = "";
    };
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
    nix-output-monitor = {
      inputs = {
        flake-utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "nixos-unstable";
        pre-commit-hooks.follows = "";
      };
      url = "git+ssh://git@hera.m-0.eu/nix-output-monitor?ref=main";
    };
    nixos-mailserver = {
      inputs = {
        flake-compat.follows = "";
        utils.follows = "";
        nixpkgs.follows = "";
        nixpkgs-22_11.follows = "";
        blobs.follows = "";
      };
      url =
        "git+https://gitlab.com/simple-nixos-mailserver/nixos-mailserver.git";
    };
    home-manager = {
      url = "home-manager/release-23.05";
      inputs = { nixpkgs.follows = ""; };
    };
    hexa-nur-packages = {
      url = "github:mweinelt/nur-packages";
      inputs.nixpkgs.follows = "nixos-unstable";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        flake-compat.follows = "";
        gitignore.follows = "";
        nixpkgs-stable.follows = "";
        nixpkgs.follows = "";
      };
    };
    impermanence.url = "github:nix-community/impermanence";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixos-stable";
    };
    nixos-hardware.url = "nixos-hardware";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; }
    (import ./flake-module.nix);
}
