{
  description = "maralorns configuration";

  inputs = {
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-stable.url = "nixpkgs/nixos-22.11";
    secrets = {
      url = "git+ssh://git@hera.m-0.eu/config-secrets";
      inputs.nixpkgs.follows = "";
    };
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
    emanote = {
      url = "github:srid/emanote";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "";
      };
    };
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
      url = "git+https://gitlab.com/simple-nixos-mailserver/nixos-mailserver.git";
    };
    home-manager = {
      url = "home-manager/release-22.11";
      inputs = {
        utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "";
      };
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
  };

  outputs = inputs @ {nixos-hardware, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} (import ./flake-module.nix);
}
