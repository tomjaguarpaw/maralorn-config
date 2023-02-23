{
  description = "maralorns configuration";

  inputs = {
    secrets = {
      url = "git+ssh://git@hera.m-0.eu/config-secrets";
      inputs.nixpkgs.follows = "";
    };
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
    # Don’t use registry so that we can override it locally.
    nixos-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
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
    nixos-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
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
  };

  outputs = inputs @ {nixos-hardware, ...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} (import ./flake-module.nix);
}
