{
  description = "maralorns configuration";

  inputs = {
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-stable.url = "nixpkgs/nixos-23.11";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixos-stable";
    };
    nixfmt = {
      url = "github:piegamesde/nixfmt/rfc101-style";
      inputs = {
        flake-compat.follows = "";
        flake-utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "";
        nixpkgs-stable.follows = "";
      };
    };
    secrets = {
      url = "git+ssh://gitea@code.maralorn.de/maralorn/config-secrets";
      inputs.nixpkgs.follows = "";
    };
    flake-parts.inputs.nixpkgs-lib.follows = "nixos-unstable";
    nix-output-monitor = {
      inputs = {
        flake-utils.follows = "pre-commit-hooks/flake-utils";
        nixpkgs.follows = "nixos-unstable";
        pre-commit-hooks.follows = "";
      };
      url = "git+https://code.maralorn.de/maralorn/nix-output-monitor.git";
    };
    nixos-mailserver = {
      inputs = {
        flake-compat.follows = "";
        utils.follows = "";
        nixpkgs.follows = "";
        nixpkgs-23_05.follows = "";
        nixpkgs-23_11.follows = "";
        blobs.follows = "";
      };
      url = "git+https://gitlab.com/simple-nixos-mailserver/nixos-mailserver.git?ref=nixos-23.11";
    };
    home-manager = {
      url = "home-manager/release-23.11";
      inputs = {
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
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixos-stable";
    };
    nixos-hardware.url = "nixos-hardware";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (import ./flake-module.nix);
}
