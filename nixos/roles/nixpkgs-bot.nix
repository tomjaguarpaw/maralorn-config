{pkgs, ...}: let
  stateDirectory = "/var/lib/nixpkgs-bot";
  config = {
    server = "https://matrix.maralorn.de";
    database = "${stateDirectory}/state.sqlite";
    repo = {
      localPath = "${stateDirectory}/nixpkgs";
      owner = "NixOS";
      name = "nixpkgs";
    };
    branches = {
      "staging" = ["staging-next"];
      "staging-next" = ["master"];
      "haskell-updates" = ["master"];
      "master" = ["nixos-unstable-small" "nixpkgs-unstable"];
      "nixpkgs-unstable" = [];
      "nixos-unstable-small" = ["nixos-unstable"];
      "nixos-unstable" = [];
      "staging-22.05" = ["staging-next-22.05"];
      "staging-next-22.05" = ["release-22.05"];
      "release-22.05" = ["nixos-22.05-small"];
      "nixos-22.05-small" = ["nixos-22.05"];
      "nixos-22.05" = [];
    };
  };
in {
  systemd.services.nixpkgs-bot = {
    wantedBy = ["multi-user.target"];
    description = "nixpkgs-bot";
    path = [pkgs.git];
    serviceConfig = {
      LoadCredential = ["matrix_token:${pkgs.privateFile "nixpkgs-bot/matrix_token"}" "github_token:${pkgs.privateFile "nixpkgs-bot/github_token"}"];
      WorkingDirectory = "/var/lib/nixpkgs-bot";
      ExecStart = "${pkgs.nixpkgs-bot}/bin/nixpkgs-bot ${builtins.toFile "config.yaml" (builtins.toJSON config)}";
      DynamicUser = true;
      StateDirectory = "nixpkgs-bot";
    };
  };
}
