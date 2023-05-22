{ pkgs, config, ... }:
let
  stateDirectory = "/var/lib/nixpkgs-bot";
  configFile = {
    server = "https://matrix.maralorn.de";
    database = "${stateDirectory}/state.sqlite";
    repo = {
      localPath = "${stateDirectory}/nixpkgs";
      owner = "NixOS";
      name = "nixpkgs";
    };
    branches = {
      "staging" = [ "staging-next" ];
      "staging-next" = [ "master" ];
      "haskell-updates" = [ "master" ];
      "master" = [ "nixos-unstable-small" "nixpkgs-unstable" ];
      "nixpkgs-unstable" = [ ];
      "nixos-unstable-small" = [ "nixos-unstable" ];
      "nixos-unstable" = [ ];
      "staging-22.11" = [ "staging-next-22.11" ];
      "staging-next-22.11" = [ "release-22.11" ];
      "release-22.11" = [ "nixos-22.11-small" ];
      "nixos-22.11-small" = [ "nixos-22.11" ];
      "nixos-22.11" = [ ];
    };
  };
in {
  systemd.services.nixpkgs-bot = {
    wantedBy = [ "multi-user.target" ];
    description = "nixpkgs-bot";
    path = [ pkgs.git ];
    serviceConfig = {
      LoadCredential = [
        "matrix_token:${config.age.secrets."nixpkgs-bot/matrix_token".path}"
        "github_token:${config.age.secrets."nixpkgs-bot/github_token".path}"
      ];
      Restart =
        "always"; # TODO: Add error handling to git querying github in nixpkgs-bot
      WorkingDirectory = "/var/lib/nixpkgs-bot";
      ExecStart = "${pkgs.nixpkgs-bot}/bin/nixpkgs-bot ${
          builtins.toFile "config.yaml" (builtins.toJSON configFile)
        }";
      DynamicUser = true;
      StateDirectory = "nixpkgs-bot";
    };
  };
}
