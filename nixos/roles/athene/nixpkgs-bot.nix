{
  pkgs,
  config,
  lib,
  ...
}:
let
  stateDirectory = "/var/lib/nixpkgs-bot";
  releases = [
    "24.11"
    "24.05"
  ];
  configFile = {
    server = "https://matrix.maralorn.de";
    database = "${stateDirectory}/state.sqlite";
    repo = {
      localPath = "${stateDirectory}/nixpkgs";
      owner = "NixOS";
      name = "nixpkgs";
    };
    branches = builtins.zipAttrsWith (_: lib.flatten) (
      [
        {
          "staging" = [ "staging-next" ];
          "staging-next" = [ "master" ];
          "haskell-updates" = [ "master" ];
          "master" = [
            "nixos-unstable-small"
            "nixpkgs-unstable"
          ];
          "nixpkgs-unstable" = [ ];
          "nixos-unstable-small" = [ "nixos-unstable" ];
          "nixos-unstable" = [ ];
        }
      ]
      ++ map (release: {
        "staging-${release}" = [ "staging-next-${release}" ];
        "staging-next-${release}" = [ "release-${release}" ];
        "release-${release}" = [ "nixos-${release}-small" ];
        "nixos-${release}-small" = [ "nixos-${release}" ];
        "nixos-${release}" = [ ];
      }) releases
    );
  };
in
{
  environment.persistence.snapshoted.directories = [ "/var/lib/private/nixpkgs-bot" ];

  systemd = {
    tmpfiles.settings.fix-var-lib-private."/disk/persist/var/lib/private".Z.mode = "0700";
    services.nixpkgs-bot = {
      wantedBy = [ "multi-user.target" ];
      description = "nixpkgs-bot";
      path = [ pkgs.git ];
      serviceConfig = {
        LoadCredential = [
          "matrix_token:${config.age.secrets."nixpkgs-bot/matrix_token".path}"
          "github_token:${config.age.secrets."nixpkgs-bot/github_token".path}"
        ];
        Restart = "always"; # TODO: Add error handling to git querying github in nixpkgs-bot
        RestartSec = "15s";
        WorkingDirectory = "/var/lib/nixpkgs-bot";
        ExecStart = "${lib.getExe pkgs.nixpkgs-bot} ${builtins.toFile "config.yaml" (builtins.toJSON configFile)}";
        DynamicUser = true;
        StateDirectory = "nixpkgs-bot";
      };
      unitConfig.StartLimitIntervalSec = "90s";
    };
  };
}
