{
  pkgs,
  config,
  lib,
  ...
}:
let
  stateDirectory = "/var/lib/merge-bot";
  configFile = {
    server = "https://code.maralorn.de";
    repos = [ "maralorn/config" ];
    botname = "marabot";
  };
in
{
  environment.persistence.snapshoted.directories = [ "/var/lib/private/merge-bot" ];

  systemd = {
    tmpfiles.settings.fix-var-lib-private."/disk/persist/var/lib/private".Z.mode = "0700";
    services.merge-bot = {
      wantedBy = [ "multi-user.target" ];
      description = "merge-bot";
      path = [ pkgs.git ];
      serviceConfig = {
        Restart = "always";
        RestartSec = "15s";
        LoadCredential = [ "merge_bot_token:${config.age.secrets."merge-bot/forgejo_token".path}" ];
        WorkingDirectory = stateDirectory;
        ExecStart = "${lib.getExe pkgs.merge-bot} ${builtins.toFile "config.json" (builtins.toJSON configFile)}";
        DynamicUser = true;
        StateDirectory = "merge-bot";
      };
      unitConfig.StartLimitIntervalSec = "90s";
    };
  };
}
