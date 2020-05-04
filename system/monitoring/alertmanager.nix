{ config, ... }: {
  services = {
    prometheus = {
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        extraFlags = [ "--data.retention 170h" ];
        configuration = {
          "global" = {
            "smtp_smarthost" = "hera.m-0.eu:587";
            "smtp_from" = "alertmanager@m-0.eu";
            "smtp_auth_username" = "alertmanager@m-0.eu";
            "smtp_auth_password" = config.m-0.private.alertmanager-mail-pw;
          };
          "route" = {
            "group_by" = [ "alertname" "alias" ];
            "group_wait" = "5m";
            "group_interval" = "30m";
            "repeat_interval" = "7d";
            "receiver" = "team-admins";
          };
          "receivers" = [{
            "name" = "team-admins";
            "email_configs" = [{
              "to" = "monitoring@maralorn.de";
              "send_resolved" = true;
            }];
          }];
        };
      };
    };
  };
}
