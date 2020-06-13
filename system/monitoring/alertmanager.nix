{ config, ... }: {
  services = {
    prometheus = {
      alertmanagers =
        [{ static_configs = [{ targets = [ "localhost:9093" ]; }]; }];
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        extraFlags = [ "--data.retention 170h" ];
        configuration = {
          global = {
            smtp_smarthost = "hera.m-0.eu:587";
            smtp_from = "alertmanager@m-0.eu";
            smtp_auth_username = "alertmanager@m-0.eu";
            smtp_auth_password = config.m-0.private.alertmanager-mail-pw;
          };
          route = {
            group_by = [ ];
            group_wait = "60s";
            group_interval = "5m";
            repeat_interval = "168h";
            receiver = "alerts";
          };
          receivers = [{
            name = "alerts";
            webhook_configs = [{
              url =
                "${config.services.go-neb.baseUrl}:4050/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
            }];
          }];
        };
      };
    };
  };
}
