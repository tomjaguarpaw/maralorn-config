{config, ...}:
{
  services = {
    prometheus = {
      alertmanagers = [{static_configs = [{targets = ["localhost:9093"];}];}];
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        extraFlags = ["--data.retention 170h"];
        configuration = {
          route = {
            group_by = ["alert_type"];
            group_wait = "60s";
            group_interval = "5m";
            repeat_interval = "168h";
            receiver = "alerts";
          };
          receivers = [
            {
              name = "alerts";
              webhook_configs = [
                {
                  send_resolved = false;
                  url = "${config.services.go-neb.baseUrl}:4050/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
                }
              ];
            }
          ];
        };
      };
    };
  };
}
