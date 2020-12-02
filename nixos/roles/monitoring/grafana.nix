{ ... }: {
  services = {
    grafana = {
      enable = true;
      auth.anonymous.enable = true;
      extraOptions = {
        AUTH_BASIC_ENABLED = "false";
        dashboards.default_home_dashboard_path =
          "${./grafana-dashboards}/health-status.json";
      };
      provision = {
        enable = true;
        datasources = [{
          access = "proxy";
          name = "prometheus";
          type = "prometheus";
          url = "http://localhost:9090";
        }];
        dashboards = [{
          name = "Static dashboards";
          options.path = ./grafana-dashboards;
        }];
      };
    };
  };
}
