{ ... }: {
  services = {
    nginx = {
      virtualHosts."stats.maralorn.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:3000/";
      };
    };
    grafana = {
      enable = true;
      auth.anonymous.enable = true;
      users = {
        allowOrgCreate = false;
        allowSignUp = false;
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
