{ pkgs, lib, ... }:
let
  heading = name: link: ''<h2><a href=\"${link}\">${name}</a></h2>'';
  badge = src: link: ''<a href=\"${link}\">\n  <img src=\"${src}\">\n</a>'';
  job = name:
    badge "https://ci.maralorn.de/badge/${name}.svg"
    "https://ci.maralorn.de/jobs/${name}";
  badges = lib.concatStringsSep "\\n" [
    (heading "ci.maralorn.de" "https://ci.maralorn.de")
    (job "kassandra")
    (job "test-config")
    (job "bump-and-test-config")
    (heading "haskell-taskwarrior"
      "https://hackage.haskell.org/package/taskwarrior")
    (badge "https://img.shields.io/hackage/v/taskwarrior.svg"
      "https://hackage.haskell.org/package/taskwarrior")
    (badge
      "https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fmaralorn%2Fhaskell-taskwarrior%2Fbadge%3Fref%3Dmaster"
      "https://actions-badge.atrox.dev/maralorn/haskell-taskwarrior/goto?ref=master")
    (badge "https://img.shields.io/hackage-deps/v/taskwarrior.svg"
      "http://packdeps.haskellers.com/reverse/taskwarrior")
  ];
  dashboards = pkgs.runCommand "dashboards" { } ''
    mkdir -p $out
    cp ${./grafana-dashboards}/* $out
    substituteInPlace $out/health-status.json --replace '@BADGES@' '${badges}' \
  '';
in {

  services = {
    grafana = {
      enable = true;
      auth.anonymous.enable = true;
      extraOptions = {
        AUTH_BASIC_ENABLED = "false";
        DASHBOARDS_DEFAULT_HOME_DASHBOARD_PATH =
          "${dashboards}/health-status.json";
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
          options.path = dashboards;
        }];
      };
    };
  };
}
