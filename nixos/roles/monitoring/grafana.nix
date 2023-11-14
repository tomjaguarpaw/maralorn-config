{ pkgs, lib, ... }:
let
  heading = name: link: ''<h2><a href=\"${link}\">${name}</a></h2>'';
  badge = src: link: ''<a href=\"${link}\">\n  <img src=\"${src}\">\n</a>'';
  job = name: badge "https://ci.maralorn.de/badge/${name}.svg" "https://ci.maralorn.de/jobs/${name}";
  badges = lib.concatStringsSep "\\n" [
    (heading "ci.maralorn.de" "https://ci.maralorn.de")
    (job "test-config")
    (job "blog")

    (heading "haskell-taskwarrior" "https://hackage.haskell.org/package/taskwarrior")
    (badge "https://github.com/maralorn/haskell-taskwarrior/actions/workflows/haskell.yml/badge.svg"
      "https://github.com/maralorn/haskell-taskwarrior/actions"
    )
    (badge "https://img.shields.io/hackage-deps/v/taskwarrior.svg"
      "http://packdeps.haskellers.com/reverse/taskwarrior"
    )
    (badge "https://repology.org/badge/vertical-allrepos/haskell:taskwarrior.svg?columns=3&header="
      "https://repology.org/project/haskell:taskwarrior/versions"
    )

    (heading "nix-output-monitor" "https://github.com/maralorn/nix-output-monitor")
    (badge "https://repology.org/badge/vertical-allrepos/nix-output-monitor.svg?columns=3&header="
      "https://repology.org/project/nix-output-monitor/versions"
    )
  ];
  dashboards = pkgs.runCommand "dashboards" { } ''
    mkdir -p $out
    cp ${./grafana-dashboards}/* $out
    substituteInPlace $out/health-status.json --replace '@BADGES@' '${badges}'
  '';
in
{
  services = {
    grafana = {
      enable = true;
      settings = {
        "auth.anonymous".enabled = true;
        security.allow_embedding = true;
        users.default_theme = "dark";
        "auth.basic".enabled = false;
        dashboards.default_home_dashboard_path = "${dashboards}/health-status.json";
      };
      provision = {
        enable = true;
        datasources.settings.datasources = [
          {
            access = "proxy";
            name = "prometheus";
            type = "prometheus";
            url = "http://localhost:9090";
          }
        ];
        dashboards.settings.providers = [
          {
            name = "Static dashboards";
            options.path = dashboards;
          }
        ];
      };
    };
  };
}
