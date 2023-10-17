{ pkgs, lib, ... }:
let
  src = pkgs.fetchurl {
    url = "https://github.com/zgoat/goatcounter/releases/download/v2.0.4/goatcounter-v2.0.4-linux-amd64.gz";
    sha256 = "1h7hv5lk2gm3klbfbgwfa7xn31az9zmlryd8bqqq38lp5r56cpb4";
  };
  goatcounter-bin = pkgs.runCommand "goatcounter-bin" { } ''
    mkdir -p $out/bin
    gunzip -c ${src} > $out/bin/goatcounter
    chmod +x $out/bin/goatcounter
  '';
  goatcounter-token = pkgs.privateValue "" "goatcounter-token";
in
{
  environment.systemPackages = [ goatcounter-bin ];
  users = {
    users.goatcounter = {
      isSystemUser = true;
      group = "goatcounter";
    };
    groups.goatcounter = { };
  };
  services.postgresql = {
    enable = true;
    ensureUsers = [
      {
        name = "goatcounter";
        ensurePermissions = {
          "DATABASE goatcounter" = "ALL PRIVILEGES";
        };
      }
    ];
    ensureDatabases = [ "goatcounter" ];
  };
  services.nginx = {
    appendHttpConfig = lib.mkAfter ''
      log_format vcombined '$host $remote_addr - $remote_user [$time_local] '
                             '"$request" $status $body_bytes_sent '
                             '"$http_referer" "$http_user_agent"';
      access_log /run/nginx/access.log vcombined;
    '';
    virtualHosts."analytics.maralorn.de" = {
      enableACME = true;
      forceSSL = true;
      locations."/".proxyPass = "http://localhost:8081/";
    };
  };
  systemd.services = {
    goatcounter = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
      serviceConfig = {
        User = "goatcounter";
        ExecStart = "${goatcounter-bin}/bin/goatcounter serve -db 'postgresql://host=/run/postgresql dbname=goatcounter' -listen *:8081 -tls http -automigrate";
        WatchdogSignal = "SIGTERM";
        WatchdogSec = "20m";
        Restart = "always";
      };
      wantedBy = [ "multi-user.target" ];
    };
    goatcounter-feeder = {
      requires = [ "goatcounter.service" ];
      after = [ "goatcounter.service" ];
      serviceConfig = {
        User = "nginx";
        Type = "oneshot";
      };
      script = ''
        (cat /run/nginx/access.log && truncate --size 0 /run/nginx/access.log) |\
         sed 's/\([^ ]*\) \(.*"[^ ]* \/\)/\2\1\//; s/ \(\/.*\)?[^ ]* / \1 /' |\
         GOATCOUNTER_API_KEY=${goatcounter-token} ${goatcounter-bin}/bin/goatcounter import -follow -site http://localhost:8081 - -format combined \
           -exclude '!method:GET' \
           -exclude 'remote_addr:2a02:c207:3002:7584:' \
           -exclude 'remote_addr:glob:::1' \
           -exclude 'remote_addr:127.0.0.1' \
           -exclude redirect
      '';
    };
  };
  systemd.timers = {
    goatcounter-feeder = {
      timerConfig.OnCalendar = "minutely";
      wantedBy = [ "timers.target" ];
    };
  };
}
