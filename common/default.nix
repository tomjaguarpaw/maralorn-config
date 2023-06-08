{ config, lib, ... }:
with lib; {
  config = {
    m-0.monitoring = [
      {
        host = "apollo:9100";
        name = "apollo";
        flaky = true;
      }
      {
        host = "apollo:9558";
        name = "apollo-user";
        flaky = true;
      }
      {
        host = "hera:9558";
        name = "hera-user";
        flaky = true;
      }
      {
        host = "zeus:9100";
        name = "zeus";
        flaky = true;
      }
      {
        host = "zeus:9558";
        name = "zeus-user";
        flaky = true;
      }
      {
        host = "fluffy.vpn.m-0.eu:9100";
        name = "fluffy";
      }
      {
        name = "ved server";
        host = "bach.vocalensemble-darmstadt.de:9100";
      }
      {
        name = "ved postfix";
        host = "bach.vocalensemble-darmstadt.de:9154";
      }
    ];
  };

  options.m-0 = {
    prefix = mkOption {
      default = "2a02:c207:3002:7584";
      type = types.str;
    };
    monitoring = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption { type = types.str; };
          host = mkOption { type = types.str; };
          container = mkOption {
            type = types.bool;
            default = false;
          };
          flaky = mkOption {
            type = types.bool;
            default = false;
          };
        };
      });
      default = [ ];
    };
    headscaleIPs = mkOption {
      type = types.listOf types.string;
      default = [ "100.64.7.0/24" "fd7a:115c:a1e0:77::/64" ];
    };
    virtualHosts = mkOption { type = types.attrs; };
    hosts = mkOption {
      type = types.attrs;
      default = let
        p = config.m-0.prefix;
        hera-p = "${p}::3";
        v4-p = "10.0.0";
      in {
        hera = {
          AAAA = "${p}::1";
          A = "213.136.94.190";
        };

        hera-intern = "${hera-p}:1";
        chor-cloud = "${hera-p}:b";

        hera-intern-v4 = "${v4-p}.1";
        chor-cloud-intern-v4 = "${v4-p}.3";
        # generate with:
        # (echo '{' && tailscale status -json | jq -r '.Self,.Peer[] | .DNSName[:-17] + " = { A = \"" + .TailscaleIPs[0] + "\"; AAAA = \"" + .TailscaleIPs[1] + "\";};"' && echo '}') > common/tailscale.nix
        tailscale = import ./tailscale.nix;
        publicAliases = {
          hera = [ "blog" "cloud" "git" "lists" "matrix" "rpg" ];
        };
        aliases = {
          hera = [
            "alerts"
            "analytics"
            "blog"
            "ci"
            "cloud"
            "element"
            "fdroid"
            "firefox-sync"
            "git"
            "lists"
            "matrix"
            "monitoring"
            "rpg"
            "rspamd"
            "rss"
            "stats"
            "stream"
            "syncthing-hera"
            "tasks"
            "taskserver"
          ];
          fluffy = [ "home" "syncthing-fluffy" "5e" "graphs" ];
          zeus = [ "syncthing-zeus" ];
          apollo = [ "syncthing-apollo" ];
        };
      };
    };
  };
}
