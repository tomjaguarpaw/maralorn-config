{
  config,
  pkgs,
  lib,
  ...
}:
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
          name = mkOption {type = types.str;};
          host = mkOption {type = types.str;};
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
      default = [];
    };
    privateListenAddresses = mkOption {
      type = types.listOf types.string;
    };
    virtualHosts = mkOption {
      type = types.attrs;
    };
    hosts = mkOption {
      type = types.attrs;
      default = let
        p = config.m-0.prefix;
        hera-p = "${p}::3";
        wg-p = "${p}::100";
        v4-p = "10.0.0";
      in {
        hera = "${p}::1";
        vpn = rec {
          prefix = "fdc0:7";
          hera = "${prefix}::1";
          fluffy = "${prefix}::2";
          apollo = "${prefix}::5";
          zeus = "${prefix}::4";
          pegasus = "${prefix}::6";
        };
        hera-wg-host = "${p}::100:0:1";

        hera-v4 = "213.136.94.190";

        hera-wg = "${wg-p}:1";
        apollo-wg = "${wg-p}:2";
        zeus-wg = "${wg-p}:3";

        hera-intern = "${hera-p}:1";
        chor-cloud = "${hera-p}:b";

        hera-intern-v4 = "${v4-p}.1";
        chor-cloud-intern-v4 = "${v4-p}.3";
        # generate with:
        # (echo '{' && tailscale status -json | jq -r '.Self,.Peer[] | .DNSName[:-17] + " = { A = \"" + .TailscaleIPs[0] + "\"; AAAA = \"" + .TailscaleIPs[1] + "\";};"' && echo '}') > common/tailscale.nix
        tailscale = import ./tailscale.nix;
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
          ];
          fluffy = ["home" "syncthing-fluffy"];
          zeus = ["syncthing-zeus"];
          apollo = ["syncthing-apollo"];
        };
      };
    };
  };
}
