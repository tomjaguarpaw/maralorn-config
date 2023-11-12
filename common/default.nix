{ config, lib, ... }:
{
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
        host = "athene.vpn.m-0.eu:9100";
        name = "athene";
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

  options.m-0 =
    let
      inherit (lib) mkOption types;
    in
    {
      prefix = mkOption {
        default = "2a02:c207:3002:7584";
        type = types.str;
      };
      monitoring = mkOption {
        type = types.listOf (
          types.submodule {
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
          }
        );
        default = [ ];
      };
      headscaleIPs = mkOption {
        type = types.listOf types.string;
        default = [
          "100.64.7.0/24"
          "fd7a:115c:a1e0:77::/64"
        ];
      };
      virtualHosts = mkOption { type = types.attrs; };
      hosts = mkOption {
        type = types.attrs;
        default = {
          hera = {
            AAAA = "${config.m-0.prefix}::1";
            A = "213.136.94.190";
          };

          # generate with:
          # (echo '{' && tailscale status -json | jq -r '.Self,.Peer[] | .DNSName[:-17] + " = { A = \"" + .TailscaleIPs[0] + "\"; AAAA = \"" + .TailscaleIPs[1] + "\";};"' && echo '}') > common/tailscale.nix
          tailscale = import ./tailscale.nix;
          publicAliases = {
            hera = [
              "blog"
              "cloud"
              "git"
              "code"
              "lists"
              "matrix"
              "rpg"
            ];
          };
          aliases = {
            hera = [
              "alerts"
              "analytics"
              "blog"
              "ci"
              "cloud"
              "git"
              "lists"
              "matrix"
              "monitoring"
              "rpg"
              "rspamd"
              "rss"
              "stats"
              "taskserver"
              "code"
            ];
            athene = [
              "firefox-sync"
              "home"
              "cache"
              "syncthing-athene"
              "5e"
              "graphs"
            ];
            zeus = [ "syncthing-zeus" ];
            apollo = [ "syncthing-apollo" ];
            hephaistos = [ "syncthing-hephaistos" ];
          };
        };
      };
    };
}
