{ config, lib, ... }:
{

  options.m-0 =
    let
      inherit (lib) mkOption types;
    in
    {
      prefix = mkOption {
        default = "2a02:c207:3002:7584";
        type = types.str;
        description = "External ipv6 prefix";
      };
      hyprland = mkOption {
        default = true;
        type = types.bool;
      };
      headscaleIPs = mkOption {
        type = types.listOf types.str;
        default = [
          "100.64.7.0/24"
          "fd7a:115c:a1e0:77::/64"
        ];
        description = "Internal headscale ips";
      };
      virtualHosts = mkOption {
        type = types.attrs;
        description = "available aliases on this machine";
      };
      hosts = mkOption {
        type = types.attrs;
        description = "hosts";
        default = {
          hera = {
            AAAA = "${config.m-0.prefix}::1";
            A = "213.136.94.190";
          };

          # generate with:
          # (echo '{' && tailscale status -json | jq -r '.Self,.Peer[] | .DNSName[:-11] + " = { A = \"" + .TailscaleIPs[0] + "\"; AAAA = \"" + .TailscaleIPs[1] + "\";};"' | sort && echo '}') > common/tailscale.nix
          tailscale = import ./tailscale.nix;
          publicAliases = {
            athene = [ "home" ];
            hera = [
              "blog"
              "cloud"
              "code"
              "lists"
              "matrix"
              "rpg"
            ];
          };
          aliases = {
            hera = [
              "analytics"
              "blog"
              "cloud"
              "lists"
              "matrix"
              "rpg"
              "rspamd"
              "code"
            ];
            athene = [
              "firefox-sync"
              "rss"
              "home"
              "monitoring"
              "notes"
              "grist"
              "alerts"
              "cache"
              "syncthing-athene"
              "5e"
              "graphs"
              "athene.id"
            ];
            zeus = [ "syncthing-zeus" ];
            hephaistos = [ "syncthing-hephaistos" ];
          };
        };
      };
    };
}
