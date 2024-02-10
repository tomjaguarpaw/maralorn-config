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
      };
      headscaleIPs = mkOption {
        type = types.listOf types.str;
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
            athene = [ "home" ];
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
              "analytics"
              "blog"
              "cloud"
              "git"
              "lists"
              "matrix"
              "rpg"
              "rspamd"
              "rss"
              "stats"
              "code"
            ];
            athene = [
              "firefox-sync"
              "home"
              "monitoring"
              "alerts"
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
