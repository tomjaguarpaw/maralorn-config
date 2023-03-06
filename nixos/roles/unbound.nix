{
  lib,
  config,
  ...
}: let
  inherit (config.m-0) hosts;
in {
  services.unbound = {
    resolveLocalQueries = false;
    enable = true;
    settings = {
      server = {
        access-control =
          [
            "127.0.0.0/8 allow"
            "::1/128 allow"
            "100.64.7.0/24 allow"
            "fd7a:115c:a1e0:77::/64 allow"
          ]
          ++ map (range: "${range} allow") config.m-0.headscaleIPs;
        interface = [
          "lo"
          "m0wire"
          "tailscale0"
        ];
      };
      auth-zone = let
        name = "maralorn.de";
      in [
        {
          inherit name;
          zonefile = builtins.toFile "${name}-zonfile" ''
            $ORIGIN ${name}.
            $TTL 60
            @ IN SOA hera.${name}. hostmaster.${name}. (
              2001062501 ; serial
              21600      ; refresh after 6 hours
              3600       ; retry after 1 hour
              604800     ; expire after 1 week
              86400 )    ; minimum TTL of 1 day
              IN MX 10 hera.m-0.eu
              IN NS hera.${name}.

            headscale IN CNAME hera.m-0.eu.
            ${
              lib.concatStringsSep "\n"
              (lib.concatLists (lib.mapAttrsToList
                  (
                    name: ips:
                      lib.mapAttrsToList
                      (type: ip: "${name} IN ${type} ${ip}")
                      (lib.filterAttrs (_: addr: addr != "") ips)
                  )
                  hosts.tailscale)
                ++ lib.concatLists (lib.mapAttrsToList
                  (to: map (from: "${from} IN CNAME ${to}"))
                  config.m-0.hosts.aliases))
            }
          '';
        }
      ];
    };
  };
}
