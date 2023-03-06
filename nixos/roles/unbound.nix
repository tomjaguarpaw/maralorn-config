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
        local-data = lib.concatLists (lib.concatLists (
          lib.mapAttrsToList
          (
            name: ips: (
              map (alias:
                lib.mapAttrsToList
                (type: ip: "\"${alias}.maralorn.de IN ${type} ${ip}\"")
                (lib.filterAttrs (_: addr: addr != "") ips))
              (hosts.aliases.${name} or [])
            )
          )
          hosts.tailscale
        ));
      };
    };
  };
}
