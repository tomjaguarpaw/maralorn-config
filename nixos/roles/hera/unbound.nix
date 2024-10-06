{ config, ... }:
{
  services = {
    resolved.enable = false;
    unbound = {
      resolveLocalQueries = true;
      enable = true;
      settings = {
        server = {
          access-control = [
            "127.0.0.0/8 allow"
            "::1/128 allow"
            "100.64.7.0/24 allow"
            "fd7a:115c:a1e0:77::/64 allow"
          ] ++ map (range: "${range} allow") config.m-0.headscaleIPs;
          interface = [
            "lo"
            "tailscale0"
          ];
        };
      };
    };
  };
}
