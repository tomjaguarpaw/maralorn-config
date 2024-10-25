{ config, lib, ... }:
let
  inherit (config.m-0) hosts;
  inherit (config.networking) hostName;
in
{
  networking = {
    resolvconf.dnsExtensionMechanism = false; # this breaks dnssec but is necessary for certain bad-behaved hotspots
    firewall = {
      enable = true; # It’s the default, but better make sure.
      allowPing = true;
    };
    nftables.enable = true; # Uses firewall variables since 23.05
    useNetworkd = true;
    useDHCP = false; # enabled per interface
    hosts =
      let
        replaceLocalHost =
          host: ips:
          if host == hostName then
            {
              AAAA = "::1";
              A = "127.0.0.1";
            }
          else
            ips;
      in
      lib.zipAttrs (
        lib.mapAttrsToList (host: ips: {
          ${(replaceLocalHost host ips).AAAA or null} = "${host} ${host}.m-0.eu";
          ${(replaceLocalHost host ips).A or null} = "${host} ${host}.m-0.eu";
        }) config.m-0.hosts
        ++ lib.mapAttrsToList (
          host: ips:
          let
            mkHost = name: "${name} ${name}.maralorn.de";
            name = "${host} ${host}.vpn.m-0.eu ${
              lib.concatMapStringsSep " " mkHost config.m-0.hosts.aliases.${host} or [ ]
            }";
          in
          {
            ${(replaceLocalHost host ips).AAAA} = name;
            ${(replaceLocalHost host ips).A} = name;
          }
        ) config.m-0.hosts.tailscale
      );
  };
  systemd.network.wait-online.anyInterface = true;
  m-0.virtualHosts = lib.genAttrs (hosts.aliases.${hostName} or [ ]) (name: "${name}.maralorn.de");
}
