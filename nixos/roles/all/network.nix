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
    hosts = lib.zipAttrs (
      lib.mapAttrsToList (
        host: ip:
        if builtins.typeOf ip == "set" then
          {
            ${ip.AAAA or null} = "${host} ${host}.m-0.eu";
            ${ip.A or null} = "${host} ${host}.m-0.eu";
          }
        else
          { "${ip}" = "${host} ${host}.m-0.eu"; }
      ) config.m-0.hosts
      ++ lib.mapAttrsToList (
        host: ips:
        let
          mkHost = name: "${name} ${name}.maralorn.de";
          name = "${host} ${host}.vpn.m-0.eu ${
            lib.concatMapStringsSep " " mkHost config.m-0.hosts.aliases.${host} or [ ]
          }";
        in
        {
          ${ips.AAAA} = name;
          ${ips.A} = name;
        }
      ) config.m-0.hosts.tailscale
    );
  };
  systemd.network.wait-online.anyInterface = true;
  m-0.virtualHosts = lib.genAttrs (hosts.aliases.${hostName} or [ ]) (name: "${name}.maralorn.de");
}
