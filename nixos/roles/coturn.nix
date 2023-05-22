{ config, pkgs, ... }:
let
  fqdn = "${config.networking.hostName}.${config.networking.domain}";
  key_dir = config.security.acme.certs."${fqdn}".directory;
in {
  users.users.turnserver.extraGroups = [ "nginx" ]; # For read access to certs;
  networking.firewall = let
    range = [{
      from = config.services.coturn.min-port;
      to = config.services.coturn.max-port;
    }];
    ports = [
      config.services.coturn.listening-port
      config.services.coturn.alt-listening-port
      config.services.coturn.tls-listening-port
      config.services.coturn.alt-tls-listening-port
    ];
  in {
    allowedUDPPortRanges = range;
    allowedTCPPortRanges = range;
    allowedTCPPorts = ports;
    allowedUDPPorts = ports;
  };
  security.acme.certs = {
    "${fqdn}".postRun = "systemctl restart coturn.service";
  };
  services = {
    coturn = {
      enable = true;
      use-auth-secret = true;
      no-cli = true;
      no-tcp-relay = true;
      min-port = 52000;
      max-port = 52100;
      pkey = "${key_dir}/key.pem";
      cert = "${key_dir}/fullchain.pem";
      static-auth-secret = (pkgs.privateValue { turn_shared_secret = ""; }
        "matrix/server-secrets").turn_shared_secret;
      realm = fqdn;
      listening-ips = [ config.m-0.hosts.hera config.m-0.hosts.hera-v4 ];
      extraConfig = ''
        fingerprint

        denied-peer-ip=10.0.0.0-10.255.255.255
        denied-peer-ip=192.168.0.0-192.168.255.255
        denied-peer-ip=172.16.0.0-172.31.255.255
      '';
    };
  };
}
