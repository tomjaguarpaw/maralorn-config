{ config, ... }: {
  networking.firewall.allowedTCPPorts = [ 3478 ];

  services = {
    coturn = {
      enable = true;
      pkey = "/var/lib/acme/hera.m-0.eu/key.pem";
      cert = "/var/lib/acme/hera.m-0.eu/fullchain.pem";
      no-tcp = true;
      static-auth-secret = config.m-0.private.turn_secret;
      realm = "maralorn.de";
      use-auth-secret = true;
    };
  };

}
