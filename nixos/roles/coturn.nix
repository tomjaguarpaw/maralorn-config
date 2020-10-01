{ config, pkgs, ... }: {
  networking.firewall.allowedTCPPorts = [ 3478 ];

  services = {
    coturn = {
      enable = true;
      pkey = "/var/lib/acme/hera.m-0.eu/key.pem";
      cert = "/var/lib/acme/hera.m-0.eu/fullchain.pem";
      no-tcp = true;
      static-auth-secret = (pkgs.privateValue { turn_shared_secret = ""; }
        "matrix/server-secrets").turn_shared_secret;
      realm = "maralorn.de";
      use-auth-secret = true;
    };
  };

}
