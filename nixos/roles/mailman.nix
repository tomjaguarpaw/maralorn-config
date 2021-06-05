{ ... }:
let
  hostname = "lists.maralorn.de";
  admin = "admin@maralorn.de";
in
{
  services = {
    mailman = {
      enable = true;
      webHosts = [ hostname ];
      serve.enable = true;
      enablePostfix = true;
      siteOwner = admin;
    };
    postfix = {
      relayDomains = [ "hash:/var/lib/mailman/data/postfix_domains" ];
      config =
        let
          lmtp = [ "hash:/var/lib/mailman/data/postfix_lmtp" ];
        in
        {
          transport_maps = lmtp;
          local_recipient_maps = lmtp;
        };
    };
    nginx.virtualHosts.${hostname} = {
      enableACME = true;
      forceSSL = true;
    };
  };

}
