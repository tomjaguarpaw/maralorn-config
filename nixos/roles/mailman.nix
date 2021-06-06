{ pkgs, lib, config, ... }:
let
  hostname = "lists.maralorn.de";
  admin = "malte.brandy@maralorn.de";
  cfg = config.services.mailman;
  lists = pkgs.privateValue { } "mail/lists";
in
{
  systemd.services.mailman.postStart = lib.concatStringsSep "\n" (
    map
      (
        x: ''
          ${cfg.package}/bin/mailman syncmembers -W -G - "${x}" << EOF
          ${lib.concatStringsSep "\n" lists.${x}}
          EOF
        ''
      )
      (builtins.attrNames lists)
  );
  services = {
    mailman = {
      enable = true;
      webHosts = [ hostname ];
      serve.enable = true;
      enablePostfix = true;
      siteOwner = admin;
      settings = {
        mailman.default_language = "de";
        "paths.fhs".template_dir = lib.mkForce (
          pkgs.setToDirectories {
            site.de = {
              "list:user:notice:goodbye.txt" = builtins.toFile "goodbye" ''
                Du erhältst nun keine E-Mails mehr über diese Mailingliste.

                Bei Fragen oder wenn Du doch E-Mails von dieser Liste bekommen möchtest wende Dich an ${admin}.
              '';
              "list:member:generic:footer.txt" = builtins.toFile "footer" ''
                ---
                Du erhältst diese E-Mail über die Mailingliste "$display_name".
                Bei Fragen oder wenn Du diese E-Mails nicht mehr bekommen möchtest wende Dich an ${admin}.
              '';

              "list:user:notice:welcome.txt" = builtins.toFile "welcome" ''
                Herzlich Willkommen auf der Mailingliste "$display_name".

                Bei Fragen und wenn Du keine E-Mails von dieser Liste mehr bekommen möchtest wende Dich an ${admin}.
              '';
            };
          }
        ).outPath;
      };
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
