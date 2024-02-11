{ pkgs, lib, ... }:
let
  hostname = "lists.maralorn.de";
  lists = pkgs.privateValue { } "mail/lists";
  me = pkgs.privateValue { mail = ""; } "mail/me";
  admin = me.mail;
in
{
  systemd.services.mailman.postStart = lib.concatStringsSep "\n" (
    map (x: ''
      ${(pkgs.mailmanPackages.buildEnvs { }).mailmanEnv}/bin/mailman syncmembers -W -G - "${x}" << EOF
      ${lib.concatStringsSep "\n" lists."${x}"}
      EOF
    '') (builtins.attrNames lists)
  );
  services = {
    mailman = {
      enable = true;
      webHosts = [ hostname ];
      serve.enable = true;
      enablePostfix = true;
      siteOwner = admin;
      webSettings = {
        ACCOUNT_ADAPTER = "django_mailman3.views.user_adapter.DisableSignupAdapter";
      };
      hyperkitty.enable = true; # Annoyingly mailman crashes when it can‘t find
      # a hyperkitty. This is stupid, but I have no patience to find a fix for
      # this.
      settings = {
        mailman.default_language = "de";
        "paths.fhs".template_dir =
          lib.mkForce
            (pkgs.recursiveLinkFarm "mailman-templates" {
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
            }).outPath;
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
    nginx.virtualHosts."${hostname}" = {
      enableACME = true;
      forceSSL = true;
    };
  };
}
