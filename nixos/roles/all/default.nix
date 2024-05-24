{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [ ../../../common ];

  i18n = {
    defaultLocale = "en_DK.UTF-8";
    supportedLocales = [
      "en_DK.UTF-8/UTF-8"
      "de_DE.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
  };

  time.timeZone = "Europe/Berlin";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  security.acme = {
    defaults = {
      dnsProvider = "inwx";
      credentialsFile = config.age.secrets.inwx_credentials.path;
      email = "security@maralorn.de";
    };
    acceptTerms = true;
    certs = lib.genAttrs (builtins.attrValues config.m-0.virtualHosts) (_: {
      webroot = null;
      dnsProvider = "inwx";
    });
  };

  security.pam.services."login".failDelay.enable = true;

  users.mutableUsers = false;

  environment.variables = lib.genAttrs [
    "CURL_CA_BUNDLE"
    "GIT_SSL_CAINFO"
    "SSL_CERT_FILE"
  ] (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");

  systemd.oomd.enableRootSlice = true;

  services = {
    logind.killUserProcesses = false;
    journald.extraConfig = "SystemMaxUse=5G";
  };

  programs.ssh = {
    extraConfig = pkgs.privateValue "" "ssh-config";
    startAgent = true;
  };
}
