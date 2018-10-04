{ pkgs, config, lib, ... }:
let
  me = config.m-0.private.me;
  unstable = import <unstable> {};
in {
  # channel = 18.03

  imports = [
    <home-manager/nixos>
    ../common
    ./modules/laptop.nix
    ./modules/server
    ./modules/standalone
    ./modules/cdarknet
    ./modules/loginctl-linger.nix
  ];


  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    firewall.allowPing = true;
    useDHCP = false;
    hosts = lib.zipAttrs (lib.mapAttrsToList (host: ip: {"${ip}" = "${host} ${host}.m-0.eu";} ) config.m-0.hosts);
  };

  users = {
    mutableUsers = false;
    users.root = {
      openssh.authorizedKeys.keys = me.keys;
    };
  };
  environment.sessionVariables = {
    LOCALE_ARCHIVE_2_27 = "${unstable.glibcLocales}/lib/locale/locale-archive";
  };
}
