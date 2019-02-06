{ pkgs, config, lib, ... }:
let
  me = config.m-0.private.me;
in {
  imports = [
    <home-manager/nixos>
    ../common
    ./modules/laptop.nix
    ./modules/git-server.nix
    ./modules/server
    ./modules/server/mathechor.de.nix
    ./modules/standalone
    "${builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark"}/default.nix"
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
}
