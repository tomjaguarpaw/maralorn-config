{ pkgs, config, lib, ... }:
let
  me = config.m-0.private.me;
  home-manager = (builtins.fetchGit { url = "https://github.com/rycee/home-manager/"; ref = "nixos-module-user-pkgs-v2";});
in {
  imports = [
    "${home-manager}/nixos"
    ../common
    ./modules/laptop.nix
    ./modules/server/git.nix
    ./modules/server
    ./modules/server/mathechor.de.nix
    ./modules/standalone
    "${(builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark")}"
    "${(builtins.fetchGit "ssh://git@hera/nixos-mailserver")}"
    ./modules/loginctl-linger.nix
  ];

  config = {

    i18n = {
      defaultLocale = "en_US.UTF-8";
    };

    time.timeZone = "Europe/Berlin";

    home-manager.useUserPackages = true;

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
    systemd.services.nixos-upgrade.path = [ pkgs.gnutar pkgs.xz.bin pkgs.gitMinimal config.nix.package.out ];
  };
}
