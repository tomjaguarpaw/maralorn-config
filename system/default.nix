{ pkgs, config, lib, ... }:
let
  me = config.m-0.private.me;
  home-manager = (builtins.fetchGit "https://github.com/rycee/home-manager/");
  unstable = builtins.fetchGit { url = "https://github.com/NixOS/nixpkgs-channels"; ref = "nixos-unstable";};
in {
  disabledModules = [ "services/mail/rspamd.nix" ];
  imports = [
    "${home-manager}/nixos"
    ../common
    ./modules/laptop.nix
    ./modules/git.nix
    ./modules/mathechor.de.nix
    ./modules/server
    ./modules/blog.nix
    ./modules/standalone
    "${(builtins.fetchGit "ssh://git@git.darmstadt.ccc.de/cdark.net/nixdark")}"
    "${(builtins.fetchGit "ssh://git@hera/nixos-mailserver")}"
    ./rspamd.nix
    ./modules/loginctl-linger.nix
  ];


  config = {
#    nix.nixPath = [ "unstable=channel:nixos-unstable" "nixos-config=/etc/nixos/" "stable=channel:nixos-18.09" ];

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

    services = {
      nginx = {
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedTlsSettings = true;
      };
    };
  };
}
