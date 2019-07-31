{ pkgs, config, lib, ... }:
let
  inherit (import ../common/lib.nix) sources;
  me = config.m-0.private.me;
in {
  imports = [
    ../cachix.nix
    ../common
    ./modules/laptop.nix
    ./modules/mathechor.de.nix
    ./modules/blog.nix
    ./modules/riot.nix
    ./modules/loginctl-linger.nix
  ];

  config = {

    i18n = { defaultLocale = "en_US.UTF-8"; };

    time.timeZone = "Europe/Berlin";

    networking = {
      firewall.allowPing = true;
      useDHCP = false;
      hosts = lib.zipAttrs
        (lib.mapAttrsToList (host: ip: { "${ip}" = "${host} ${host}.m-0.eu"; })
        config.m-0.hosts);
    };

    users = {
      mutableUsers = false;
      users.root = { openssh.authorizedKeys.keys = me.keys; };
    };

    environment = {
      etc = {
        "nix-path/nixpkgs".source = sources.nixpkgs;
        "nix-path/nixos".source = sources.nixpkgs;
        "nix-path/unstable".source = sources.unstable;
        "nix-path/home-manager".source = sources.home-manager;
      };
    };

    nix = {
      binaryCaches =
        [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
      binaryCachePublicKeys =
        [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
      nixPath = [ "/etc/nix-path" ];
    };

    services = {
      prometheus.exporters = {
        node = {
          enable = true;
          openFirewall = true;
          enabledCollectors = [ "systemd" "logind" ];
          disabledCollectors = [ "timex" ];
        };
        nginx = {
          enable = config.services.nginx.enable;
          openFirewall = true;
        };
      };
      nginx = {
        statusPage = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedTlsSettings = true;
      };
    };
  };
}
