{ pkgs, config, lib, ... }:
let me = config.m-0.private.me;
in {
  imports = [
    ../../common
    ./modules/laptop.nix
    ./modules/loginctl-linger.nix
  ];

  i18n = { defaultLocale = "en_US.UTF-8"; };

  # For nixos-rebuild
  nixpkgs.overlays = import ../../overlays { inherit lib; };

  time.timeZone = "Europe/Berlin";

  networking = {
    firewall.allowPing = true;
    useDHCP = false;
    hosts = lib.zipAttrs
      (lib.mapAttrsToList (host: ip: { "${ip}" = "${host} ${host}.m-0.eu"; })
        config.m-0.hosts);
  };

  security.acme = {
    email = "security@maralorn.de";
    acceptTerms = true;
  };

  users = {
    mutableUsers = false;
    users.root.openssh.authorizedKeys = { inherit (me) keys; };
  };

  environment = {
    etc = lib.mapAttrs'
      (name: value: lib.nameValuePair "nix-path/${name}" { source = value; })
      (lib.filterAttrs (name: value: name != "__functor") pkgs.sources) // {
        "nix-path/nixos".source = pkgs.sources.nixpkgs;
      };
    variables =
      lib.genAttrs [ "CURL_CA_BUNDLE" "GIT_SSL_CAINFO" "SSL_CERT_FILE" ]
      (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");
  };

  nix = {
    binaryCaches =
      [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
    binaryCachePublicKeys =
      [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
    nixPath = [ "/etc/nix-path" ];
    extraOptions = ''
      fallback = true
      keep-outputs = true
    '';
    gc = {
      automatic = true;
      dates = "Mon 4:00";
      options = "--delete-older-than 7d";
    };
  };

  systemd.services = let
    hosts = builtins.attrNames config.services.nginx.virtualHosts;
    makeConfig = host: {
      name = "acme-${host}";
      value = {
        serviceConfig = {
          Restart = "on-failure";
          RestartSec = 600;
        };
        unitConfig = {
          StartLimitIntervalSec = 2400;
          StartLimitBurst = 3;
        };
      };
    };
  in builtins.listToAttrs (map makeConfig hosts);

  services = {
    prometheus.exporters = {
      node = {
        enable = true;
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
      clientMaxBodySize = "500m";
    };
  };
}
