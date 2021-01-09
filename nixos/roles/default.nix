{ pkgs, config, lib, ... }: {
  imports = [ ../../common ./admin.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  # For nixos-rebuild
  nixpkgs.overlays =
    [ (_: _: (import ../../channels.nix).${config.networking.hostName}) ]
    ++ import ../../overlays { inherit lib; };

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
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  environment = {
    etc = lib.mapAttrs'
      (name: value: lib.nameValuePair "nix-path/${name}" { source = value; })
      (lib.filterAttrs (name: value: name != "__functor") pkgs.sources) // {
        "nix-path/nixos".source = pkgs.sources.${pkgs.nixpkgs-channel};
        "nix-path/nixpkgs".source = pkgs.sources.${pkgs.nixpkgs-channel};
        "nix-path/home-manager".source =
          pkgs.sources.${pkgs.home-manager-channel};
      };
    variables =
      lib.genAttrs [ "CURL_CA_BUNDLE" "GIT_SSL_CAINFO" "SSL_CERT_FILE" ]
      (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");
  };

  system.activationScripts =
    lib.mkIf (!pkgs.withSecrets) { text = "echo No secrets loaded!; exit 1;"; };

  nix = {
    binaryCaches =
      [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
    binaryCachePublicKeys =
      [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
    nixPath = [ "/etc/nix-path" ];
    trustedUsers = [ "maralorn" "laminar" ];
    buildMachines = pkgs.privateValue [ ] "remote-builders";
    extraOptions = ''
      fallback = true
      keep-outputs = true
      auto-optimise-store = true
      builders-use-substitutes = true
    '';
    optimise.automatic = true;
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
    journald.extraConfig = "SystemMaxUse=512M";
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
      appendHttpConfig = "access_log off;";
    };
  };
  programs = {
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };
}
