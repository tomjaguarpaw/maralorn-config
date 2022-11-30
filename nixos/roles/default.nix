{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [../../common ./admin.nix ../../cachix.nix];

  i18n = {
    defaultLocale = "en_DK.UTF-8";
    supportedLocales = ["en_DK.UTF-8/UTF-8" "de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8"];
  };

  # For nixos-rebuild
  nixpkgs.overlays =
    [(_: _: (import ../../channels.nix)."${config.networking.hostName}")]
    ++ import ../../overlays {inherit lib;};

  time.timeZone = "Europe/Berlin";

  networking = {
    resolvconf.dnsExtensionMechanism = false; # this breaks dnssec but is necessary for certain bad-behaved hotspots
    firewall.allowPing = true;
    useDHCP = false; # enabled per interface
    hosts =
      lib.zipAttrs
      (
        lib.mapAttrsToList
        (host: ip:
          if builtins.typeOf ip == "set"
          then {}
          else {"${ip}" = "${host} ${host}.m-0.eu";})
        config.m-0.hosts
      );
  };

  security.acme = {
    defaults.email = "security@maralorn.de";
    acceptTerms = true;
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  environment = {
    etc =
      lib.mapAttrs'
      (name: value: lib.nameValuePair "nix-path/${name}" {source = value;})
      (lib.filterAttrs (name: value: name != "__functor") pkgs.sources)
      // {
        "nix-path/nixos".source = pkgs.sources."${pkgs.nixpkgs-channel}";
        "nix-path/nixpkgs".source = pkgs.sources."${pkgs.nixpkgs-channel}";
        "nix-path/home-manager".source =
          pkgs.sources."${pkgs.home-manager-channel}";
      };
    variables =
      lib.genAttrs ["CURL_CA_BUNDLE" "GIT_SSL_CAINFO" "SSL_CERT_FILE"]
      (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");
  };

  system.activationScripts =
    lib.mkIf (!pkgs.withSecrets) {text = "echo No secrets loaded!; exit 1;";};

  nix = {
    settings.substituters = lib.mkAfter (
      pkgs.privateValue [] "binary-caches"
      # ++ (
      #   if config.networking.hostName != "hera" then [ "ssh-ng://nix-ssh@hera.m-0.eu?trusted=true&priority=100" ] else [ ]
      # )
    );
    settings.trusted-public-keys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "nixbuild.net/maralorn-1:cpqv21sJgRL+ROaKY1Gr0k7AKolAKaP3S3iemGxK/30="
    ];
    nixPath = ["/etc/nix-path"];
    settings.trusted-users = ["maralorn" "laminar"];
    buildMachines = pkgs.privateValue [] "remote-builders";
    extraOptions = ''
      experimental-features = nix-command flakes
      fallback = true
      auto-optimise-store = true
      builders-use-substitutes = true
    '';
    optimise = {
      dates = [];
      automatic = true;
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
  in
    {nix-optimise.serviceConfig.Type = "oneshot";} // builtins.listToAttrs (map makeConfig hosts);

  services = {
    logind.killUserProcesses = false;
    journald.extraConfig = "SystemMaxUse=5G";
    prometheus.exporters = {
      node = {
        enable = true;
        enabledCollectors = ["systemd" "logind"];
        disabledCollectors = ["timex"];
      };
      nginx = {
        inherit (config.services.nginx) enable;
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
  programs = {
    command-not-found.dbPath = "${
      builtins.fetchTarball {
        url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
      }
    }/programs.sqlite";
    git.config.init.defaultBranch = "main";
    ssh = {
      extraConfig = pkgs.privateValue "" "ssh-config";
      startAgent = true;
    };
    zsh = {
      enable = true;
      autosuggestions.enable = true;
      enableCompletion = true;
      syntaxHighlighting.enable = true;
    };
  };
}
