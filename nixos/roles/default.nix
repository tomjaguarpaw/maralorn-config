{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (config.m-0) hosts;
  inherit (config.networking) hostName;
in {
  imports = [
    ../../common
    ./admin.nix
  ];

  i18n = {
    defaultLocale = "en_DK.UTF-8";
    supportedLocales = ["en_DK.UTF-8/UTF-8" "de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8"];
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    resolvconf.dnsExtensionMechanism = false; # this breaks dnssec but is necessary for certain bad-behaved hotspots
    firewall = {
      enable = true; # It’s the default, but better make sure.
      allowPing = true;
    };
    nftables.enable = true; # Uses firewall variables since 23.05
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
        ++ lib.mapAttrsToList
        (host: ips: let
          mkHost = name: "${name} ${name}.maralorn.de";
          name = "${mkHost host} ${lib.concatMapStringsSep " " mkHost config.m-0.hosts.aliases.${host} or []}";
        in {
          ${ips.AAAA} = name;
          ${ips.A} = name;
        })
        config.m-0.hosts.tailscale
      );
  };

  m-0 = {
    virtualHosts = lib.genAttrs (hosts.aliases.${hostName} or []) (name: "${name}.maralorn.de");
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
    systemPackages = builtins.attrValues {
      inherit
        (pkgs)
        git
        gnumake
        mkpasswd
        file
        wget
        curl
        wireguard-tools
        gnupg
        bind
        liboping
        psmisc
        unzip
        rename
        whois
        lsof
        parted
        python3
        binutils
        ntfsprogs
        ventoy-bin
        htop
        helix
        btop
        tree
        pwgen
        borgbackup
        inotifyTools
        direnv
        socat
        nmap
        ncdu
        tcpdump
        tmux
        tig
        exa
        fzf
        fd
        sd
        bat
        ripgrep
        ranger
        pass
        sshuttle
        vnstat
        entr
        libargon2
        mblaze
        niv
        compsize
        mediainfo
        asciinema
        nix-output-monitor
        jq
        home-manager
        builders-configurator
        ;
      inherit (pkgs.python3Packages) qrcode;
    };
    variables =
      lib.genAttrs ["CURL_CA_BUNDLE" "GIT_SSL_CAINFO" "SSL_CERT_FILE"]
      (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");
  };

  nix = {
    registry.pkgs = {
      from = {
        type = "indirect";
        id = "pkgs";
      };
      flake = pkgs.flake-inputs.nixos-unstable;
    };
    settings.trusted-users = ["maralorn" "laminar"];
    # Extra Option which is on by default: allow-import-from-derivation = true
    extraOptions = ''
      experimental-features = nix-command flakes repl-flake
      fallback = true
      auto-optimise-store = true
      builders-use-substitutes = true
      warn-dirty = false
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
      };
    };
    nginx = {
      virtualHosts =
        lib.genAttrs
        (map (name: "${name}.maralorn.de") (builtins.filter (name: !(builtins.elem name hosts.publicAliases.${hostName} or []))
            (hosts.aliases.${hostName} or []))) (_: {
          extraConfig = ''
            satisfy any;
            ${lib.concatMapStringsSep "\n" (ip_range: "allow ${ip_range};") config.m-0.headscaleIPs}
            deny all;
          '';
        });
      statusPage = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      clientMaxBodySize = "500m";
    };
  };
  programs = {
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
  documentation = {
    # man.enable = true;
    doc.enable = false;
    info.enable = false;
    nixos.enable = false;
  };
}
