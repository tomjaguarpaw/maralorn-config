{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (config.m-0) hosts;
  inherit (config.networking) hostName;
in
{
  imports = [ ../../common ];

  i18n = {
    defaultLocale = "en_DK.UTF-8";
    supportedLocales = [
      "en_DK.UTF-8/UTF-8"
      "de_DE.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    resolvconf.dnsExtensionMechanism = false; # this breaks dnssec but is necessary for certain bad-behaved hotspots
    firewall = {
      enable = true; # It’s the default, but better make sure.
      allowPing = true;
    };
    nftables.enable = true; # Uses firewall variables since 23.05
    useNetworkd = true;
    useDHCP = false; # enabled per interface
    hosts = lib.zipAttrs (
      lib.mapAttrsToList
        (
          host: ip:
          if builtins.typeOf ip == "set" then
            {
              ${ip.AAAA or null} = "${host} ${host}.m-0.eu";
              ${ip.A or null} = "${host} ${host}.m-0.eu";
            }
          else
            { "${ip}" = "${host} ${host}.m-0.eu"; }
        )
        config.m-0.hosts
      ++
        lib.mapAttrsToList
          (
            host: ips:
            let
              mkHost = name: "${name} ${name}.maralorn.de";
              name = "${host} ${host}.vpn.m-0.eu ${
                lib.concatMapStringsSep " " mkHost config.m-0.hosts.aliases.${host} or [ ]
              }";
            in
            {
              ${ips.AAAA} = name;
              ${ips.A} = name;
            }
          )
          config.m-0.hosts.tailscale
    );
  };

  m-0.virtualHosts = lib.genAttrs (hosts.aliases.${hostName} or [ ]) (name: "${name}.maralorn.de");

  nix = {
    # Extra Option which is on by default: allow-import-from-derivation = true
    settings = {
      trusted-public-keys = [ "cache.maralorn.de:nul5zddJUyqgWvtcailq5WMdnqWXMmSY/JOxumIvTdU=" ];
      experimental-features = [
        "nix-command"
        "flakes"
        "repl-flake"
      ];
      fallback = true;
      auto-optimise-store = true;
      builders-use-substitutes = true;
      keep-derivations = true;
      keep-outputs = true;
      warn-dirty = false;
    };
  };

  security.acme = {
    defaults = {
      dnsProvider = "inwx";
      credentialsFile = config.age.secrets.inwx_credentials.path;
      email = "security@maralorn.de";
    };
    acceptTerms = true;
    certs = lib.genAttrs (builtins.attrValues config.m-0.virtualHosts) (
      _: {
        webroot = null;
        dnsProvider = "inwx";
      }
    );
  };

  security.pam.services."login".failDelay.enable = true;

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
  };

  environment = {
    systemPackages = builtins.attrValues {
      inherit (pkgs)
        asciinema
        bat
        bind
        binutils
        borgbackup
        btop
        builders-configurator
        compsize
        curl
        direnv
        entr
        exa
        fd
        file
        fzf
        git
        gnumake
        gnupg
        helix
        home-manager
        htop
        inotifyTools
        jq
        libargon2
        liboping
        lsof
        mblaze
        mediainfo
        mkpasswd
        ncdu
        niv
        nix-output-monitor
        nmap
        ntfsprogs
        parted
        psmisc
        pwgen
        python3
        ranger
        rename
        ripgrep
        sd
        socat
        sshuttle
        sysbench
        tcpdump
        tig
        tmux
        tree
        unzip
        ventoy-bin
        vnstat
        wget
        whois
        wireguard-tools
      ;
      inherit (pkgs.python3Packages) qrcode;
    };
    variables =
      lib.genAttrs
        [
          "CURL_CA_BUNDLE"
          "GIT_SSL_CAINFO"
          "SSL_CERT_FILE"
        ]
        (_: "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt");
  };

  systemd = {
    network.wait-online.anyInterface = true;
    services =
      let
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
      {
        nix-gc.serviceConfig.Type = "oneshot";
        nix-optimise.serviceConfig.Type = "oneshot";
      }
      // builtins.listToAttrs (map makeConfig hosts);

    oomd.enableRootSlice = true;
  };

  services = {
    logind.killUserProcesses = false;
    journald.extraConfig = "SystemMaxUse=5G";
    prometheus.exporters = {
      node = {
        enable = true;
        enabledCollectors = [
          "systemd"
          "logind"
        ];
        disabledCollectors = [ "timex" ];
      };
      nginx = {
        inherit (config.services.nginx) enable;
      };
    };
    nginx = {
      enable = lib.mkDefault (config.m-0.virtualHosts != { });
      virtualHosts =
        lib.mapAttrs'
          (name: hostname: {
            name = hostname;
            value = {
              forceSSL = true;
              enableACME = true;
              extraConfig = lib.mkIf (!(builtins.elem name (hosts.publicAliases.${hostName} or [ ]))) ''
                satisfy any;
                ${lib.concatMapStringsSep "\n" (ip_range: "allow ${ip_range};") config.m-0.headscaleIPs}
                deny all;
              '';
            };
          })
          config.m-0.virtualHosts;
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
}
