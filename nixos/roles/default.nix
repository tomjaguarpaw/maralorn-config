{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import ../../nix/sources.nix) nixos-unstable;
  networkingModule = name: "${nixos-unstable}/nixos/modules/services/networking/${name}.nix";
in {
  # nftables using module not available in 22.11.
  disabledModules = [
    "services/networking/firewall.nix"
    "services/networking/nftables.nix"
    "services/networking/nat.nix"
    "services/networking/redsocks.nix"
    "services/networking/miniupnpd.nix"
    "services/audio/roon-server.nix"
    "services/audio/roon-bridge.nix"
  ];

  imports = [
    (builtins.getFlake "github:ryantm/agenix").nixosModules.default
    (networkingModule "firewall-iptables")
    (networkingModule "firewall-nftables")
    (networkingModule "firewall")
    (networkingModule "nat-iptables")
    (networkingModule "nat-nftables")
    (networkingModule "nat")
    (networkingModule "nftables")
    ../../common
    ./admin.nix
  ];

  age.secrets = (import ../../private/secret-config.nix).module-config {
    inherit (config.networking) hostName;
    inherit lib;
  };

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
    firewall = {
      enable = true; # It’s the default, but better make sure.
      allowPing = true;
      extraInputRules = "meta iifname m0wire accept comment \"My VPN\"";
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
    systemPackages = builtins.attrValues {
      inherit
        (pkgs)
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
        ;
      inherit (pkgs.python3Packages) qrcode;
    };
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
    settings = {
      substituters = lib.mkAfter (
        pkgs.privateValue [] "binary-caches"
        # ++ (
        #   if config.networking.hostName != "hera" then [ "ssh-ng://nix-ssh@hera.m-0.eu?trusted=true&priority=100" ] else [ ]
        # )
      );
      trusted-public-keys = [
        "nixbuild.net/maralorn-1:cpqv21sJgRL+ROaKY1Gr0k7AKolAKaP3S3iemGxK/30="
      ];
      trusted-users = ["maralorn" "laminar"];
    };
    nixPath = ["/etc/nix-path"];
    buildMachines = pkgs.privateValue [] "remote-builders";
    extraOptions = ''
      experimental-features = nix-command flakes
      fallback = true
      auto-optimise-store = true
      builders-use-substitutes = true
      allow-import-from-derivation = true
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
