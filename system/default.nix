{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/my-lib.nix) writeHaskellScript getNivPath;
  me = config.m-0.private.me;
  sources = import ../nix/sources.nix;
  nixpkgsPath = sources.nixpkgs;
  unstablePath = sources.unstable;
in {
  imports = [
    ../common
    ./modules/laptop.nix
    ./modules/git.nix
    ./modules/mathechor.de.nix
    ./modules/server
    ./modules/blog.nix
    ./modules/riot.nix
    ./modules/standalone
    ./modules/loginctl-linger.nix
  ];

  config = {

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

    environment = {
      systemPackages = [
        (writeHaskellScript {
            name = "update-nixos";
            imports = [ "qualified Data.ByteString.Lazy.Char8 as C" "qualified Data.List as L" ];
            bins = [ getNivPath config.system.build.nixos-rebuild];
          } ''

          getNivAssign name = fmap process . readTrim $ get_niv_path "/etc/nixos/nix/sources.nix" name
              where process str = ["-I", name ++ "=" ++ C.unpack str]

          main = do
              paths <- mapM getNivAssign ["nixpkgs", "unstable"]
              nixos_rebuild (concat paths ++ ["switch"])
        '')
      ];
      etc = {
        "nix-path/nixpkgs".source = nixpkgsPath;
        "nix-path/nixos".source = nixpkgsPath;
        "nix-path/unstable".source = unstablePath;
      };
    };


    nix = {
      binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
      binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
      nixPath = [
        "/etc/nix-path"
        "nixos-config=/etc/nixos/configuration.nix"
      ];
      gc.options = "--delete-older-than 5d";
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
