let
  inherit (import (import ../nix/sources.nix).nixpkgs { }) lib;
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./default.nix ];
      m-0.hostName = hostName;
    };
in {
  apollo = let
    setStartpage = startpage:
      { ... }: {
        programs.firefox.profiles."fz2sm95u.default".settings = {
          "browser.startup.homepage" = startpage;
        };
      };
    makeBlock = list:
      { pkgs, lib, ... }: {
        systemd.user = {
          services.blockserver = {
            Unit = { Description = "Serve a blocklist"; };
            Service = {
              ExecStart = "${pkgs.python3}/bin/python -m http.server 8842 -d ${
                  pkgs.writeTextDir "blocklist"
                  (lib.concatStringsSep "\r\n" list)
                }";
              Restart = "always";
            };
            Install = { WantedBy = [ "default.target" ]; };
          };
        };
      };
    tinkerPages = [
      "reddit.com"
      "github.com"
      "*.ccc.de"
      "haskell.org"
      "*.haskell.org"
      "*.nixos.org"
      "nixos.org"
      "matrix.org"
      "riot.im"
      "youtube.de"
      "youtube.com"
    ];
    leisurePages = [
      "zeit.de"
      "heise.de"
      "spiegel.de"
      "xkcd.com"
      "smbc-comics.com"
      "tagesschau.de"
      "welt.de"
      "ndr.de"
      "ard.de"
      "zdf.de"
      "twitter.com"
      "chaos.social"
    ];
    apolloConfig = imports:
      makeConfig "apollo" (imports ++ [
        ./roles/zettelkasten.nix
        ./battery.nix
        ./mpd.nix
        ./mpclient.nix
        ./on-my-machine.nix
        ./desktop
        ./git-sign.nix
        ./laptop.nix
        ./mail.nix
        ./update_tasks.nix
      ]);
    unrestricted = [
      ./orga.nix
      ./accounting.nix
      ./mail-client.nix
      ./pythia.nix
      ./research.nix
      ./tinkering.nix
      ./chat.nix
      (setStartpage "https://stats.maralorn.de/d/health-status")
      (makeBlock [ ])
    ];
  in {
    unrestricted = apolloConfig unrestricted;
    orga = apolloConfig [
      ./orga.nix
      ./mail-client.nix
      ./accounting.nix
      ./pythia.nix
      (setStartpage "https://habitica.com")
      (makeBlock (tinkerPages ++ leisurePages))
    ];
    research = apolloConfig [
      ./research.nix
      (makeBlock (tinkerPages ++ leisurePages))
      (setStartpage "http://localhost:8042")
    ];
    gaming = apolloConfig (unrestricted ++ [./games.nix]);
  };
  hera = {
    default = makeConfig "hera" [
      ./on-my-machine.nix
      ./headless.nix
      ..nixos/machineshera/weechat
      ..nixos/machineshera/secret
      ./kassandra-server.nix
      ./headless-mpd.nix
      ./mail.nix
    ];
  };
}
