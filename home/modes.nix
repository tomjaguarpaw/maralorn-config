let
  inherit (import <nixpkgs> { }) lib;
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
        ./battery.nix
        ./mpd.nix
        ./mpclient.nix
        ./on-my-machine.nix
        ./desktop
        ./firefox.nix
        ./git-sign.nix
        ./laptop.nix
      ]);
  in {
    communication = apolloConfig [
      ./mail.nix
      ./update_tasks.nix
      ./chat.nix
      (setStartpage "https://cloud.maralorn.de/apps/calendar")
      (makeBlock [ ])
    ];
    orga = apolloConfig [
      ./accounting.nix
      ./mail.nix
      ./pythia.nix
      ./update_tasks.nix
      (setStartpage "https://cloud.maralorn.de/apps/calendar")
      (makeBlock (tinkerPages ++ leisurePages))
    ];
    research = apolloConfig [
      ./research.nix
      (makeBlock (tinkerPages ++ leisurePages))
      (setStartpage "http://localhost:8042")
    ];

    tinkering = apolloConfig [
      ./mail.nix
      ./update-script.nix
      ./tinkering.nix
      ./chat.nix
      (makeBlock leisurePages)
      (setStartpage "https://stats.maralorn.de/d/health-status")
    ];
    leisure = apolloConfig [
      ./games.nix
      ./chat.nix
      (makeBlock [ ])
      (setStartpage "https://stats.maralorn.de/d/health-status")
    ];
  };
  hera = {
    default = makeConfig "hera" [
      ./on-my-machine.nix
      ./hosts/hera/weechat
      ./hosts/hera/secret
      ./kassandra.nix
      ./headless-mpd.nix
      ./mail.nix
    ];
  };
}
