let
  inherit (import <nixpkgs> { }) lib;
  home-manager = import <home-manager/home-manager/home-manager.nix>;
  buildHomeManager = attr:
    (home-manager {
      confPath = ~/git/config/home.nix;
      confAttr = attr;
    }).activationPackage;
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./home ];
      m-0.hostName = hostName;
    };
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
    ];
    leisurePages = [
      "zeit.de"
      "heise.de"
      "spiegel.de"
      "youtube.de"
      "youtube.com"
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
        home/battery.nix
        home/mpd.nix
        home/mpclient.nix
        home/on-my-machine.nix
        home/desktop
        home/firefox.nix
        home/git-sign.nix
        home/laptop.nix
      ]);
  in {
    orga = apolloConfig [
      home/accounting.nix
      home/mail.nix
      home/pythia.nix
      home/update_tasks.nix
      (setStartpage "https://cloud.maralorn.de/apps/calendar")
      (makeBlock (tinkerPages ++ leisurePages))
    ];
    research = apolloConfig [
      home/latex.nix
      (makeBlock (tinkerPages ++ leisurePages))
      (setStartpage "http://localhost:8042")
    ];

    tinkering = apolloConfig [
      home/mail.nix
      home/update-script.nix
      home/tinkering.nix
      home/chat.nix
      (makeBlock leisurePages)
      (setStartpage "https://stats.maralorn.de/d/health-status")
    ];
    leisure = apolloConfig [
      home/games.nix
      home/chat.nix
      (makeBlock [ ])
      (setStartpage "https://stats.maralorn.de/d/health-status")
    ];
  };

in {
  hera = makeConfig "hera" [
    ./home
    home/on-my-machine.nix
    hosts/hera/weechat
    hosts/hera/secret
    home/kassandra.nix
    home/headless-mpd.nix
    home/mail.nix
  ];
} // lib.listToAttrs (lib.mapAttrsToList (name: config:
  {
    name = "apollo-${name}";
    value = config;
  }
) apollo)
