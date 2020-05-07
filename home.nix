let
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./home ];
      m-0.hostName = hostName;
    };
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
                pkgs.writeTextDir "blocklist" (lib.concatStringsSep "\r\n" list)
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
    "matrix.org"
    "riot.im"
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
  apollo-orga = apolloConfig [
    home/accounting.nix
    home/mail.nix
    home/pythia.nix
    home/update_tasks.nix
    (setStartpage "https://cloud.maralorn.de/apps/calendar")
    (makeBlock (tinkerPages ++ leisurePages))
  ];
  apollo-research = apolloConfig [
    home/latex.nix
    (makeBlock (tinkerPages ++ leisurePages))
    (setStartpage "http://localhost:8042")
  ];

  apollo-tinkering = apolloConfig [
    home/mail.nix
    home/update-script.nix
    home/tinkering.nix
    home/chat.nix
    (makeBlock leisurePages)
    (setStartpage "https://stats.maralorn.de/d/health-status")
  ];
  apollo-leisure = apolloConfig [
    home/games.nix
    home/chat.nix
    (makeBlock [ ])
    (setStartpage "https://stats.maralorn.de/d/health-status")
  ];

  hera = { };
}
