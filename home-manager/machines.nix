let
  inherit (import (import ../nix/sources.nix).nixos-unstable { }) lib;
  restrictedPages = [
    "reddit.com"
    "github.com"
    "*.ccc.de"
    "haskell.org"
    "*.haskell.org"
    "*.nixos.org"
    "nixos.org"
    "matrix.org"
    "element.io"
    "youtube.de"
    "youtube.com"
    "*.element.io"
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
  makeConfig = hostName: imports:
    { ... }: {
      imports = imports ++ [ ./roles/default.nix ];
      m-0.hostName = hostName;
      nixpkgs.overlays = [ (_: _: (import ../channels.nix).${hostName}) ];
    };
  makeAutostart = name:
    { config, ... }: {
      config.xdg.configFile."autostart/${name}.desktop".source =
        "${config.home.path}/share/applications/${name}.desktop";
    };
  on-my-machines = [
    ./roles/on-my-machine.nix
    ./roles/mail.nix
    ./roles/kassandra.nix
    ./roles/taskwarrior.nix
    ./roles/vdirsyncer.nix
    ./roles/khard.nix
    ./roles/khal.nix
    ./roles/mode-switching.nix
  ];
  daily-driver = name: extra:
    let
      all = extra ++ on-my-machines ++ [
        (import ./roles/firefox.nix "http://localhost:8842")
        (makeAutostart "kassandra2")
        (makeAutostart "unlock-ssh")
        ./roles/accounting.nix
        ./roles/beets.nix
        ./roles/daily-driver-programs.nix
        ./roles/desktop-items.nix
        ./roles/desktop.nix
        ./roles/git-sign.nix
        ./roles/gnome.nix
        ./roles/hoogle.nix
        ./roles/kitty.nix
        ./roles/mpclient.nix
        ./roles/mpd.nix
        ./roles/pythia.nix
        ./roles/research.nix
        ./roles/sleep-nag.nix
        ./roles/systemd-exporter.nix
        ./roles/tinkering.nix
        ./roles/update_tasks.nix
      ];
      orgaExtra = [
        ./roles/accounting.nix
        ./roles/mail-client.nix
        ./roles/pythia.nix
        ./roles/tinkering.nix
      ];
      blockServer = import ./roles/block-server.nix;
    in
    {
      research = makeConfig name (all ++ [ (blockServer restrictedPages) ]);
      orga = makeConfig name (all ++ orgaExtra ++ [ (blockServer restrictedPages) ]);
      leisure = makeConfig name (
        all ++ orgaExtra ++ [
          ./roles/games.nix
          ./roles/chat.nix
          (blockServer [ ])
        ]
      );
    };
in
{
  apollo = daily-driver "apollo" [
    ./roles/battery.nix
  ];
  zeus = daily-driver "zeus" [
    ./roles/hourly-maintenance.nix
    ./roles/state.nix
  ];
  hera = {
    default = makeConfig "hera" (on-my-machines ++ [
      ./roles/weechat
      ./roles/mail2rss.nix
      ./roles/headless-mpd.nix
      ./roles/headless.nix
    ]);
  };
}
