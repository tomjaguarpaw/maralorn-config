let
  restrictedPages =
    [
      "reddit.com"
      "github.com"
      "*.ccc.de"
      "haskell.org"
      "*.haskell.org"
      "*.nixos.org"
      "nixos.org"
      "matrix.org"
      "element.io"
      "youtube.*"
      "*.element.io"
      "twitter.com"
    ]
    ++ newsPages;
  newsPages = [
    "chaos.social"
    "zeit.de"
    "heise.de"
    "spiegel.de"
    "taz.de"
    "faz.net"
    "bild.de"
    "xkcd.com"
    "smbc-comics.com"
    "tagesschau.de"
    "welt.de"
    "ndr.de"
    "ard.de"
    "zdf.de"
  ];

  makeConfig = hostName: imports: _: {
    imports = imports ++ [./roles/default.nix];
    m-0.hostName = hostName;
    nixpkgs.overlays = [(_: _: (import ../channels.nix)."${hostName}")];
  };
  makeAutostart = name: {config, ...}: {
    config.xdg.configFile."autostart/${name}.desktop".source = "${config.home.path}/share/applications/${name}.desktop";
  };
  on-my-machines = [
    ./roles/on-my-machine.nix
    ./roles/accounting.nix
    ./roles/mail.nix
    ./roles/kassandra.nix
    ./roles/taskwarrior.nix
    ./roles/vdirsyncer.nix
    ./roles/khard.nix
    ./roles/khal.nix
    ./roles/mode-switching.nix
    ./roles/systemd-exporter.nix
  ];
  daily-driver = name: extra: let
    all =
      extra
      ++ on-my-machines
      ++ [
        (makeAutostart "kassandra2")
        (makeAutostart "unlock-ssh")
        ./roles/beets.nix
        ./roles/daily-driver-programs.nix
        ./roles/desktop-items.nix
        ./roles/desktop.nix
        ./roles/git-sign.nix
        ./roles/gnome.nix
        ./roles/status-script.nix
        ./roles/hoogle.nix
        ./roles/terminal.nix
        ./roles/mpclient.nix
        ./roles/mpd.nix
        ./roles/pythia.nix
        ./roles/research.nix
        #./roles/night-shutdown.nix
        ./roles/tinkering.nix
        ./roles/wallpaper.nix
        ./roles/zettelkasten.nix
        ./roles/kitty.nix
      ];
    orgaExtra = [
      ./roles/accounting.nix
      ./roles/mail-client.nix
      ./roles/pythia.nix
      ./roles/tinkering.nix
    ];
    blockServer = import ./roles/block-server.nix;
  in {
    klausur = makeConfig name (
      all
      ++ [
        (blockServer restrictedPages)
      ]
    );
    orga = makeConfig name (
      all
      ++ orgaExtra
      ++ [
        (blockServer restrictedPages)
      ]
    );
    communication = makeConfig name (
      all
      ++ orgaExtra
      ++ [
        ./roles/chat.nix
        (blockServer restrictedPages)
      ]
    );
    code = makeConfig name (
      all
      ++ orgaExtra
      ++ [
        ./roles/chat.nix
        ./roles/leisure.nix
        (blockServer newsPages)
      ]
    );
    leisure = makeConfig name (
      all
      ++ orgaExtra
      ++ [
        ./roles/games.nix
        ./roles/chat.nix
        ./roles/leisure.nix
        (blockServer newsPages)
      ]
    );
    unrestricted = makeConfig name (
      all
      ++ orgaExtra
      ++ [
        ./roles/games.nix
        ./roles/chat.nix
        ./roles/leisure.nix
        (blockServer [])
      ]
    );
  };
in {
  apollo = daily-driver "apollo" [
    ./roles/battery.nix
    ./roles/untrusted-env.nix
  ];
  zeus = daily-driver "zeus" [
    ./roles/hourly-maintenance.nix
    (import ./roles/state.nix "orga")
    ./roles/trusted-env.nix
    ./roles/monitor-config
  ];
  fluffy.default = makeConfig "fluffy" [
    ./roles/on-my-machine.nix
    ./roles/mode-switching.nix
    ./roles/systemd-exporter.nix
    ./roles/headless.nix
    (import ./roles/state.nix "default")
  ];
  hera.default = makeConfig "hera" (on-my-machines
    ++ [
      ./roles/fetch-banking-timer.nix
      ./roles/weechat
      ./roles/mail-sort.nix
      ./roles/mail2rss.nix
      ./roles/headless-mpd.nix
      ./roles/headless.nix
      ./roles/create-plans.nix
    ]);
}
