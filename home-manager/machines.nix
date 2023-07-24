flake-inputs:
let
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
    "youtube.*"
    "*.element.io"
    "twitter.com"
  ] ++ newsPages;
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
    imports = imports ++ [ ./roles/default.nix ];
    m-0.hostName = hostName;
  };
  makeAutostart =
    name:
    { config, ... }:
    {
      config.xdg.configFile."autostart/${name}.desktop".source = "${config.home.path}/share/applications/${name}.desktop";
    };
  orga-basics = [
    ./roles/mail.nix
    #./roles/taskwarrior.nix
  ];
  default = [
    ./roles/on-my-machine.nix
    ./roles/systemd-exporter.nix
  ];
  daily-driver =
    name: extra:
    let
      all =
        extra
        ++ orga-basics
        ++ default
        ++ [
          (makeAutostart "kassandra2")
          (makeAutostart "unlock-ssh")
          ./roles/beets.nix
          #./roles/conky
          ./roles/desktop-items.nix
          ./roles/desktop.nix
          ./roles/firefox.nix
          ./roles/git-sign.nix
          #./roles/gnome.nix
          ./roles/haskell-env.nix
          ./roles/kassandra.nix
          ./roles/khal.nix
          ./roles/khard.nix
          ./roles/rss-client.nix
          ./roles/mode-switching.nix
          ./roles/mpclient.nix
          ./roles/mpd.nix
          ./roles/mpv
          #./roles/night-shutdown.nix
          ./roles/pythia.nix
          #./roles/refresh-config.nix
          ./roles/research.nix
          ./roles/status-script.nix
          ./roles/terminal.nix
          ./roles/tinkering.nix
          ./roles/vdirsyncer.nix
          #./roles/wallpaper.nix
          ./roles/zettelkasten.nix
        ]
        ++ flake-inputs.self.nixFromDirs [ ./modules/clients ]
      ;
      blockServer = import ./roles/block-server.nix;
    in
    {
      klausur = makeConfig name (all ++ [ (blockServer restrictedPages) ]);
      code = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
          ./roles/chat.nix
          (blockServer newsPages)
        ]
      );
      leisure = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
          ./roles/games.nix
          ./roles/chat.nix
          (blockServer newsPages)
        ]
      );
      unrestricted = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
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
    ./roles/untrusted-env.nix
  ];
  zeus = daily-driver "zeus" (
    [
      (import ./roles/state.nix "klausur")
      ./roles/create-plans.nix
      ./roles/monitor-config
      ./roles/trusted-env.nix
    ]
    ++ flake-inputs.self.nixFromDirs [ ./modules/zeus ]
  );
  fluffy.default = makeConfig "fluffy" (
    default
    ++ [
      ./roles/headless.nix
      (import ./roles/state.nix "default")
    ]
  );
  hera.default = makeConfig "hera" (
    default
    ++ orga-basics
    ++ [
      ./roles/fetch-banking-timer.nix
      ./roles/weechat
      ./roles/mail-sort.nix
      ./roles/mail2rss.nix
      ./roles/headless-mpd.nix
      ./roles/headless.nix
    ]
  );
}
