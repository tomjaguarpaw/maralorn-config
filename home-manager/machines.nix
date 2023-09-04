flake-inputs:
let
  restrictedPages = [
    "reddit.com"
    "github.com"
    "*.github.io"
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
    "code.maralorn.de"
    "chaos.social"
  ] ++ newsPages;
  newsPages = [
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

  makeConfig = hostName: imports: {
    imports =
      imports
      ++ [ ./roles/default.nix ]
      ++ flake-inputs.self.nixFromDirs [ ./modules/all ]
    ;
    m-0.hostName = hostName;
  };
  orga-basics = [
    ./roles/mail.nix
    ./roles/taskwarrior.nix
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
          ./roles/beets.nix
          ./roles/desktop.nix
          ./roles/firefox.nix
          ./roles/git-sign.nix
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
          ./roles/refresh-config.nix
          ./roles/research.nix
          ./roles/terminal.nix
          ./roles/tinkering.nix
          ./roles/vdirsyncer.nix
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
      orga = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
          ./roles/chat.nix
          (blockServer restrictedPages)
        ]
      );
      unrestricted = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
          ./roles/chat.nix
          (blockServer [ ])
        ]
      );
      leisure = makeConfig name (
        all
        ++ [
          ./roles/mail-client.nix
          ./roles/chat.nix
          (blockServer newsPages)
        ]
        ++ (
          if name == "zeus" then
            flake-inputs.self.nixFromDirs [ ./modules/gaming ]
          else
            [ ]
        )
      );
    };
in
{
  apollo = daily-driver "apollo" (
    flake-inputs.self.nixFromDirs [ ./modules/apollo ]
  );

  zeus = daily-driver "zeus" (
    [ ./roles/create-plans.nix ]
    ++ flake-inputs.self.nixFromDirs [
      ./modules/impermanent
      ./modules/zeus
    ]
  );
  fluffy.default = makeConfig "fluffy" (
    default
    ++ [ ./roles/headless.nix ]
    ++ flake-inputs.self.nixFromDirs [ ./modules/impermanent ]
  );
  hera.default = makeConfig "hera" (
    default
    ++ orga-basics
    ++ [
      ./roles/weechat
      ./roles/mail-sort.nix
      ./roles/mail2rss.nix
      ./roles/headless.nix
    ]
  );
}
