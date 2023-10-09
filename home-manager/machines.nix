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
  orga-basics = [ ./roles/mail.nix ];
  default = [
    ./roles/on-my-machine.nix
    ./roles/systemd-exporter.nix
    flake-inputs.nix-index-database.hmModules.nix-index
  ];
  daily =
    orga-basics
    ++ default
    ++ [
      ./roles/beets.nix
      ./roles/desktop.nix
      ./roles/firefox.nix
      ./roles/haskell-env.nix
      ./roles/kassandra.nix
      ./roles/khal.nix
      ./roles/khard.nix
      ./roles/mode-switching.nix
      ./roles/mpv
      #./roles/night-shutdown.nix
      ./roles/research.nix
      ./roles/terminal.nix
      ./roles/tinkering.nix
      ./roles/vdirsyncer.nix
    ]
    ++ flake-inputs.self.nixFromDirs [ ./modules/clients ]
  ;
  blockServer = import ./roles/block-server.nix;
  daily-driver =
    name: extra:
    let
      all = extra ++ daily;
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
    flake-inputs.self.nixFromDirs [
      ./modules/apollo
      ./modules/metal
    ]
  );

  zeus = daily-driver "zeus" (
    flake-inputs.self.nixFromDirs [
      ./modules/impermanent
      ./modules/zeus
      ./modules/metal
    ]
  );
  fluffy.default = makeConfig "fluffy" (
    default
    ++ [ ./roles/headless.nix ]
    ++ flake-inputs.self.nixFromDirs [
      ./modules/impermanent
      ./modules/metal
    ]
  );
  hephaistos.default = makeConfig "hephaistos" (
    flake-inputs.self.nixFromDirs [
      ./modules/impermanent
      ./modules/hephaistos
      ./modules/metal
      ./modules/laptops
    ]
    ++ daily
    ++ [
      ./roles/mail-client.nix
      ./roles/chat.nix
      (blockServer restrictedPages)
    ]
  );
  hera.default = makeConfig "hera" (
    flake-inputs.self.nixFromDirs [ ./modules/hera ]
    ++ default
    ++ orga-basics
    ++ [
      ./roles/weechat
      ./roles/mail-sort.nix
      ./roles/mail2rss.nix
      ./roles/headless.nix
    ]
  );
}
