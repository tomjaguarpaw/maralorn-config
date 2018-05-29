{ pkgs, ... }:
let
  jali = with pkgs.python36Packages; buildPythonApplication rec {
    name = "${pname}-${version}";
    pname = "jali";
    doCheck = false;
    version = "1d1c5d0a";
    src = pkgs.fetchgit {
      url = "https://git.darmstadt.ccc.de/jali/jali.git";
      rev = version;
      sha256 = "1nzzangp7yr2gq66qz7wk2cqqwjlhrfaqmc85qigjv4vpfmlphl0";
    };
    propagatedBuildInputs = with pkgs; [ jinja2 pendulum GitPython aqbanking ];
  };
in

{

  imports = [
    ../../home-manager
    ../../home-common/my-systems.nix
    ../../home-common/graphical
    ../../home-common/latex.nix
    ./battery.nix
    ./sleep-nag.nix
  ];

  programs = {
    firefox = {
      enable = true;
    };
    git = {
      signing = {
        signByDefault = true;
        key = "6C3D12CD88CDF46C5EAF4D12226A2D41EF5378C9";
      };
    };
  };
  home.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
  };

  services = {
    udiskie = {
      enable = true;
      notify = true;
    };
  };

  home.packages = with pkgs.gnome3; [
      glade
    ] ++ (with pkgs; [
    # web
    chromium
    signal-desktop
    tdesktop
    acpi
    dino

    rustracer

    arandr
    qutebrowser

    mumble

    xorg.xev
    xorg.xbacklight
    meld

    icedtea8_web

    hledger
    haskellPackages.hledger-ui
    ledger
    jali

    # tools & office
    feh
    gimp
    imagemagick
    libreoffice-fresh
    pandoc
    xournal
    musescore
    handbrake
    octave

    # look & feel
    libertine
    nerdfonts
    dina-font
    envypn-font
    google-fonts
    gnome3.gnome-font-viewer
    unscii
    xfontsel

    # media
    ncmpcpp
    pavucontrol
    deluge
    mpd
    gmpc
    calibre
    mpv
  ]);
}
