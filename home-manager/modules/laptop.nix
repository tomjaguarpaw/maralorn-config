{ lib, pkgs, config, ...}:
with lib;
{

options.m-0.laptop.enable = mkEnableOption "Laptop";

config = mkIf config.m-0.laptop.enable {
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

  home.packages = with pkgs; [
    # web
    chromium

    # communication
    signal-desktop
    tdesktop
    acpi
    dino
    mumble

    # config
    arandr
    xorg.xev
    xorg.xbacklight

    #dev
    meld
    icedtea8_web

    # tools & office
    feh
    gimp
    imagemagick
    ghostscript
    libreoffice-fresh
    pandoc
    xournal
    musescore
    handbrake

    #dev
    octave
    stack
    cabal2nix
    filezilla
    vscode

    # look & feel
    libertine
    nerdfonts

    # media
    spotify
    ncmpcpp
    pavucontrol
    deluge
    mpd
    gmpc
    calibre
    mpv
    youtubeDL
  ];
};

}
