{ pkgs, ... }:
{

  imports = [
    ../../home-common
    ../../home-common/my-systems.nix
    ../../home-common/graphical
    ../../home-common/latex.nix
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
    acpi

    arandr
    qutebrowser

    mumble

    # tools & office
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
