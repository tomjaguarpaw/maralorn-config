{ lib, pkgs, config, ...}:
with lib;
{

options.m-0.laptop.enable = mkEnableOption "Laptop";

config = let
  rewlan = pkgs.writeShellScriptBin "rewlan" ''
    nmcli r wifi off;
    sleep 0.1s;
    nmcli r wifi on;
  '';
in
mkIf config.m-0.laptop.enable {
  home.file.".ncmpcpp/config".text = ''
    ask_before_clearing_playlists=no
    mouse_support = yes
    song_columns_list_format = "(24)[red]{a} $R(48)[blue]{t} (24)[green]{b} (4)[magenta]{l}"
    playlist_display_mode = columns
    search_engine_display_mode = columns
    browser_display_mode = columns
    user_interface = alternative
  '';
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
    network-manager-applet.enable = true;
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
    rewlan

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
    evince
    gnome3.nautilus

    #dev
    octave
    stack
    cabal2nix
    filezilla
    vscode

    # look & feel
    libertine
    nerdfonts

    networkmanagerapplet

    # media
    ncmpcpp
    ncpamixer
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
