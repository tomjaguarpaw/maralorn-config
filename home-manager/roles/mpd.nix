{
  pkgs,
  config,
  lib,
  ...
}:
let
  audio_dir = "${config.home.homeDirectory}/media/audio";
  playlist_dir = "${audio_dir}/playlists";
  mprisCfg = config.xdg.configFile."mpDris2/mpDris2.conf";
  replace_string = "@HOST@";
in
{
  home.file."media/audio/playlists" = {
    source = pkgs.recursiveLinkFarm "mpd-playlists" (
      lib.mapAttrs'
        (
          name: content:
          lib.nameValuePair "${name}.m3u" (builtins.toFile "${name}.m3u" content)
        )
        {
          "radio-swiss-classic" = "https://stream.srg-ssr.ch/m/rsc_de/aacp_96";
          "radio-swiss-jazz" = "https://stream.srg-ssr.ch/m/rsj/aacp_96";
          "br-klassik" = "http://dispatcher.rndfnk.com/br/brklassik/live/mp3/high";
          "klassik-radio-games" = "https://klassikr.streamabc.net/klr-games-mp3-128-1540253";
          "klassik-radio-movie" = "https://klassikr.streamabc.net/klr-movie-mp3-128-5213277";
          "radio-caprice-power-metal" = "http://79.120.77.11:8000/powermetal";
          "metal-hammer" = "https://metal-hammer.stream.laut.fm/metal-hammer";
        }
    );
    recursive = true;
  };
  xdg.configFile."mpDris2/mpDris2.conf".enable = false;
  services = {
    mpd = {
      enable = true;
      musicDirectory = audio_dir;
      playlistDirectory = playlist_dir;
      network.listenAddress = "::";
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "PipeWire"
        }
      '';
    };
    mpdris2 = {
      enable = true;
      mpd = {
        musicDirectory = lib.mkForce null;
        host = replace_string;
      };
    };
  };
  home.packages = [
    (pkgs.writeShellScriptBin "switch-mpd" ''
      mkdir -p ${config.xdg.configHome}/mpDris2
      ${
        lib.getExe pkgs.sd
      } -p ${replace_string} "$1" ${mprisCfg.source} > $HOME/${mprisCfg.target}
      ${pkgs.systemd}/bin/systemctl --user restart mpdris2.service
    '')
  ];
}
