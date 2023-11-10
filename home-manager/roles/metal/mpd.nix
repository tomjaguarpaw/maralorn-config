{
  pkgs,
  config,
  lib,
  ...
}:
let
  audio_dir = "${config.home.homeDirectory}/media/audio";
  playlist_dir = "${audio_dir}/playlists";
in
{
  systemd.user.services.mpd.Service.Restart = "on-failure";
  services = {
    mpd = {
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "PipeWire"
        }
      '';
      enable = true;
      musicDirectory = audio_dir;
      playlistDirectory = playlist_dir;
      network.listenAddress = "::";
    };
  };
  home = {
    file = {
      "media/audio/playlists" = {
        source = pkgs.recursiveLinkFarm "mpd-playlists" (
          lib.mapAttrs'
            (
              name: content:
              lib.nameValuePair "${name}.m3u" (builtins.toFile "${name}.m3u" content)
            )
            {
              "athene" = "http://athene.vpn.m-0.eu:8666";
              "radio-swiss-classic" = "https://stream.srg-ssr.ch/m/rsc_de/aacp_96";
              "radio-swiss-jazz" = "https://stream.srg-ssr.ch/m/rsj/aacp_96";
              "br-klassik" = "http://dispatcher.rndfnk.com/br/brklassik/live/mp3/high";
              "klassik-radio-games" = "https://klassikr.streamabc.net/klr-games-mp3-128-1540253";
              "klassik-radio-movie" = "https://klassikr.streamabc.net/klr-movie-mp3-128-5213277";
              "querfunk" = "http://mp3.querfunk.de/qfhi";
              "radio-dreyeckland" = "https://stream.rdl.de/rdl";
              "hr2-kultur" = "https://hlshr2.akamaized.net/hls/live/2016534/hr2/master.m3u8";
            }
        );
        recursive = true;
      };

      ".ncmpcpp/config".text = ''
        ask_before_clearing_playlists=no
        mouse_support = yes
        song_columns_list_format = "(24)[red]{a} $R(48)[blue]{t} (24)[green]{b} (4)[magenta]{l}"
        playlist_display_mode = columns
        search_engine_display_mode = columns
        browser_display_mode = columns
        user_interface = alternative
      '';
    };
    packages = [ pkgs.ncmpcpp ];
  };
}
