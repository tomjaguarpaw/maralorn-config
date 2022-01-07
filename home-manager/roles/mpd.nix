{ pkgs, config, lib, ... }: {
  services = {
    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/media/audio";
      playlistDirectory = pkgs.setToDirectories (lib.mapAttrs' (name: content: lib.nameValuePair "${name}.m3u" (builtins.toFile "${name}.m3u" content)) {
        "radio-swiss-classic" = "https://stream.srg-ssr.ch/m/rsc_de/aacp_96";
        "radio-swiss-jazz" = "https://stream.srg-ssr.ch/m/rsj/aacp_96";
        "br-klassik" = "http://dispatcher.rndfnk.com/br/brklassik/live/mp3/high";
        "klassik-radio-games" = "https://klassikr.streamabc.net/klr-games-mp3-128-1540253";
        "klassik-radio-movie" = "https://klassikr.streamabc.net/klr-movie-mp3-128-5213277";
        "radio-caprice-power-metal" = "http://79.120.77.11:8000/powermetal";
        "metal-hammer" = "https://metal-hammer.stream.laut.fm/metal-hammer";
      });
      extraConfig = ''
        audio_output {
              type "pulse"
              name "Pipewire"
        }
      '';
    };
    mpdris2.enable = true;
  };
}
