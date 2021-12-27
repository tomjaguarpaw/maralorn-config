{ pkgs, config, lib, ... }: {
  services = {
    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/media/audio";
      playlistDirectory = pkgs.setToDirectories (lib.mapAttrs' (name: content: lib.nameValuePair "${name}.m3u" (builtins.toFile "${name}.m3u" content)) {
        "radio-swiss-classic" = "https://stream.srg-ssr.ch/m/rsc_de/aacp_96";
        "radio-swiss-jazz" = "https://stream.srg-ssr.ch/rsj/aacp_96";
        "br-klassik" = "http://dispatcher.rndfnk.com/br/brklassik/live/mp3/high";
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
