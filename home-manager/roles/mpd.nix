{ pkgs, config, ... }: {
  services = {
    mpd = {
      enable = true;
      musicDirectory = "${config.home.homeDirectory}/media/audio";
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
