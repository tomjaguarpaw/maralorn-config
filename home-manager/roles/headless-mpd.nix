{...}: {
  services = {
    mpd = {
      enable = true;
      network.listenAddress = "::1";
      musicDirectory = "/media/audio";
      extraConfig = ''
        audio_output {
         type "httpd"
         name "HTTP Stream"
         encoder "vorbis"
         port "8666"
         bitrate "192"
         format "44100:16:1"
         always_on "yes" # prevent MPD from disconnecting all listeners when playback is stopped.
         tags "yes" # httpd supports sending tags to listening streams.
        }
      '';
    };
  };
}
