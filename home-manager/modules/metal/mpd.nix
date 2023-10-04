{
  services.mpd.extraConfig = ''
    audio_output {
      type "pipewire"
      name "PipeWire"
    }
  '';
}
