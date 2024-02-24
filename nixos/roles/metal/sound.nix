{
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };
  hardware.pulseaudio = {
    tcp.enable = true;
    zeroconf.discovery.enable = true;
  };
}
