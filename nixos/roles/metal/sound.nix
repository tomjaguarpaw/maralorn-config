{
  # rtkit is optional but recommended
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  hardware.pulseaudio = {
    tcp.enable = true;
    zeroconf.discovery.enable = true;
  };
}
