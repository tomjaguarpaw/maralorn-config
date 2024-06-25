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
  # services.jack.jackd.enable = true;
  # support ALSA only programs via ALSA JACK PCM plugin
  # alsa.enable = false;
  # support ALSA only programs via loopback device (supports programs like Steam)
  # loopback = {
  #   enable = true;
  #   # buffering parameters for dmix device to work with ALSA only semi-professional sound programs
  #   #dmixConfig = ''
  #   #  period_size 2048
  #   #'';
  # };

  users.extraUsers.maralorn.extraGroups = [ "jackaudio" ];
}
