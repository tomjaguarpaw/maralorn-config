{
  # rtkit is optional but recommended
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
  };
  #services.pipewire.enable = lib.mkForce false;
  #sound.enable = true;
  #hardware = {
  #  pulseaudio = {
  #    package = pkgs.pulseaudioFull;
  #    enable = true;
  #  };
  #};
}
