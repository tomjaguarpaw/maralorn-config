{
  networking.firewall = {
    allowedTCPPorts = [
      6600 # mpd
    ];
    allowedUDPPorts = [
      34197 # factorio
    ];
  };
}
