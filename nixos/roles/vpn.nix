{config, ...}: {
  services.tailscale.enable = true;
  networking = {
    firewall = {
      extraInputRules = ''
        meta iifname m0wire accept comment "wireguard vpn"
        meta iifname tailscale0 accept comment "headscale vpn"
      '';
      checkReversePath = "loose";
      allowedUDPPorts = [config.services.tailscale.port];
    };
  };
}
