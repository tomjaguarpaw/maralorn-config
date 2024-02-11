{
  networking = {
    domain = "lo.m-0.eu";
    networkmanager.enable = false;
    interfaces.enp34s0 = {
      useDHCP = true;
      ipv6.addresses = [
        {
          address = "fdc0:1::4";
          prefixLength = 64;
        }
      ];
    };
  };
}
