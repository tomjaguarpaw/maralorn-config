{ config, pkgs, ... }:

{
  system.stateVersion = "17.03";

  networking = {
    hostName = "charon.olymp.space";

    interfaces.ens3 = {
      ipv4.addresses = [{ address = "45.32.154.139"; prefixLength = 22; }];
      ipv6.addresses = [{ address = "2001:19f0:6c01:b0d::1"; prefixLength = 64; }];
    };
    defaultGateway = "45.32.152.1";
    nameservers = [ "108.61.10.10" "2001:19f0:300:1704::6" ];
  };

  imports = [
    ./hardware-configuration.nix
    ../../host-common/common.nix
    ../../host-common/init_ssh.nix
    /etc/nixos/local/config.nix
    ./dav.nix
    ./mail.nix
    ./matrix.nix
    ./web.nix
  ];

  boot.initrd.network.postCommands = "ip address add 2001:19f0:6c01:b0d::b007/64 dev eth0";
  boot.initrd.postMountCommands = "ip link set eth0 down";

  users.users = {
    choreutes = {
      description = "Tobias Schmalz";
      isNormalUser = true;
      passwordFile = "/etc/nixos/local/pw-choreutes";
    };
    swantje = {
      description = "Swantje Mahncke";
      isNormalUser = true;
      passwordFile = "/etc/nixos/local/pw-swantje";
    };
  };
  services = {

    # Taskserver
    # taskserver = {
    #   enable = true;
    #   fqdn = config.networking.hostName;
    #   listenHost = "::";
    #   organisations.users.users = [ "maralorn" ];
    #   config = { request.limit = 0; };
    # };
  };

  boot.loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/vda";
  };

}
