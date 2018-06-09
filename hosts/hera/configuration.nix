{ config, pkgs, ... }:

# You need pw-files for every configured user in ./secret/pw-useralias for login to work.

let
  me = config.m-0.private.me;
  unstable-pkgs = import <unstable> {};
in {

imports = [
 ./hardware-configuration.nix
 ../../system
 ./git.nix
 ./borg.nix
 <unstable/nixos/modules/services/networking/ndppd.nix>
];

networking = {
  hostName = "hera";
  interfaces.ens18 = {
    proxyARP = true;
    ipv4.addresses = [{ address = "213.136.94.190"; prefixLength = 24; }];
    ipv6.addresses = [{ address = config.m-0.hosts.hera; prefixLength = 128; }];
  };
  defaultGateway = "213.136.94.1";
  defaultGateway6 = { address = "fe80::1"; interface = "ens18"; };

  bridges.bridge.interfaces = [ ];
  interfaces.bridge = {
    proxyARP = true;
    ipv6.addresses = [{ address = config.m-0.hosts.hera-intern; prefixLength = 112; }];
  };
  nameservers = [ "213.136.95.10" "2a02:c207::1:53" "2a02:c207::2:53" ];
};

nixpkgs.config.packageOverrides = pkgs: {
  ndppd = unstable-pkgs.ndppd;
};

services = {
  ndppd = {
    enable = true;
    interface = "ens18";
    network = "${config.m-0.prefix}::/64";
  };
  borgbackup.jobs.data = {
    doInit = false;
    encryption.mode = "none";
    paths = "/home/${me.user}/data";
    repo = "borg@borg:.";
  };
};

m-0 = {
  # dropbearkey -t rsa -f /etc/nixos/hosts/<hostname>/secret/boot_rsa
  server.enable = true;
  standalone.enable = true;
};

home-manager.users."${me.user}" = (import ./home.nix);

# Use the systemd-boot EFI boot loader.
boot = {
  loader = {
    grub = {
      enable = true;
      version = 2;
      device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0";
    };
  };
  supportedFilesystems = [ "exfat" ];
  kernelParams = [ "ip=213.136.94.190::213.136.94.1:255.255.255.0:hera" ];
  initrd = {
    postMountCommands = ''
      ip address flush dev eth0
      ip link set eth0 down
    '';
    luks.devices = [
    {
      name = "root";
      device = "/dev/disk/by-uuid/536fe284-36f2-425c-b0c5-a737280f9470";
      preLVM = true;
      allowDiscards = true;
    }
    ];
  };
};

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?

}
