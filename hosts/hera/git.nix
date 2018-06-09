{ config, ... }:
let
  me = config.m-0.private.me;
in {
  containers.git = {
    autoStart = true;
    privateNetwork = true;
    bindMounts = {
      "/home/git" = { hostPath = "/home/maralorn/data/git"; isReadOnly = false; };
    };
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      imports = [../../system];

      users.users.git = {
        isNormalUser = true;
        uid = 1000;
        openssh.authorizedKeys.keys = me.keys;
      };
      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [{ address = config.m-0.hosts.git; prefixLength = 64; }];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = { address = config.m-0.hosts.hera-intern; interface = "eth0"; };
      };

      environment.systemPackages = [ pkgs.git ];

      services = {
        sshd.enable = true;
      };
    };
  };
}
