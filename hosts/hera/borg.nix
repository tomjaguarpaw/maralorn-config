{ config, ... }:
let me = config.m-0.private.me;
in {
  containers.borg = {
    autoStart = true;
    privateNetwork = true;
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      imports = [ ../../system ];

      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [{
            address = config.m-0.hosts.borg;
            prefixLength = 112;
          }];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = {
          address = config.m-0.hosts.hera-intern;
          interface = "eth0";
        };
      };

      services = {
        borgbackup.repos.backups = {
          authorizedKeys = me.keys;
          authorizedKeysAppendOnly = me.backupkeys;
          quota = "150G";
        };
        sshd.enable = true;
      };
    };
  };
}
