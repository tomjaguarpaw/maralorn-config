{config, lib, ...}:
let
  inherit (lib) mkOption types;
in
{

  options.m-0.server = {
    initrd-ssh = {
      key = mkOption {type = types.path;};
      ip-config = mkOption {
        type = types.str;
        default = "dhcp";
      };
      networkingModules = mkOption {type = types.listOf types.str;};
    };
  };

  config.boot = {
    kernelParams = ["ip=${config.m-0.server.initrd-ssh.ip-config}"];
    initrd = {
      kernelModules = config.m-0.server.initrd-ssh.networkingModules;
      network = {
        enable = true;
        ssh = {
          enable = true;
          shell = "/bin/cryptsetup-askpass";
          authorizedKeys = config.users.users.root.openssh.authorizedKeys.keys;
          hostKeys = [config.m-0.server.initrd-ssh.key];
        };
      };
    };
  };
}
