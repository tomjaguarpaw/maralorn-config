{ pkgs, lib, config, ...}:
with lib;

let
  makeScripts = name:
  let
    knownHosts = pkgs.writeText "KnownBootHosts" ''
      hera.m-0.eu,213.136.94.190 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt
    '';
  in
    pkgs.writeShellScriptBin "unlock-${name}" ''
    ${pkgs.pass}/bin/pass eu/m-0/${name}/disk | ssh -4 root@${name}.m-0.eu -o UserKnownHostsFile=${knownHosts} cryptsetup-askpass
  '';
in
{

options.m-0.unlocker = mkOption {
  default = [];
  type = types.listOf types.str;
};

config = {
  home.packages = map makeScripts config.m-0.unlocker;
};

}
