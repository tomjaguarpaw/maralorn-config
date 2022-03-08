{pkgs, ...}: let
  makeUnlocker = {
    name,
    hostName,
    pubKey,
    passPath,
  }: let
    knownHosts = pkgs.writeText "KnownBootHosts" "${hostName} ${pubKey}";
  in
    pkgs.writeShellScriptBin "unlock-${name}" ''
      echo "Waiting for host to come up";
      while true; do
        echo -n .
        /run/wrappers/bin/ping -4 ${hostName} -c 1 -w 1 > /dev/null && break;
        sleep 1s;
      done;
      echo
      echo "Ping successful; Entering disk encryption password"
      ${pkgs.pass}/bin/pass ${passPath} | ssh -4 root@${hostName} -o UserKnownHostsFile=${knownHosts} cryptsetup-askpass && echo "Unlocking of ${name} successful" || echo "Unlocking of ${name} failed"
    '';
  unlocker = [
    {
      name = "hera";
      hostName = "hera-v4";
      pubKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt";
      passPath = "eu/m-0/hera.m-0.eu/disk";
    }
  ];
in {config = {home.packages = map makeUnlocker unlocker;};}
