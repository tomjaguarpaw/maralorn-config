{ pkgs, lib, config, ...}:
with lib;

let
  makeUnlocker = { name, hostName, pubKey, passPath }:
  let
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
    ${pkgs.pass}/bin/pass ${passPath} | ssh -4 root@${hostName} -o UserKnownHostsFile=${knownHosts} cryptsetup-askpass && echo "Unlocking of ${name} successful" || echo "Unlockung of ${name} failed"
  '';
in
{

options.m-0.unlocker = mkOption {
  default = [];
  type = types.listOf types.attrs;
};

config = {
  home.packages = map makeUnlocker config.m-0.unlocker;
};

}
