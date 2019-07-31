{ pkgs , config , lib, ... }:
{

  xsession.initExtra = let
    cat-pw = pkgs.writeShellScriptBin "cat-ssh-pw" ''
      pass eu/m-0/${config.m-0.hostName}/ssh
    '';
    start-agent = pkgs.writeShellScriptBin "start-ssh-agent" ''
      ${pkgs.psmisc}/bin/killall -q ssh-agent
      eval `${pkgs.openssh}/bin/ssh-agent -s`
      systemctl --user set-environment SSH_AUTH_SOCK="$SSH_AUTH_SOCK"
      systemctl --user set-environment SSH_AGENT_PID="$SSH_AGENT_PID"
      SSH_ASKPASS=${cat-pw}/bin/cat-ssh-pw ${pkgs.openssh}/bin/ssh-add & < /dev/null
    '';
  in
    ". ${start-agent}/bin/start-ssh-agent";

}
