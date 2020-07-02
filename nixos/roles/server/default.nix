{ config, pkgs, lib, ... }: {

  imports = [ ./init_ssh.nix ];

  nix = {
    #gc.automatic = true;
    optimise.automatic = true;
  };

}
