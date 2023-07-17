{ config, pkgs, ... }:
{
  services.emanote = {
    enable = true;
    # host = "127.0.0.1"; # default listen address is 127.0.0.1
    # port = 7000;        # default http port is 7000
    notes = [
      "${config.home.homeDirectory}/git/notes" # add as many layers as you like
    ];
    package = pkgs.emanote;
  };
}
