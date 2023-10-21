{ pkgs, ... }:
{
  home.packages = [ pkgs.khard ];
  xdg.configFile."khard/khard.conf".text = ''
    [addressbooks]
    [[Kontakte]]
    path = ~/.contacts/nextcloud/Kontakte

    [general]
    debug = no
    default_action = list
  '';
}
