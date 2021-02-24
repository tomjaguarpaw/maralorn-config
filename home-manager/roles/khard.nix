{ pkgs, ... }: {
  home = {
    packages = [ pkgs.khard ];
    file.".config/khard/khard.conf".text = ''
      [addressbooks]
      [[Kontakte]]
      path = ~/.contacts/nextcloud/Kontakte

      [general]
      debug = no
      default_action = list
    '';
  };
}
