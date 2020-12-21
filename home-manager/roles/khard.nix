{ pkgs, ... }: {
  home = {
    packages = [ pkgs.khard ];
    file.".config/khard/khard.conf".text = ''
      [addressbooks]
      [[contacts]]
      path = ~/.contacts/nextcloud/contacts

      [general]
      debug = no
      default_action = list
    '';
  };
}
