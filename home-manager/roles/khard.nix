{ pkgs, ... }: {
  home = {
    packages = [ pkgs.khard ];
    file.".config/khard/khard.conf".text = ''
      [addressbooks]
      [[notmuch]]
      path = ~/.contacts/nextcloud/contacts

      [general]
      debug = no
      default_action = list
    '';
  };
}
