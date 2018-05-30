let
in
{
  users.users = {
    "${me}" = {
      description = "${me.name}";
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "systemd-journal" "networkmanager" "docker" ];
      openssh.authorizedKeys.keys = keys;
      passwordFile = "${me.pw-file}";
    };
    root = {
      openssh.authorizedKeys.keys = keys;
      passwordFile = "${me.pw-file}";
    };
  };
}
