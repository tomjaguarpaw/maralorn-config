{
  pkgs,
  config,
  lib,
  ...
}:
let
  mprisCfg = config.xdg.configFile."mpDris2/mpDris2.conf";
  replace_string = "@HOST@";
in
{
  xdg.configFile."mpDris2/mpDris2.conf".enable = false;
  services.mpdris2 = {
    enable = true;
    mpd = {
      musicDirectory = lib.mkForce null;
      host = replace_string;
    };
  };
  home.packages = [
    (pkgs.writeShellScriptBin "switch-mpd" ''
      mkdir -p ${config.xdg.configHome}/mpDris2
      ${lib.getExe pkgs.sd} -p ${replace_string} "$1" ${mprisCfg.source} > $HOME/${mprisCfg.target}
      ${pkgs.systemd}/bin/systemctl --user restart mpdris2.service
    '')
  ];
}
