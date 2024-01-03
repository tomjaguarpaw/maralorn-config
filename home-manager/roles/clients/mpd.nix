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
      rm -r $HOME/${mprisCfg.target}
      cp ${mprisCfg.source} $HOME/${mprisCfg.target}
      ${lib.getExe pkgs.sd} ${replace_string} "$1" $HOME/${mprisCfg.target}
      ${pkgs.systemd}/bin/systemctl --user restart mpdris2.service
    '')
  ];
}
