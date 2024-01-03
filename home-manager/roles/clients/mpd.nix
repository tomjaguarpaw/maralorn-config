{
  pkgs,
  config,
  lib,
  ...
}:
let
  mprisCfg = config.xdg.configFile."mpDris2/mpDris2.conf";
in
{
  services.mpdris2 = {
    enable = true;
    mpd.host = "::";
  };
  home.packages = [
    (pkgs.writeShellScriptBin "switch-mpd" ''
      mkdir -p ${config.xdg.configHome}/mpDris2
      rm -f $HOME/${mprisCfg.target}
      cp ${mprisCfg.source} $HOME/${mprisCfg.target}
      ${lib.getExe pkgs.sd} "host = ::" "host = $1" $HOME/${mprisCfg.target}
      ${pkgs.systemd}/bin/systemctl --user restart mpdris2.service
    '')
  ];
}
