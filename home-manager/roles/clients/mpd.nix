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
    (pkgs.writeShellScriptBin "switch-mpd" (
      let
        target = "$HOME/${mprisCfg.target}";
      in
      ''
        mkdir -p ${config.xdg.configHome}/mpDris2
        rm -f ${target} ${target}.home-manager-backup
        cp ${mprisCfg.source} ${target}
        ${lib.getExe pkgs.sd} "host = ::" "host = $1" ${target}
        ${pkgs.systemd}/bin/systemctl --user restart mpdris2.service
      ''
    ))
  ];
}
