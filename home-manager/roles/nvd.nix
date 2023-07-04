{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (config.lib) dag;
in
{
  home.activation.report-changes = dag.entryAnywhere ''
    if [[ -v oldGenPath && "$oldGenPath" != "$newGenPath" ]]; then
      ${lib.getExe pkgs.nvd} diff $oldGenPath $newGenPath
    fi
  '';
}
