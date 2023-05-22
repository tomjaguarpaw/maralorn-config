{ config, pkgs, ... }:
let inherit (config.lib) dag;
in {
  home.activation.report-changes = dag.entryAnywhere ''
     if [[ -n "$oldGenPath" && "$oldGenPath" != "$newGenPath" ]]; then
    ${pkgs.nvd}/bin/nvd diff $oldGenPath $newGenPath
     fi
  '';
}
