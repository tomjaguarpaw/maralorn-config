{ config, pkgs, lib, ... }:
with lib;
let
  dag = config.lib.dag;
  nvd = import pkgs.sources.nvd { inherit pkgs; };
in
{
  home.activation.report-changes = dag.entryAnywhere "${nvd}/bin/nvd $oldGenPath $newGenPath";
}
