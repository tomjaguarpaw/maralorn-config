{ config, pkgs, lib, ... }:
with lib;
let
  dag = config.lib.dag;
in
{
  home.activation.report-changes = dag.entryAnywhere "${pkgs.nvd}/bin/nvd diff $oldGenPath $newGenPath";
}
