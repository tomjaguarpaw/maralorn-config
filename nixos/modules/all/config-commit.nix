{ pkgs, lib, ... }:
let
  inherit (pkgs.flake-inputs.self) sourceInfo;
in
{
  system.systemBuilderCommands = lib.mkIf (sourceInfo ? rev) ''
    echo ${sourceInfo.rev} > $out/config-commit
  '';
}
