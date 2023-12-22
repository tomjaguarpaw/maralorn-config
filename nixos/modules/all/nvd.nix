{ pkgs, lib, ... }:
{
  system.activationScripts.diff = {
    supportsDryActivation = true;
    text = ''
      ${lib.getExe pkgs.nvd} --nix-bin-dir=${lib.getBin pkgs.nix}/bin diff /run/current-system "$systemConfig"
    '';
  };
}
