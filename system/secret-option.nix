{ config, pkgs, lib, ... }:
with lib;
{
  options = {
    m-0.secrets = mkOption {
      default = {};
      type = types.attrs;
    };
  };
}
