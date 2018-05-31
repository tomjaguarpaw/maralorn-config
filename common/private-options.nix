{ config, pkgs, lib, ... }:
with lib;
{
  options = {
    m-0.private = mkOption {
      default = {};
      type = types.attrs;
    };
  };
}
