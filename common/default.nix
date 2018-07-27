{ config, pkgs, lib, ... }:
with lib;
{
  imports = [ ./secret ];
  options = {
    m-0.private = mkOption {
      default = {};
      type = types.attrs;
    };
    m-0.prefix = mkOption {
      default = "2a02:c207:3002:7584";
      type = types.str;
    };
    m-0.hosts = mkOption {
      type = types.attrs;
      default = let p = config.m-0.prefix; in {
        hera = "${p}::1";
        hera-intern = "${p}::3:1";
        git = "${p}::3:2";
        borg = "${p}::3:3";
        dav = "${p}::3:5";
        blog = "${p}::3:6";
        chor = "${p}::3:7";
        matrix = "${p}::3:8";
        apollo = "${p}::1:1";
        athene = "${p}::2:1";
      };
    };
  };
}
