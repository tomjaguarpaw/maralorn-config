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
      default = let
        p = config.m-0.prefix;
        hera-p = "${p}::3";
        apollo-p = "${p}::1";
        wg-p = "${p}::100";
      in {
        hera-wg = "${wg-p}::1";
        apollo-wg = "${wg-p}::2";

        hera-v4 = "213.136.94.190";
        hera = "${p}::1";
        hera-intern = "${hera-p}:1";
        git = "${hera-p}:2";
        borg = "${hera-p}:3";
        dav = "${hera-p}:5";
        blog = "${hera-p}:6";
        chor = "${hera-p}:7";
        matrix = "${hera-p}:8";

        apollo = apollo-wg;
      };
    };
  };
}
