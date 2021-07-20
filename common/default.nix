{ config, pkgs, lib, ... }:

with lib;

{

  config = {
    m-0.monitoring = [
      {
        host = "apollo:9100";
        name = "apollo";
        flaky = true;
      }
      {
        host = "apollo:9558";
        name = "apollo-user";
        flaky = true;
      }
      {
        host = "zeus:9100";
        name = "zeus";
        flaky = true;
      }
      {
        host = "zeus:9558";
        name = "zeus-user";
        flaky = true;
      }
      {
        name = "ved server";
        host = "bach.vocalensemble-darmstadt.de:9100";
      }
      {
        name = "ved postfix";
        host = "bach.vocalensemble-darmstadt.de:9154";
      }
    ];
  };

  options = {
    m-0.private = mkOption {
      default = { };
      type = types.attrs;
    };
    m-0.prefix = mkOption {
      default = "2a02:c207:3002:7584";
      type = types.str;
    };
    m-0.monitoring = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption { type = types.str; };
          host = mkOption { type = types.str; };
          container = mkOption {
            type = types.bool;
            default = false;
          };
          flaky = mkOption {
            type = types.bool;
            default = false;
          };
        };
      });
      default = [ ];
    };
    m-0.hosts = mkOption {
      type = types.attrs;
      default =
        let
          p = config.m-0.prefix;
          hera-p = "${p}::3";
          apollo-p = "${p}::1";
          wg-p = "${p}::100";
          v4-p = "10.0.0";
        in
        rec {
          hera = "${p}::1";
          hera-wg-host = "${p}::100:0:1";

          hera-v4 = "213.136.94.190";

          hera-wg = "${wg-p}:1";
          apollo-wg = "${wg-p}:2";
          zeus-wg = "${wg-p}:3";

          hera-intern = "${hera-p}:1";
          chor-cloud = "${hera-p}:b";

          apollo = apollo-wg;
          zeus = zeus-wg;

          hera-intern-v4 = "${v4-p}.1";
          chor-cloud-intern-v4 = "${v4-p}.3";
        };
    };
  };

}
