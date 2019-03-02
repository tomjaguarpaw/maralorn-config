{ config, pkgs, lib, ... }:
with lib;

let
  cfg = config.m-0.riot;
in
{

options = {
  m-0.riot = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    hostname = mkOption {
      type = types.str;
    };
    config = mkOption {
      type = types.attrs;
    };
  };
};

config = mkIf cfg.enable {
  services = {
    nginx = {
      enable = true;
      virtualHosts."${cfg.hostname}" = {
        enableACME = true;
        forceSSL = true;
        root = pkgs.unstable.riot-web;
        locations."/config.json" = {
          extraConfig = ''
            default_type application/json;
            return 200 '${builtins.toJSON cfg.config}';
          '';
        };
      };
    };
  };
};

}
