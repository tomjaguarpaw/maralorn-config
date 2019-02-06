{ config, pkgs, lib, ... }:
with lib;

let

me = config.m-0.private.me;
page = pkgs.stdenv.mkDerivation {
  name = "mathechor.de";
  src = builtins.fetchGit "git@hera:mathechor.de";
  buildInputs = [ pkgs.pandoc pkgs.python3 ];
  LC_ALL="en_US.UTF-8";
  LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  installPhase = ''
    mkdir $out
    cp -r intern/output $out/intern
    cp -r public/output $out/public
  '';
};

in
{

options = {
  m-0.mathechor-de = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    password = mkOption {
      type = types.str;
    };
  };
};

config = mkIf config.m-0.mathechor-de.enable {
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."mathechor.de" = {
        serverAliases = ["www.mathechor.de"];
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "${page}/public";
            index = "index.html";
            extraConfig = "location ~* \.(otf)$ {add_header Access-Control-Allow-Origin *;}";
          };
        };
      };
      virtualHosts."intern.mathechor.de" = {
        forceSSL = true;
        enableACME = true;
        basicAuth.mathechor = config.m-0.mathechor-de.password;
        locations = {
          "/" = {
            root = "${page}/intern";
            index = "index.html";
          };
        };
      };
    };
  };
};

}
