{ config, pkgs, lib, ... }:
with lib;

let

page = pkgs.stdenv.mkDerivation {
  name = "blog.maralorn.de";
  src = builtins.fetchGit "git@hera:blog";
  buildInputs = [ (pkgs.python3.withPackages (ps: [ps.pelican ps.markdown])) ];
  LC_ALL="en_US.UTF-8";
  LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive";
  buildPhase = ''
    make html
  '';
  installPhase = ''
    mkdir $out
    cp -r output/* $out
  '';
};

in
{

options = {
  m-0.blog = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };
};

config = mkIf config.m-0.blog.enable {
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services = {
    nginx = {
      enable = true;
      virtualHosts."blog.maralorn.de" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            root = "${page}";
            index = "index.html";
          };
        };
      };
    };
  };
};

}
