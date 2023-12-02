{pkgs, lib, ...}:
let
  dhallFiles = pkgs.runCommand "kassandra-config-src" {} ''
    mkdir $out
    ${lib.getExe pkgs.kassandra-standalone} print-types > $out/types.dhall
    ln -s ${./kassandra}/{config,backend}.dhall $out
    ln -s ${pkgs.privateFile "kassandra-uiConfig.dhall"} $out/uiConfig.dhall
  '';
  backend = pkgs.dhallPackages.buildDhallPackage {
    name = "kassandra-backend-config";
    code = "${dhallFiles}/backend.dhall : (${dhallFiles}/types.dhall).BackendConfig";
    source = true;
    dependencies = [pkgs.dhallPackages.Prelude];
  };
  standalone = pkgs.dhallPackages.buildDhallPackage {
    name = "kassandra-standalone-config";
    code = "${dhallFiles}/config.dhall : (${dhallFiles}/types.dhall).StandaloneConfig";
    source = true;
    dependencies = [pkgs.dhallPackages.Prelude];
  };
  dhallResult = pkgs.recursiveLinkFarm "kassandra-config" {
    "backend.dhall" = "${backend}/source.dhall";
    "config.dhall" = "${standalone}/source.dhall";
  };
in
{
  xdg.configFile.kassandra.source = dhallResult.out;
  home.packages = [pkgs.kassandra-standalone];
}
