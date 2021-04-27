{ pkgs, ... }: let
  dhallFiles = pkgs.runCommand "kassandra-config-src" {} ''
    mkdir $out
    ${pkgs.kassandra2}/bin/kassandra2 print-types > $out/types.dhall
    ln -s ${./kassandra}/{config,backend}.dhall $out
    ln -s ${../../private/kassandra-uiConfig.dhall} $out/uiConfig.dhall
  '';
  backend = pkgs.dhallPackages.buildDhallPackage {
    name = "kassandra-backend-config";
    code = "${dhallFiles}/backend.dhall : (${dhallFiles}/types.dhall).BackendConfig";
    source = true;
    dependencies = [ pkgs.dhallPackages.Prelude ];
  };
  standalone = pkgs.dhallPackages.buildDhallPackage {
    name = "kassandra-standalone-config";
    code = "${dhallFiles}/config.dhall : (${dhallFiles}/types.dhall).StandaloneConfig";
    source = true;
    dependencies = [ pkgs.dhallPackages.Prelude ];
  };

  dhallResult = pkgs.runCommand "kassandra-config" {} ''
    mkdir $out
    ln -s ${backend}/source.dhall $out/backend.dhall
    ln -s ${standalone}/source.dhall $out/config.dhall
  '';
in
{
  home.file = {
    "kassandra-config" = {
      target = ".config/kassandra";
      source = dhallResult.out;
    };
  };
}
