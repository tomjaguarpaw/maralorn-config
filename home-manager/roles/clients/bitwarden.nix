{
  pkgs,
  lib,
  config,
  ...
}:
let
  rbw-fzf-src = builtins.fetchurl {
    url = "https://github.com/doy/rbw/raw/9958e9ab02b1db5308f4bb131d2c8687d13cf4fd/bin/rbw-fzf";
    sha256 = "sha256:0yrgdpjfwcwv93yb1vr5rzplp4ak9avaahqn8fic7cxxvccqjcm0";
  };
  rbw-fzf = pkgs.runCommand "rbw-fzf" { } ''
    mkdir -p $out/bin
    install ${rbw-fzf-src} $out/bin/rbw-totp-fzf
    install ${rbw-fzf-src} $out/bin/rbw-fzf
    ${lib.getExe pkgs.sd} "rbw get" "rbw code" $out/bin/rbw-totp-fzf
    ${pkgs.resholve.phraseSolution "rbw-fzf-resholve" {
      scripts = [
        "bin/rbw-totp-fzf"
        "bin/rbw-fzf"
      ];
      interpreter = lib.getExe pkgs.bash;
      execer = [
        "cannot:${lib.getExe config.programs.rbw.package}"
        "cannot:${lib.getExe pkgs.fzf}"
      ];
      inputs = [
        config.programs.rbw.package
        pkgs.perl
        pkgs.fzf
        pkgs.coreutils
        pkgs.findutils
      ];
    }}
  '';
in
{
  home.packages = [ rbw-fzf ];
  programs.rbw = {
    enable = true;
    settings = {
      email = "bitwarden@maralorn.de";
      base_url = "https://bitwarden.darmstadt.ccc.de";
      lock_timeout = 86400; # One day
    };
  };
}
