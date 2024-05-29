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
  jsonFormat = pkgs.formats.json { };
in
{
  home.packages = builtins.attrValues rec {
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
    ssh-rbw-pubkey = pkgs.writeShellScriptBin "ssh-rbw-pubkey" ''
      file=$(mktemp)
      ${lib.getExe ssh-rbw-privkey} "$1" > $file
      ssh-keygen -y -f $file
      rm $file
    '';
    ssh-rbw-privkey = pkgs.writeShellScriptBin "ssh-rbw-privkey" ''
      ${lib.getExe config.programs.rbw.package} get --folder ssh "$1"
    '';
    ssh-rbw-gen = pkgs.writeShellScriptBin "ssh-rbw-gen" ''
      ssh-keygen -ted25519 -C "maralorn.$1@pantheon"
    '';
    ssh-rbw-add = pkgs.writeShellScriptBin "ssh-rbw-add" ''
      ssh-add <(${lib.getExe ssh-rbw-privkey} "$1")
    '';
    unlock-keys = pkgs.writeShellScriptBin "unlock-keys" ''
      set -x
      ${lib.getBin pkgs.dbus}/bin/dbus-update-activation-environment --systemd SSH_AUTH_SOCK
      echo "Ensuring unlock …"
      if ! rbw unlocked; then
        killall rbw-agent;
        rbw unlock
      fi
      echo "Waiting until unlocked …"
      until rbw unlocked; do sleep 1s; echo "Still waiting …"; done

      echo "Loading keys …"
      ${lib.getExe ssh-rbw-add} code
      ${lib.getExe ssh-rbw-add} signing
      ${lib.getExe ssh-rbw-add} login
    '';
  };
  programs.rbw.enable = true;
  xdg.configFile."rbw/config.json".source = jsonFormat.generate "rbw-config.json" {
    email = "bitwarden@maralorn.de";
    base_url = "https://bitwarden.darmstadt.ccc.de";
    lock_timeout = 86400; # One day
    pinentry = lib.getExe pkgs.pinentry;
  };
}
