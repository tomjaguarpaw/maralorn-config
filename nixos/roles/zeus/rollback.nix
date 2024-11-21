_: { }
#{ pkgs, ... }:
#let
#  script = pkgs.writers.writeHaskell "rollback" {
#    libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
#  } (builtins.readFile ./sysroot-rollback.hs);
#in
#{
#
#  boot.initrd.systemd = {
#    storePaths = [ script ];
#    services.rollback = {
#      description = "Rollback File System Root";
#      wantedBy = [ "basic.target" ];
#      before = [
#        "initrd-root-fs.target"
#        "sysroot-var-lib-nixos.mount"
#      ];
#      after = [ "sysroot.mount" ];
#      unitConfig.DefaultDependencies = "no";
#      serviceConfig = {
#        Type = "oneshot";
#        ExecStart = script;
#      };
#    };
#  };
#}
#
