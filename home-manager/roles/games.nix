{ pkgs, lib, config, ... }: let
  gw2dir = "${config.home.homeDirectory}/volatile/GW2";
  wine = pkgs.wineWowPackages.staging;
  gw2env = ''
     export MESA_GLSL_CACHE_DISABLE=0
     export MESA_GLSL_CACHE_DIR="$PWD/shader_cache"
     export mesa_glthread=true

     # Wine Settings
     export DXVK_HUD=version,devinfo,fps
     export DXVK_LOG_LEVEL=none
     export WINEDEBUG=-all
     export WINEARCH=win64
     export WINEPREFIX="$PWD/data"
     export STAGING_SHARED_MEMORY=1
     export WINEESYNC=1
  '';
  gw2setup = pkgs.writeShellScriptBin "gw2-setup" ''
     mkdir -p ${gw2dir}
     cd ${gw2dir}
     ${gw2env}
     echo Launching winecfg to configure desktop window
     ${wine}/bin/winecfg
     echo Downloading installer
     wget https://account.arena.net/content/download/gw2/win/64 -O Gw2Setup-64.exe
     echo Running installer
     ${wine}/bin/wine64 ./Gw2Setup-64.exe
  '';
  gw2run = pkgs.writeShellScriptBin "gw2" ''
     cd ${gw2dir}
     ${gw2env}
     cd "$PWD/data/drive_c/Guild Wars 2"
     ${wine}/bin/wine64 ./Gw2-64.exe $@ -autologin
     '';
in
  {

  dconf.settings."org/gnome/settings-daemon/plugins/media-keys" = {
    mic-mute = lib.mkForce [ ];
    next = lib.mkForce [ ];
    play = lib.mkForce [ ];
    previous = lib.mkForce [ ];
    screensaver = lib.mkForce [ ];
    volume-down = lib.mkForce [ ];
    volume-up = lib.mkForce [ ];
  };

  home.packages = builtins.attrValues {
    factorio = pkgs.factorio.override {
      username = "maralorn";
      token = import ../../nixos/machines/apollo/secret/factory.nix;
      experimental = true;
    };
    inherit (pkgs) steam minetest;
    inherit gw2run gw2setup wine;
  };
}
