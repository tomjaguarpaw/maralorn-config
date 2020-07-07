{ pkgs, lib, ... }: {

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
    factorio = pkgs.unstableUnfree.factorio.override {
      username = "maralorn";
      token = import ../../nixos/machines/apollo/secret/factory.nix;
      experimental = true;
    };
    inherit (pkgs.unfree) steam;
    inherit (pkgs) minetest;
    inherit (pkgs.wineWowPackages) staging;
    gw2 = pkgs.writeShellScriptBin "gw2" ''
      cd /home/maralorn/volatile/GW2

      # Intel/AMD Mesa Specific Env_vars
      # export vblank_mode=0
      export MESA_GLSL_CACHE_DISABLE=0
      export MESA_GLSL_CACHE_DIR="$PWD/shader_cache"
      export mesa_glthread=true

      # Wine Settings
      #export DXVK_HUD=version,devinfo,fps
      export DXVK_LOG_LEVEL=none
      export WINEDEBUG=-all
      export WINEARCH=win64
      export WINEPREFIX="$PWD/data"
      export STAGING_SHARED_MEMORY=1
      export WINEESYNC=1

      # Launch Command
      cd "$PWD/data/drive_c/GW2"
      wine64 ./GW2.exe $@ -autologin
    '';
  };
}
