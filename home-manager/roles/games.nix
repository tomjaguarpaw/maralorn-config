{ pkgs, lib, config, ... }:
let
  gw2dir = "${config.home.homeDirectory}/volatile/GW2";
  wine = pkgs.wineWowPackages.staging;
  gw2env = ''
    cd ${gw2dir}
    export MESA_GLSL_CACHE_DISABLE=0
    export MESA_GLSL_CACHE_DIR="${gw2dir}/shader_cache"
    export mesa_glthread=true

    # Wine Settings
    export DXVK_HUD=fps,frametimes
    export DXVK_LOG_LEVEL=none
    #export DXVK_STATE_CACHE=1 default
    export DXVK_STATE_CACHE_PATH="${gw2dir}/dxvk_state_cache/"
    export WINEDEBUG=-all
    export WINEARCH=win64
    export WINEPREFIX="${gw2dir}/data"
    export STAGING_SHARED_MEMORY=1
    export WINEESYNC=1
  '';
  dxvk = fetchTarball {
    url = "https://github.com/doitsujin/dxvk/releases/download/v1.7.2/dxvk-1.7.2.tar.gz";
    sha256 = "07q9fsrvjq2ndnhd93000jw89bkaw6hdi2yhl4d6j8n4ak71r8pv";
  };
  gw2installdxvk = pkgs.writeShellScriptBin "gw2-install-dxvk" ''
    ${gw2env}
    cd ${dxvk}
    bash ./setup_dxvk.sh install
  '';
  gw2setup = pkgs.writeShellScriptBin "gw2-setup" ''
    mkdir -p ${gw2dir}
    ${gw2env}
    echo Launching winecfg to configure desktop window
    ${wine}/bin/winecfg
    echo Installing dxvk
    ${gw2installdxvk}/bin/gw2-install-dxvk
    echo Downloading installer
    wget https://account.arena.net/content/download/gw2/win/64 -O Gw2Setup-64.exe
    echo Running installer
    ${wine}/bin/wine64 ./Gw2Setup-64.exe
  '';
  gw2run = pkgs.writeShellScriptBin "gw2" ''
    ${gw2env}
    cd "${gw2dir}/data/drive_c/Guild Wars 2"
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
    #factorio = pkgs.factorio.override {
    #  username = "maralorn";
    #  token = pkgs.privateValue "" "factorio";
    #};
    inherit (pkgs) steam minecraft;
    inherit gw2run gw2setup wine gw2installdxvk;
  };
}
