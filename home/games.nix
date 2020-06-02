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
    inherit (pkgs.unfree) steam;
    inherit (pkgs) minetest;
    gw2 = pkgs.buildFHSUserEnv {
      name = "gw2";
      targetPkgs = pkgs: (with pkgs; [ sambaFull ]);
      multiPkgs = pkgs:
        (with pkgs;
          with xorg; [
            file
            freetype
            libpng
            mesa_drivers
            zlib
            libXi
            libXcursor
            libXrandr
            libXrender
            libXxf86vm
            libXcomposite
            libXext
            libX11
            libudev
            libGLU
            mesa_noglu.osmesa
            libdrm
            libpulseaudio
            alsaLib
            openal
            mpg123
            gnutls
            krb5Full
            ncurses5
            vulkan-headers
            vulkan-loader
            vulkan-tools
          ]);
      runScript = "/home/maralorn/GW2/play.sh";
    };
  };
}
