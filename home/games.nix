{ pkgs, ... }:
let inherit (import ../lib) unfreePkgs writeHaskellScript;
in {
  home.packages = builtins.attrValues {
    inherit (unfreePkgs) steam;
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
            libtxc_dxtn
            gnutls
            krb5Full
            ncurses5
            vulkan-headers
            vulkan-loader
            vulkan-tools
          ]);
      runScript = let
        gw2wrapper = writeHaskellScript {
          name = "gw2wrapper";
          bins = [ pkgs.procps ];
          imports = [ "System.Directory (withCurrentDirectory)" ];

        } ''
          waitForExit = do
            sleep "5s"
            processes <- ps "aux" |> captureTrim
            when
              (BS.isInfixOf (encodeUtf8 "GW2.exe") (toStrict processes))
              waitForExit
          main = do
            withCurrentDirectory "/home/maralorn/GW2" $ exe "./play.sh"
            waitForExit
        '';
      in "${gw2wrapper}/bin/gw2wrapper";
    };
  };
}
