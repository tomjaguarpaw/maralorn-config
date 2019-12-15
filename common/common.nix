{ pkgs ? import <nixpkgs> }: {
  syncthing = {
    declarativeWith = hosts:
      let
        folderContent = { devices = hosts; };
        rootFolder = "/media";
        devices = {
          apollo = { id = "?"; };
          hera = {
            id =
              "TJHVUM6-RTB6V3D-JF4GIB2-TVDF2ST-5MTN6N2-ZDIWGF7-XZUCCFG-EQG5WA6";
          };
        };
      in {
        devices = pkgs.lib.getAttrs hosts devices;
        folders = {
          "${rootFolder}/science" = folderContent;
          "${rootFolder}/documents" = folderContent;
          "${rootFolder}/audio" = folderContent;
          "${rootFolder}/video" = folderContent;
          "${rootFolder}/images" = folderContent;
        };
      };
  };
}
