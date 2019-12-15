{ pkgs ? import <nixpkgs> }: {
  syncthing = {
    declarativeWith = hosts: path:
      let
        mkFolder = name: {
          path = "${path}/${name}";
          devices = hosts;
        };
        rootFolder = path;
        devices = {
          apollo = {
            id =
              "BOTTTGS-QQUHWAK-IFBT3T2-HGHHUZ7-QHRZXC7-JC42VT7-67ZOJBE-WHDWEQX";
          };
          hera = {
            addresses = [ "tcp6://hera.m-0.eu" ];
            id =
              "TJHVUM6-RTB6V3D-JF4GIB2-TVDF2ST-5MTN6N2-ZDIWGF7-XZUCCFG-EQG5WA6";
          };
        };
      in {
        devices = pkgs.lib.getAttrs hosts devices;
        folders = {
          science = mkFolder "science";
          documents = mkFolder "documents";
          audio = mkFolder "audio";
          video = mkFolder "video";
          images = mkFolder "images";
          books = mkFolder "books";
        };
      };
  };
}
