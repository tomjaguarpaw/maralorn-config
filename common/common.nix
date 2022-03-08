{pkgs}: {
  syncthing = {
    declarativeWith = hosts: path: let
      mkFolder = name: {
        path = "${path}/${name}";
        devices = hosts;
      };
      devices = {
        apollo.id = "BOTTTGS-QQUHWAK-IFBT3T2-HGHHUZ7-QHRZXC7-JC42VT7-67ZOJBE-WHDWEQX";
        zeus.id = "5BUZIS5-ESTYAJO-IQQD7EA-O3VGONJ-E74OHUJ-ZSLF4JK-6HS3UHG-4CQ5OAO";
        hera = {
          addresses = ["tcp6://hera.m-0.eu"];
          id = "TJHVUM6-RTB6V3D-JF4GIB2-TVDF2ST-5MTN6N2-ZDIWGF7-XZUCCFG-EQG5WA6";
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
        tmp = mkFolder "tmp";
      };
    };
  };
}
