{ pkgs }:
{
  syncthing = {
    declarativeWith =
      hosts: path:
      let
        mkFolder = name: {
          path = "${path}/${name}";
          devices = hosts;
        };
        devices = pkgs.lib.mapAttrs (name: conf: conf // { addresses = [ "tcp6://${name}.vpn.m-0.eu" ]; }) {
          apollo.id = "BOTTTGS-QQUHWAK-IFBT3T2-HGHHUZ7-QHRZXC7-JC42VT7-67ZOJBE-WHDWEQX";
          zeus.id = "5BUZIS5-ESTYAJO-IQQD7EA-O3VGONJ-E74OHUJ-ZSLF4JK-6HS3UHG-4CQ5OAO";
          pegasus.id = "BISYPNZ-54VKBKS-LBND4AS-JNWVOW7-BTW2UMV-QHYM5TZ-GE3AK3E-PGSXPQE";
          hera.id = "TJHVUM6-RTB6V3D-JF4GIB2-TVDF2ST-5MTN6N2-ZDIWGF7-XZUCCFG-EQG5WA6";
          hephaistos.id = "M6QOG65-2VIL4IF-IP2IGHW-QAT7VSC-3V3244M-UQ7Y5A6-T2TJEAL-BP427QR";
          athene.id = "7EMUMTN-NQATI52-B67EUJP-DVB6OLE-KQSM5LJ-2HI3KX5-UJAKUTL-PAV6HQ3";
        };
      in
      {
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
