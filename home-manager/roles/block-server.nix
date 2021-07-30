list:
{ pkgs, lib, ... }: {
  systemd.user.services.blockserver = {
    Unit.Description = "Serve a blocklist";
    Service =
      let
        blocklist = pkgs.writeTextDir "blocklist" (lib.concatStringsSep "\r\n" list);
        startpage = pkgs.writeTextDir "index.html" (builtins.readFile ./startpage.html);
      in
      {
        ExecStart = "${pkgs.python3}/bin/python -m http.server 8842 -d ${pkgs.symlinkJoin { name = "blockserver-dir"; paths = [ blocklist startpage ]; }}";
        Restart = "always";
      };
    Install.WantedBy = [ "default.target" ];
  };
}
