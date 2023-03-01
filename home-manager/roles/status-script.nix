{pkgs, ...}: let
  status-script =
    pkgs.writeHaskell "status-script"
    {
      libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
      ghcEnv = {
        PATH = "${pkgs.lib.makeBinPath [pkgs.git pkgs.notmuch pkgs.playerctl pkgs.khal pkgs.nix pkgs.coreutils]}:$PATH";
      };
      ghcArgs = ["-threaded"];
    }
    (builtins.readFile ./status-script.hs);
in {
  systemd.user.services.status-script = {
    Unit.Description = "Status Script";
    Service.ExecStart = toString status-script;
    Install.WantedBy = ["graphical-session.target"];
  };
}
