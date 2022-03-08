{
  pkgs,
  config,
  ...
}: let
  simpleDesktopItem = name: command:
    pkgs.makeDesktopItem {
      inherit name;
      desktopName = name;
      exec = command;
    };
  namedTerminalDesktopItem = name: command:
    pkgs.makeDesktopItem {
      inherit name;
      desktopName = name;
      exec = command;
      terminal = "true";
    };
  superSimpleDesktopItem = name: simpleDesktopItem name name;
  terminalDesktopItem = name: namedTerminalDesktopItem name name;
in {
  home.packages = map superSimpleDesktopItem ["kassandra2"] ++ map terminalDesktopItem ["unlock-ssh"];
}
