{ pkgs, config, ... }:
let
  simpleDesktopItem = name: command:
    pkgs.makeDesktopItem {
      name = name;
      desktopName = name;
      exec = command;
    };
  namedTerminalDesktopItem = name: command:
    pkgs.makeDesktopItem {
      name = name;
      desktopName = name;
      exec = command;
      terminal = "true";
    };
  superSimpleDesktopItem = name: simpleDesktopItem name name;
  terminalDesktopItem = name: namedTerminalDesktopItem name name;

in
{
  home.packages = map superSimpleDesktopItem [ "kassandra" ] ++ map terminalDesktopItem [ "unlock-ssh" ];
}
