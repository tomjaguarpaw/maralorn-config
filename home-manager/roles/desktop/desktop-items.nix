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
  home.packages = map superSimpleDesktopItem [ "kassandra2" "gw2" "chat" ]
    ++ map terminalDesktopItem [
    "maintenance"
    "ncmpcpp"
    "kassandra"
    "hotkeys"
    "vim"
    "select-mode"
    "unlock-ssh"
  ];
}
