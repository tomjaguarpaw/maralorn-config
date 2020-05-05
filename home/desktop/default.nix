{ pkgs, lib, config, ... }:
let
  inherit (import ../../pkgs) desktop-pkgs;
  inherit (import ../../lib) colors;
in {
  imports = [
    ./sway.nix
    ./wallpaper.nix
    ./rofi.nix
    ./ssh-agent.nix
    ./sleep-nag.nix
    ./kitty.nix
  ];
  m-0 = {
    workspaces = [
      "tasks"
      "chat"
      "mail"
      "code"
      "research"
      "work"
      "ccc"
      "orga"
      "leisure"
      "config"
    ];
    terminal = "${desktop-pkgs.terminal}/bin/terminal";
    colors = colors;
  };
  home = { packages = builtins.attrValues desktop-pkgs; };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Arc";
      package = pkgs.arc-icon-theme;
    };
    theme = {
      name = "Arc";
      package = pkgs.arc-theme;
    };
  };
  services = {
    udiskie = {
      enable = true;
      notify = true;
    };
  };
}
