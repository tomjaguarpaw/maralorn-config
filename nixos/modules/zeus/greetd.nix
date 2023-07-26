{ pkgs, ... }:
{
  # autologin
  services.greetd.settings.default_session = {
    user = "maralorn";
    command = pkgs.writeShellScript "launch-hyprland" ''
      rm -rf /tmp/hypr
      exec Hyprland
    '';
  };
}
