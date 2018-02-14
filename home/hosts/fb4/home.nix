{ pkgs, ... }:
{
  imports = [
    ../../snippets/everywhere.nix
    ../../snippets/graphical.nix
    # ./syncthing.nix
    # ./nix-gc.nix
    # ./nix-update-channel.nix
  ];

  home.packages = with pkgs; [
    xautolock
    syncthing
  ];

  systemd.user.systemctlPath = "/usr/bin/systemctl";
}
