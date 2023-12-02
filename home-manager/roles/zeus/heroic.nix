{pkgs, ...}: {home.packages = builtins.attrValues {inherit (pkgs) heroic gogdl;};}
