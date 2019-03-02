{ config, pkgs, lib, ... }:

with lib;

{

imports = [ ./secret ];

config = {
  nixpkgs.overlays = [ (self: super: {
    unstable = import (builtins.fetchGit { url = "https://github.com/NixOS/nixpkgs-channels"; ref = "nixos-unstable";}) {};
  })];
};


options = {
  m-0.private = mkOption {
    default = {};
    type = types.attrs;
  };
  m-0.prefix = mkOption {
    default = "2a02:c207:3002:7584";
    type = types.str;
  };
  m-0.hosts = mkOption {
    type = types.attrs;
    default = let
      p = config.m-0.prefix;
      hera-p = "${p}::3";
      apollo-p = "${p}::1";
      wg-p = "${p}::100";
      v4-p = "10.0.0";
    in rec {
      hera = "${p}::1";

      hera-v4 = "213.136.94.190";

      hera-wg = "${wg-p}:1";
      apollo-wg = "${wg-p}:2";

      hera-intern = "${hera-p}:1";
      git = "${hera-p}:2";
      borg = "${hera-p}:3";
      dav = "${hera-p}:5";
      blog = "${hera-p}:6";
      chor = "${hera-p}:7";
      matrix = "${hera-p}:8";
      cloud = "${hera-p}:9";
      web = "${hera-p}:a";
      mathechor-cloud = "${hera-p}:b";

      apollo = apollo-wg;

      hera-intern-v4 = "${v4-p}.1";
      cloud-intern-v4 = "${v4-p}.2";
      mathechor-cloud-intern-v4 = "${v4-p}.3";
      matrix-intern-v4 = "${v4-p}.4";
    };
  };
};

}
