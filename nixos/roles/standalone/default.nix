{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (pkgs.flake-inputs.self) sourceInfo;
  s = builtins.substring;
  formatDate = date: "${s 0 4 date}-${s 4 2 date}-${s 6 2 date}-${s 8 2 date}:${s 10 2 date}";
in {
  imports = [
    ../vpn.nix
  ];
  services.getty.greetingLine = "Welcome to NixOS ${config.system.nixos.version} (\m) - \l";
  system = {
    nixos.label =
      if sourceInfo ? shortRev
      then "${config.system.nixos.version}-${formatDate sourceInfo.lastModifiedDate}-${sourceInfo.shortRev}"
      else "${config.system.nixos.version}-dirty";
    systemBuilderCommands = lib.mkIf (sourceInfo ? rev) ''
      echo ${sourceInfo.rev} > $out/config-commit
    '';
  };

  boot = {
    plymouth.enable = true;
    loader = {
      timeout = 1;
      grub = {
        backgroundColor = "#000000";
        # So that boot does not fill up with old kernels
        configurationLimit = 5;
      };
    };
    kernel.sysctl."fs.inotify.max_user_watches" = 204800;
  };

  security.sudo.extraConfig = "\n    Defaults timestamp_type=global, timestamp_timeout=15\n  ";

  services.openssh = {
    enable = true;
    # permitRootLogin = "prohibit-password"; Default
  };

  nix = {
    nixPath = ["nixpkgs=flake:pkgs"];

    registry.pkgs = {
      from = {
        type = "indirect";
        id = "pkgs";
      };
      flake = pkgs.flake-inputs.nixos-unstable;
    };
    settings.trusted-users = ["maralorn" "laminar"];
    optimise = {
      dates = [];
      automatic = true;
    };
  };

  environment = {
    systemPackages = [pkgs.updateSystem];
    etc."nix/machines".source = toString (pkgs.runCommand "nix-machines" {} ''
      cp $(${pkgs.builders-configurator}/bin/builders-configurator ${config.networking.hostName} --without-connection) $out
    '');
  };

  nix = {
    distributedBuilds = lib.mkDefault true;
    gc = {
      automatic = false;
      options = "-d";
    };
  };

  programs = {
    mtr.enable = true;
  };
}
