{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [../vpn.nix];

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

  services.sshd.enable = true;

  environment.etc."nix/machines".source = toString (pkgs.runCommand "nix-machines" {} ''
    cp $(${pkgs.builders-configurator}/bin/builders-configurator ${config.networking.hostName} --without-connection) $out
  '');

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
