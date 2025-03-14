{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.persistence.snapshoted.files = [
    "/var/lib/private/gitea-runner/athene/.labels"
    "/var/lib/private/gitea-runner/athene/.runner"
    "/var/lib/private/gitea-runner/forgejo-runner-key"
  ];
  programs.ssh.extraConfig = lib.mkBefore ''
    Match localuser gitea-runner
      IdentityFile /var/lib/gitea-runner/forgejo-runner-key
    Match localuser gitea-runner host athene
      User ci-upload-user
  '';
  nix.settings.trusted-users = [ "gitea-runner" ];
  services.gitea-actions-runner = {
    package = pkgs.forgejo-actions-runner;
    instances.${config.networking.hostName} = {
      enable = true;
      name = config.networking.hostName;
      url = "https://code.maralorn.de";
      token = "";
      labels = [ "nix:host" ];

      # Fix for: https://gitea.com/gitea/act_runner/issues/361
      settings = {
        runner.capacity = 4;
        host.workdir_parent = "/var/lib/gitea-runner/action-cache-dir";
      };
      hostPackages = builtins.attrValues {
        inherit (pkgs)
          archive-nix-path
          bash
          builders-configurator
          coreutils
          curl
          gawk
          gitMinimal
          gnused
          jq
          nix
          nix-update
          nodejs
          tea
          openssh
          wget
          ;
      };
    };
  };
  # Mainly to override faulty restart from upstream
  systemd.services.gitea-runner-athene = {
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = lib.mkForce 30;
    };
    unitConfig.StartLimitIntervalSec = 300;
  };
}
