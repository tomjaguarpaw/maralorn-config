{
  config,
  pkgs,
  lib,
  ...
}:
{
  environment.persistence.snapshoted.files = [
    "/var/lib/private/gitea-runner/fluffy/.labels"
    "/var/lib/private/gitea-runner/fluffy/.runner"
    "/var/lib/private/gitea-runner/forgejo-runner-key"
  ];
  programs.ssh.extraConfig = lib.mkBefore ''
    Match localuser gitea-runner
      IdentityFile /var/lib/gitea-runner/forgejo-runner-key
    Match localuser gitea-runner host fluffy
      User ci-upload-user
  '';
  nix.settings.trusted-users = [ "gitea-runner" ];
  services.gitea-actions-runner.instances.${config.networking.hostName} = {
    enable = true;
    name = config.networking.hostName;
    url = "https://code.maralorn.de";
    tokenFile = config.age.secrets.forgejo-runner-token.path;
    labels = [ "nix:host" ];
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
        nix
        nodejs
        openssh
        wget
      ;
    };
  };
}
