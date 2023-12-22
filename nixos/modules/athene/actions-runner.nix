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
  services.gitea-actions-runner.instances.${config.networking.hostName} = {
    enable = true;
    name = config.networking.hostName;
    url = "https://code.maralorn.de";
    # I am fine with leaking this secret, because it will be invalidated after
    # first use.
    token = "Kn0b5KM3YEzpXO2FDBW1N8op6w4Q0M8bkJpth2e2";
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
