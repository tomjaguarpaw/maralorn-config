{
  config,
  pkgs,
  ...
}:
{
  services.gitea-actions-runner.instances.${config.networking.hostName} = {
    enable = true;
    name = config.networking.hostName;
    url = "https://code.maralorn.de";
    tokenFile = config.age.secrets.forgejo-runner-token.path;
    labels = [ "nix:host" ];
    hostPackages = builtins.attrValues {
      inherit (pkgs)
        bash
        coreutils
        curl
        gawk
        gitMinimal
        gnused
        nodejs
        wget
        nix
        archive-nix-path
        builders-configurator
      ;
    };
  };
}
