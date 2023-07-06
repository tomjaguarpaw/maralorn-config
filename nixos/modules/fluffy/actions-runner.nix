{
  config,
  pkgs,
  ...
}:
{
  services.gitea-actions-runner.instances.fluffy = {
    enable = true;
    name = "fluffy";
    url = "https://code.maralorn.de";
    tokenFile = config.age.secrets.forgejo-runner-token.path;
    labels = [ "nix:host" ];
    hostPackages = builtins.attrValues {
      inherit (pkgs)
        bash
        coreutils
        curl
        gitMinimal
        wget
        nix
      ;
    };
  };
}
