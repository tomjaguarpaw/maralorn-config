{
  config,
  ...
}:
{
  virtualisation.podman.enable = true;
  services.gitea-actions-runner.instances.${config.networking.hostName} = {
    enable = true;
    name = config.networking.hostName;
    url = "https://code.maralorn.de";
    tokenFile = config.age.secrets.forgejo-runner-token.path;
    labels = [ "ubuntu-latest:docker:catthehacker/ubuntu:act-latest" ];
  };
}
