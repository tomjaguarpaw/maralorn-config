{ pkgs, config, ... }:
{

  services = {
    nix-serve = {
      package = pkgs.nix-serve-ng;
      port = 4983;
      enable = true;
      bindAddress = "localhost";
      secretKeyFile = config.age.secrets.nix-serve-secret-key.path;
    };
    nginx.virtualHosts.${config.m-0.virtualHosts.cache}.locations."/".proxyPass = "http://[::1]:${toString config.services.nix-serve.port}";
  };
}
