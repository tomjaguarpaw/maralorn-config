{ config, ... }: {
  services = {
    nix-serve = {
      enable = true;
      secretKeyFile = "/var/cache-priv-key.pem";
    };
    nginx = {
      enable = true;
      virtualHosts."binarycache" = {
        listen = [{
          addr = "[::]";
          port = 5001;
        }];
        locations."/".proxyPass =
          "http://127.0.0.1:${toString config.services.nix-serve.port}";
      };
    };
  };
}
