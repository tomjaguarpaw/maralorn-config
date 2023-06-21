{ pkgs, config, ... }: {

  services.nix-serve = {
    package = pkgs.haskell.lib.overrideCabal pkgs.nix-serve-ng {
      src = pkgs.fetchFromGitHub {
        repo = "nix-serve-ng";
        owner = "aristanetworks";
        rev = "dabf46d65d8e3be80fa2eacd229eb3e621add4bd";
        hash = "sha256-SoJJ3rMtDMfUzBSzuGMY538HDIj/s8bPf8CjIkpqY2w=";
      };
    };
    enable = true;
    bindAddress = "localhost";
    secretKeyFile = config.age.secrets.nix-serve-secret-key.path;
  };

  services.nginx.virtualHosts.${
    config.m-0.virtualHosts."cache"
  }.locations."/".proxyPass =
    "http://[::1]:${toString config.services.nix-serve.port}";
}
