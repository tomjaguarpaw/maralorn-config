{ ... }:
let
  watchNixpkgsPackage = name: path: {
    job_name = path;
    metrics_path = "/job/${path}/prometheus";
    scheme = "https";
    scrape_interval = "1h";
    scrape_timeout = "60s";
    static_configs = [{
      labels = {
        packageName = name;
        url = "https://hydra.nixos.org/job/${path}";
      };
      targets = [ "hydra.nixos.org" ];
    }];
  };
  watchHaskellUnstable = name:
    watchNixpkgsPackage name
    "nixpkgs/haskell-updates/haskellPackages.${name}.x86_64-linux";
  watchHaskellStable = name:
    watchNixpkgsPackage name
    "nixos/release-20.03/nixpkgs.haskellPackages.${name}.x86_64-linux";
  watchedUnstablePkgs = [ "cabal-fmt" "neuron" ];
  watchedPkgs = [
    "ghcide"
    "brittany"
    "releaser"
    "hlint"
    "relude"
    "taskwarrior"
    "pandoc"
    "shh"
  ];
in {
  services.prometheus.scrapeConfigs =
    map watchHaskellUnstable (watchedUnstablePkgs ++ watchedPkgs)
    ++ map watchHaskellStable watchedPkgs;
}
