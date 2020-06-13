{ pkgs, ... }:
let
  watchNixpkgsPackage = name: path: {
    job_name = path;
    metrics_path = "/job/${path}/prometheus";
    scheme = "https";
    scrape_interval = "1h";
    scrape_timeout = "60s";
    static_configs = [{
      labels = {
        inherit name;
        packageName = name;
        url = "https://hydra.nixos.org/job/${path}";
        alert_type = "nixpkgs";
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
  watchedHaskellUpdatesPkgs = builtins.attrNames (pkgs.myHaskellPackages)
    ++ [ "reflex-dom" ];
  watchedStablePkgs = [
    "ghcide"
    "brittany"
    "releaser"
    "hlint"
    "relude"
    "taskwarrior"
    "pandoc"
    "shh"
    "clay"
  ];
in {
  services.prometheus.scrapeConfigs =
    map watchHaskellUnstable watchedHaskellUpdatesPkgs
    ++ map watchHaskellStable watchedStablePkgs;
}
