{ pkgs, ... }:
let
  watchNixpkgsPackage = name: branch: path:
    let job_name = "nixpkgs ${name} on ${branch}";
    in {
      inherit job_name;
      metrics_path = "/job/${path}/prometheus";
      scheme = "https";
      scrape_interval = "1h";
      scrape_timeout = "60s";
      static_configs = [{
        labels = {
          name = job_name;
          packageName = name;
          url = "https://hydra.nixos.org/job/${path}";
          alert_type = "nixpkgs";
        };
        targets = [ "hydra.nixos.org" ];
      }];
    };
  watchHaskellUnstable = name:
    watchNixpkgsPackage name "haskell-updates"
    "nixpkgs/haskell-updates/haskellPackages.${name}.x86_64-linux";
  watchHaskellStable = name:
    watchNixpkgsPackage name "release-20.03"
    "nixos/release-20.03/nixpkgs.haskellPackages.${name}.x86_64-linux";
  watchedHaskellUpdatesPkgs = builtins.attrNames (pkgs.myHaskellPackages) ++ [
    "reflex-dom"
    "jsaddle-warp"
    "snap"
    "dependent-sum-template"
    "universe-dependent-sum"
    "hnix"
    "ref-tf"
    "modern-uri"
    "network-uri"
    "github"
    "cookie"
    "shelly"
    "stan"
  ];
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
