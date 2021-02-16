{ pkgs, ... }:
let
  watchNixpkgsPackage = name: branch: path:
    let job_name = "nixpkgs ${name} on ${branch}";
    in {
      inherit job_name;
      metrics_path = "/job/${path}/prometheus";
      scheme = "https";
      scrape_interval = "1h";
      scrape_timeout = "120s";
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
  watchedHaskellUpdatesPkgs = builtins.attrNames (pkgs.myHaskellPackages) ++ [
    "jsaddle-warp"
    "stan"
    "dependent-sum-template"
    "universe-dependent-sum"
    "hnix"
    "ref-tf"
    "modern-uri"
    "network-uri"
    "github"
    "cookie"
    "shelly"
    "nix-output-monitor"
    "servant"
    "cachix"
    "taffybar"
  ];
in {
  services.prometheus.scrapeConfigs =
    map watchHaskellUnstable watchedHaskellUpdatesPkgs ++ [
      (watchNixpkgsPackage "haskell-language-server-toplevel" "haskell-updates"
        "nixpkgs/haskell-updates/haskell-language-server.x86_64-linux")
    ];
}
