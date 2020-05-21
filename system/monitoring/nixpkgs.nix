{ ... }:
let
  watchNixpkgsPackage = name: {
    job_name = "nixpkgs-${name}";
    metrics_path = "/job/${name}/prometheus";
    scheme = "https";
    scrape_interval = "1h";
    scrape_timeout = "60s";
    static_configs = [{ targets = [ "hydra.nixos.org" ]; }];
  };
  watchHaskellUnstable = name:
    watchNixpkgsPackage
    "nixpkgs/haskell-updates/haskellPackages.${name}.x86_64-linux";
  watchHaskellStable = name:
    watchNixpkgsPackage
    "nixos/release-20.03/nixpkgs.haskellPackages.${name}.x86_64-linux";
  watchedUnstablePkgs = [ "cabal-fmt" "neuron" ];
  watchedPkgs =
    [ "ghcide" "brittany" "releaser" "hlint" "relude" "taskwarrior" "pandoc" "shh" ];
in {
  services.prometheus.scrapeConfigs =
    map watchHaskellUnstable (watchedUnstablePkgs ++ watchedPkgs)
    ++ map watchHaskellStable watchedPkgs;
}
