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
  watchNixpkgsHaskellPackage = name: [
    (watchNixpkgsPackage
      "nixpkgs/haskell-updates/haskellPackages.${name}.x86_64-linux")
    (watchNixpkgsPackage
      "nixos/release-20.03/nixpkgs.haskellPackages.${name}.x86_64-linux")
  ];
in {
  services.prometheus.scrapeConfigs = [
    (watchNixpkgsPackage
      "nixpkgs/haskell-updates/haskellPackages.cabal-fmt.x86_64-linux")
  ] ++ (watchNixpkgsHaskellPackage "ghcide")
    ++ (watchNixpkgsHaskellPackage "brittany")
    ++ (watchNixpkgsHaskellPackage "releaser")
    ++ (watchNixpkgsHaskellPackage "hlint")
    ++ (watchNixpkgsHaskellPackage "relude")
    ++ (watchNixpkgsHaskellPackage "taskwarrior")
    ++ (watchNixpkgsHaskellPackage "pandoc");
}
