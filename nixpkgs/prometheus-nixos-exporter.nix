final: _: {
  prometheus-nixos-exporter = final.callPackage (
    final.flake-inputs.nixos-infra + /modules/prometheus/nixos-exporter/default.nix
  ) { };
}
