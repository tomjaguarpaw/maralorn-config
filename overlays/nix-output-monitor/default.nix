final: prev:
let master = import prev.sources.nixpkgs-master { };
in {
  nix-output-monitor = import prev.sources.nix-output-monitor { pkgs = final; };
}
