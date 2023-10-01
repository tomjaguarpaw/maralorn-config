flake-inputs: {
  imports =
    [
      (import ../../roles/home-manager.nix flake-inputs)
      flake-inputs.nixos-hardware.nixosModules.lenovo-thinkpad
      ../../roles
      ../../roles/fonts.nix
    ]
    ++ flake-inputs.self.nixFromDirs [
      ../../modules/hephaistos
      ../../modules/clients
      ../../modules/laptops
      ../../modules/not-home
      ../../modules/all
      ../../modules/impermanent
      ../../modules/beefs
      ../../modules/metal
    ];

  networking.hostName = "hephaistos";

  system.stateVersion = "23.05";
}
