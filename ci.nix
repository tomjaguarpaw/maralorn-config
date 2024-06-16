let
  self = builtins.getFlake (toString ./.);
  inherit (self.nixosConfigurations.zeus) pkgs;
in
pkgs.lib.mapAttrs' (name: value: {
  name = "nixos-${name}";
  value = value.config.system.build.toplevel.drvPath;
}) self.nixosConfigurations
// pkgs.lib.mapAttrs' (name: value: {
  name = "check-${name}";
  value = value.drvPath;
}) self.checks.x86_64-linux
