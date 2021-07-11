{ pkgs, ... }: {
  disabledModules = [ "services/networking/firefox/sync-server.nix" ];
  imports = [
    "${pkgs.sources."nixos-19.09"}/nixos/modules/services/networking/firefox/sync-server.nix"
  ];
  services.firefox = {
    enable = true;
    allowNewUsers = false;
  };
}
