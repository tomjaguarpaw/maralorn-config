{ lib, pkgs, ... }:
{
  services.pipewire.enable = lib.mkForce false;
  sound.enable = true;
  hardware = {
    pulseaudio = {
      package = pkgs.pulseaudioFull;
      enable = true;
    };
  };
}
