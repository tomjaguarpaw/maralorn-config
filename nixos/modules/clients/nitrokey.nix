{ pkgs, lib, ... }:
{
  hardware.nitrokey.enable = true;
  environment.systemPackages = [ pkgs.pynitrokey ];
  services.udev.extraRules = ''
    ACTION=="remove", ENV{ID_BUS}=="usb", ENV{ID_MODEL_ID}=="42b2", ENV{ID_VENDOR_ID}=="20a0", RUN+="${
      lib.getBin pkgs.systemd
    }/bin/loginctl lock-sessions"
  '';
}
