{ pkgs, lib, ... }:
{
  hardware.nitrokey.enable = true;
  environment.systemPackages = [
    pkgs.pynitrokey
    pkgs.yubikey-touch-detector
  ];
  security.pam = {
    services = {
      login.u2fAuth = true;
      sudo.u2fAuth = true;
    };
    u2f = {
      enable = true;
      authFile = pkgs.writeText "pam_u2f_auth_file" ''
        maralorn:owBYvu5qIyZsF0MLkAU4XJKi6H2kI6SnlKkxHUkeJIKXcnnobhgVIll05+eqjSMy9yiH7+LuDA4oMf1CzE75ha3KRj4xQ5KUMt1V8Kdva4m61y8DUq7Fj9jO8KvZ9Upieds9nXL20rUxTvFRBcyri4Lo9FhnjPbAku6CbiSAe2rJc0srkOEnd76BbzZHCMtMbo6OZ35CK7NfguTdDi6t/q2FWD4FQs1M3kqe9Q36rytR4P34vpEAmt/wib2yfkg1yP0BTG5up9E9OqcQtnDI/QJQvSoeFb/5UxWTiEtxe9vbGQ==,W6Oe9sGK6GJ7elBJf533Eh1JY07AKP4cw0aictZTdE36KHkARgqQGr+uuRxcKmuEkwS6dOBK5nij6R6R5NjZZw==,es256,+presence
      '';
      origin = "pam://maralorns-device";
      cue = true;
    };
  };
  services.udev.extraRules = ''
    ACTION=="remove", ENV{ID_BUS}=="usb", ENV{ID_MODEL_ID}=="42b2", ENV{ID_VENDOR_ID}=="20a0", RUN+="${
      lib.getBin pkgs.systemd
    }/bin/loginctl lock-sessions"
  '';
}
