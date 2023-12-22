{ pkgs, ... }:
{
  hardware.nitrokey.enable = true;
  environment.systemPackages = [
    pkgs.pynitrokey
    pkgs.yubikey-touch-detector
    pkgs.solo2-cli
  ];
  services.udev.packages = [ pkgs.solo2-cli ];
  security.pam = {
    services = {
      login.u2fAuth = true;
      sudo.u2fAuth = true;
    };
    u2f = {
      enable = true;
      # Only one key at a time is supported
      authFile = pkgs.writeText "pam_u2f_auth_file" "maralorn:owBYvu5qIyZsF0MLkAU4XJKi6H2kI6SnlKkxHUkeJIKXcnnobhgVIll05+eqjSMy9yiH7+LuDA4oMf1CzE75ha3KRj4xQ5KUMt1V8Kdva4m61y8DUq7Fj9jO8KvZ9Upieds9nXL20rUxTvFRBcyri4Lo9FhnjPbAku6CbiSAe2rJc0srkOEnd76BbzZHCMtMbo6OZ35CK7NfguTdDi6t/q2FWD4FQs1M3kqe9Q36rytR4P34vpEAmt/wib2yfkg1yP0BTG5up9E9OqcQtnDI/QJQvSoeFb/5UxWTiEtxe9vbGQ==,W6Oe9sGK6GJ7elBJf533Eh1JY07AKP4cw0aictZTdE36KHkARgqQGr+uuRxcKmuEkwS6dOBK5nij6R6R5NjZZw==,es256,+presence"; # Nitrokey 1
      # "maralorn:owBYvbupFpUfPfjD54xXro5Q1/OKR42PqWcBYmeoRj7CvY4BFw1JaeZFYsLrzUCAF8HD1YIAWo0gQw0x61zXl5GiTxbkzzQP5gK7SxqB0I8uzlhnsUAhzeGvjXwK6iq/cRZ87Jlhh/diEaMwkZDySe8PEOWZhgx5fOKIU6I221QJtsE30ZvK7JrNg0IXTxm8Wuh0WY/3/ap4ZPyOCBA2ByFN4eb5f2qp1IXNArEQXTpOcruLtdPL3fwInKwF6TtBBAFMFaXiOeUNsF2tNjbmAlD/iznnTuLJly7zO8goUG7q,JQtMyEsWzJID+tWD0E/P5ZVLBDwB3GlY/BntZZptEku/NF+tNjbHUjJ1vBBzLXDy/55h8c13hnFnwHCOK6Lkig==,es256,+presence"; # Solokey 1

      origin = "pam://maralorns-device";
      cue = true;
    };
  };
}
