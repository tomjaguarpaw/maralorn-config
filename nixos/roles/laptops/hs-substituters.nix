{
  nix.settings = {
    substituters = [
      "https://nixcache.reflex-frp.org"
      "http://nixcache.heilmannsoftware.net"
    ];
    trusted-public-keys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "nixcache.heilmannsoftware.net:5kPz/A5gVCs8E96y8PlT10sdAJtAQ5n4n3E0j7UXGQA="
    ];
  };
}
