{
  nix.settings = {
    substituters = [
      "http://nixcache.hs.local"
      "https://nixcache.reflex-frp.org"
    ];
    trusted-public-keys = [
      "nixcache.hs.local-1:NZ7btgoWdlaD6+GRHW7VhQ+gwg5lAVqFmElCQrQHS4w="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    ];
  };
}
