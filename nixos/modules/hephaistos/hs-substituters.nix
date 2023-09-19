{
  nix.settings = {
    substituters = [
      "http://nixcache.hs.local"
      "https://nixcache.reflex-frp.org"
      "https://cache.iog.io"
    ];
    trusted-public-keys = [
      "nixcache.hs.local-1:NZ7btgoWdlaD6+GRHW7VhQ+gwg5lAVqFmElCQrQHS4w="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
