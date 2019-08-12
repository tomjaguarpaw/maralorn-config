{ ... }: {
  nix = {
    binaryCaches = [ "http://hera:5001" ];
    binaryCachePublicKeys =
      [ "hera:dPBUx652Yr9s2pQDeHRQ5dl5zSuo+j94AyvJGZJaMFw=" ];
  };
}
