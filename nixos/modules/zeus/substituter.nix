{
  lib,
  ...
}:
{
  nix.settings.substituters = lib.mkBefore [ "https://cache.maralorn.de" ];
}
