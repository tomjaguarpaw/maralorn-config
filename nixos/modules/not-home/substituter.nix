{
  lib,
  ...
}:
{
  nix.settings.substituters = lib.mkAfter [ "https://cache.maralorn.de" ];
}
