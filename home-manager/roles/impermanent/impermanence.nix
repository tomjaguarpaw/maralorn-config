{ config, ... }:
let
  lnk = config.lib.file.mkOutOfStoreSymlink;
in
{
  home.file = {
    ".volatile".source = lnk "/disk/volatile${config.home.homeDirectory}";
    ".persist".source = lnk "/disk/persist${config.home.homeDirectory}";
    logs.source = lnk "${config.home.homeDirectory}/media/documents/aktuell/it/logs";
    documents.source = lnk "${config.home.homeDirectory}/media/documents/aktuell";
  };
}
