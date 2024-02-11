{ config, ... }:
{
  home.file = {
    ".volatile".source = config.lib.file.mkOutOfStoreSymlink "/disk/volatile${config.home.homeDirectory}";
    ".persist".source = config.lib.file.mkOutOfStoreSymlink "/disk/persist${config.home.homeDirectory}";
    "logs".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/media/documents/aktuell/it/logs";
    "documents".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/media/documents/aktuell";
  };
}
