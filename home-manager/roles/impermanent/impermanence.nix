{config, ...}:
{
  home.file = {
    ".volatile".source = config.lib.file.mkOutOfStoreSymlink "/disk/volatile${config.home.homeDirectory}";
    ".persist".source = config.lib.file.mkOutOfStoreSymlink "/disk/persist${config.home.homeDirectory}";
  };
}
