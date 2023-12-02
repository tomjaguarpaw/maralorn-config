{lib, ...}:
{
  options = {
    m-0 = {
      hostName = lib.mkOption {type = lib.types.str;};
      colors = lib.mkOption {
        default = {};
        type = lib.types.attrs;
      };
    };
  };
  config.m-0.colors.accent = "3000d0";
}
