{lib, ...}: {
  options = {
    m-0 = {
      hostName = lib.mkOption {type = lib.types.str;};
      terminal = lib.mkOption {
        default = "foot";
        type = lib.types.str;
      };
      colors = lib.mkOption {
        default = {};
        type = lib.types.attrs;
      };
    };
  };
}
