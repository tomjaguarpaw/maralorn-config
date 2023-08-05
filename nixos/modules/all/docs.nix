{
  documentation = {
    dev.enable = true;
    nixos = {
      #includeAllModules = true;
      options.splitBuild = true;
    };
  };
}
