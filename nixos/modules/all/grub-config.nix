{
  boot.loader = {
    timeout = 1;
    grub = {
      backgroundColor = "#000000";
      # So that boot does not fill up with old kernels
      configurationLimit = 5;
    };
  };
}
