{
  programs.git = {
    signing = {
      signByDefault = true;
      key =
        "key::ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVmGAML5Ap+5RNPqVnWumAVMoY6xbebvS/KfDeqp8lk signing-key";
    };
    extraConfig.gpg.format = "ssh";
  };
}
