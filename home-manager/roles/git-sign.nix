let
  signing-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVmGAML5Ap+5RNPqVnWumAVMoY6xbebvS/KfDeqp8lk";
in
{
  programs.git = {
    signing = {
      signByDefault = true;
      key = "key::${signing-key}";
    };
    extraConfig = {
      gpg.format = "ssh";
      gpg.ssh.allowedSignersFile = builtins.toFile "git-ssh-allowedSignersFile" ''
        mail@maralorn.de ${signing-key}
      '';
    };
  };
}
