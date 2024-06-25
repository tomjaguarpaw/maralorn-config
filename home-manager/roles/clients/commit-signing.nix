let
  signing-key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB8yvZbrsY9WGQexlBjmUhgBf0P3CkaPrIXIqchOtsHr"; # maralorn.sign@pantheon";
  file = builtins.toFile "git-ssh-allowedSignersFile" ''
    mail@maralorn.de ${signing-key}
    mott@heilmannsoftware.de ${signing-key}
  '';
in
{
  # Enabling signing might break ob thunk because of outdated git versions
  # programs.git = {
  #   signing = {
  #     signByDefault = true;
  #     key = "key::${signing-key}";
  #   };
  #   extraConfig = {
  #     gpg.format = "ssh";
  #     gpg.ssh.allowedSignersFile = file;
  #   };
  # };
  programs = {
    jujutsu = {
      settings.signing = {
        sign-all = true;
        backend = "ssh";
        key = signing-key;
        backends.ssh.allowed-signers = file;
      };
    };
  };
}
