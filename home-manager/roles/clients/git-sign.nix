let
  # signing-key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIIwv46KpfLQp1w5GwXCUUKtG4oKmFU04FLvkxp6DDnE+AAAABHNzaDo="; # maralorn@solo-key-1
  signing-key = "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIDl0e4VqFH93mCNwbuz5CcbURbHj+b9DdW3RbzRoKPYLAAAADHNzaDptYXJhbG9ybg=="; # maralorn@nitro-key-1
in
{
  programs.git = {
    signing = {
      signByDefault = false;
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
