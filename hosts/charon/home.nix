{
  imports = [
    ../../home-common/default.nix
    ../../home-common/my-systems.nix
    ./morgenreport.nix
    ./sort-mail.nix
  ];

  home.forceCopies.paths = [ ".dovecot.sieve" ];
  home.file = {
    sieve-rules = {
      target = ".dovecot.sieve";
      text = builtins.readFile ./dovecot.sieve;
    };
  };
}
