{ pkgs, config, ... }: {

  imports = [ ../../home ../../home/on-my-machine.nix ./secret ];

  m-0 = {
    hostName = "hera";
    mail = {
      enable = true;
      accounts = config.m-0.private.mail_accounts;
    };
    weechat = {
      enable = true;
      inherit (config.m-0.private.hackint) user hackint_pw freenode_pw channels;
    };
  };

}
