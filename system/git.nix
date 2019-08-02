{ config, pkgs, lib, ... }:
let
  inherit (import ../lib) writeHaskellScript haskellList;
  me = config.m-0.private.me;
  update-command = [
    "${pkgs.systemd}/bin/systemctl"
    "restart"
    "test-and-update.service"
    "--no-block"
  ];
  post-update = writeHaskellScript {
    name = "post-update";
    bins = [ pkgs.git pkgs.nix ];
    imports = [ "System.Environment (lookupEnv)" "Data.Foldable (for_)" ];
  } ''
    main = do
      mirror <- lookupEnv "GL_OPTION_MIRROR"
      for_ mirror $ \mirror -> do
        echo ([i|Forwarding push to #{mirror}|] :: String)
        git "push" "--all" mirror
      deploy <- lookupEnv "GL_OPTION_WEB_DEPLOY"
      for_ deploy $ \deploy -> do
        echo ([i|Deploying build to /var/www/#{deploy}|] :: String)
        nix "build" "-o" ([i|/var/www/#{deploy}|] :: String)
        echo "Done"
      test <- lookupEnv "GL_OPTION_TEST"
      for_ test $ \_ -> do
        echo "Triggering (an async) system update."
        exe "sudo" ${haskellList update-command};
  '';
in {
  users.users.git.linger =
    true; # Frequent restarting of the systemd-user-unit leads to errors
  security.sudo.extraRules = [{
    commands = [{
      command = builtins.concatStringsSep " " update-command;
      options = [ "NOPASSWD" ];
    }];
    users = [ "git" ];
  }];
  services.gitolite = {
    enable = true;
    user = "git";
    adminPubkey = builtins.elemAt me.keys 0;
    commonHooks = [ "${post-update}/bin/post-update" ];
  };
}
