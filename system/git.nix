{ config, pkgs, lib, ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript haskellList;
  me = config.m-0.private.me;
  test-command =
    [ "${pkgs.systemd}/bin/systemctl" "start" "test-and-bump-config.service" ];
  upgrade-command =
    [ "${pkgs.systemd}/bin/systemctl" "start" "system-maintenance.service" ];
  post-update = writeHaskellScript {
    name = "post-update";
    bins = [ pkgs.git pkgs.nix ];
    imports = [ "System.Environment (lookupEnv)" "Data.Foldable (for_)" ];
  } ''
    main = do
      mirror <- lookupEnv "GL_OPTION_MIRROR"
      for_ mirror $ \mirror -> do
        writeOutput ([i|Forwarding push to #{mirror}|] :: String)
        git "push" "--all" mirror
      deploy <- lookupEnv "GL_OPTION_WEB_DEPLOY"
      for_ deploy $ \deploy -> do
        writeOutput ([i|Deploying build to /var/www/#{deploy}|] :: String)
        nix "build" "-o" ([i|/var/www/#{deploy}|] :: String)
        writeOutput "Done"
      test <- lookupEnv "GL_OPTION_TEST"
      for_ test $ \_ -> do
        writeOutput "Triggering a system update … You can wait or disconnect";
        exe "sudo" ${haskellList test-command};
        exe "sudo" ${haskellList upgrade-command};
        writeOutput "Done";
  '';
in {
  users.users.git.linger =
    true; # Frequent restarting of the systemd-user-unit leads to errors
  security.sudo.extraRules = [{
    commands = [
      {
        command = builtins.concatStringsSep " " test-command;
        options = [ "NOPASSWD" ];
      }
      {
        command = builtins.concatStringsSep " " upgrade-command;
        options = [ "NOPASSWD" ];
      }
    ];
    users = [ "git" ];
  }];
  services.gitolite = {
    enable = true;
    user = "git";
    adminPubkey = builtins.elemAt me.keys 0;
    commonHooks = [ "${post-update}/bin/post-update" ];
  };
}
