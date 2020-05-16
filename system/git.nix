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
    imports = [
      "System.Environment (lookupEnv)"
      "System.Directory (withCurrentDirectory)"
    ];

  } ''
    checkout :: String -> IO FilePath
    checkout path = do
      (decodeUtf8 -> dir) <-  mktemp "-d" |> captureTrim
      git "clone" path dir
      pure dir

    main = do
      mirror <- lookupEnv "GL_OPTION_MIRROR"
      whenJust mirror $ \mirror -> do
        say [i|Forwarding push to #{mirror}|]
        git "push" "--all" "-f" mirror
      deploy <- lookupEnv "GL_OPTION_WEB_DEPLOY"
      whenJust deploy $ \deploy -> do
        (maybe [] (\x -> ["-f","default.nix",x]) -> target) <- lookupEnv "GL_OPTION_WEB_DEPLOY_NIX_TARGET"
        (decodeUtf8 -> path) <- pwd |> captureTrim
        say [i|Deploying build to /var/www/#{deploy}|]
        bracket (checkout path) (rm "-rf") $ \dir -> withCurrentDirectory dir $ nix_build "-o" ([i|/var/www/#{deploy}|] :: String) target
        say "Done"
      test <- lookupEnv "GL_OPTION_TEST"
      whenJust test $ \_ -> do
        say "Triggering (an async) system update."
        exe "sudo" ${haskellList update-command};
  '';
in {
  systemd.tmpfiles.rules = let cfg = config.services.gitolite;
  in lib.mkAfter
  [ "z ${cfg.dataDir}/.ssh/id_ed25519 0600 ${cfg.user} ${cfg.group} - -" ];
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
