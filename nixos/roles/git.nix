{ config, pkgs, lib, ... }:
let
  me = config.m-0.private.me;
  update-command = [
    "${pkgs.systemd}/bin/systemctl"
    "restart"
    "test-config.service"
    "--no-block"
  ];
  post-update = pkgs.writeHaskellScript {
    name = "post-update";
    bins = [ pkgs.git pkgs.laminar ];
    imports = [
      "System.Environment (lookupEnv)"
      "System.Directory (withCurrentDirectory)"
    ];
  } ''
    checkout :: String -> IO FilePath
    checkout path = do
      (decodeUtf8 -> repoDir) <-  mktemp "-d" |> captureTrim
      git "clone" path repoDir
      pure repoDir

    main = do
      jobMay <- lookupEnv "GL_OPTION_CI_JOB"
      whenJust jobMay $ \job -> do
        args <- toString . Text.intercalate " " . fmap toText <$> getArgs
        jobName <- decodeUtf8 <$> (laminarc ["queue", job, [i|BRANCH=#{args}|]] |> captureTrim)
        say [i|Queued job #{jobName}.\nSee https://ci.maralorn.de/jobs/#{Text.replace ":" "/" jobName}|]
      mirrorMay <- lookupEnv "GL_OPTION_MIRROR"
      whenJust mirrorMay $ \mirror -> do
        say [i|Force pushing all branches to #{mirror}|]
        git "push" "--all" "-f" mirror
      deployMay <- lookupEnv "GL_OPTION_WEB_DEPLOY"
      whenJust deployMay $ \deploy -> do
        (maybe [] (\x -> ["-A", x]) -> target) <- lookupEnv "GL_OPTION_WEB_DEPLOY_NIX_TARGET"
        (decodeUtf8 -> path) <- pwd |> captureTrim
        say [i|Building default.nix #{show target} to /var/www/#{deploy}|]
        bracket (checkout path) (rm "-rf") $ \repoDir -> withCurrentDirectory repoDir $ nix_build "-o" ([i|/var/www/#{deploy}|] :: String) target
        say "Done"
  '';
in {
  systemd.tmpfiles.rules = let cfg = config.services.gitolite;
  in lib.mkAfter
  [ "z ${cfg.dataDir}/.ssh/id_ed25519 0600 ${cfg.user} ${cfg.group} - -" ];
  services.gitolite = {
    enable = true;
    user = "git";
    adminPubkey = builtins.elemAt (pkgs.privateValue [ "" ] "ssh-keys") 0;
    commonHooks = [ "${post-update}/bin/post-update" ];
  };
}
