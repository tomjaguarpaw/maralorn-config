{
  config,
  pkgs,
  lib,
  ...
}:
let
  gitoliteCfg = config.services.gitolite;
  post-update =
    pkgs.writeHaskellScript
      {
        name = "post-update";
        bins = [
          pkgs.git
          pkgs.laminar
        ];
        imports = [ "System.Directory (withCurrentDirectory)" ];
      }
      ''
        checkout :: String -> IO FilePath
        checkout path = do
          (decodeUtf8 -> repoDir) <-  mktemp "-d" |> captureTrim
          git "clone" path repoDir
          pure repoDir

        main = do
          jobMay <- lookupEnv "GL_OPTION_CI_JOB"
          whenJust jobMay $ \job -> do
            args <- toString . Text.intercalate " " . fmap toText <$> getArgs
            setEnv "LAMINAR_REASON" [i|Build triggered by push to branch #{args}|]
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
in
{
  systemd.tmpfiles.rules = lib.mkAfter [
    "z ${gitoliteCfg.dataDir}/.ssh/id_ed25519 0600 ${gitoliteCfg.user} ${gitoliteCfg.group} - -"
  ];
  systemd.services.gitolite-init.postStart = ''
    export GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=no"
    dir=$(mktemp -d)
    cd $dir
    git clone git@localhost:gitolite-admin
    cd gitolite-admin
    cp -r ${pkgs.flake-inputs.secrets}/gitolite/* .
    if [[ "$(git status --porcelain)" != "" ]]; then
      git "config" "user.email" "git@hera.m-0.eu"
      git "config" "user.name" "git user"
      git add -A
      git commit -m 'Overwrite gitolite config from nixos config'
      git push -u origin master
    fi
  '';
  services = {
    gitolite = {
      enable = true;
      user = "git";
      adminPubkey = builtins.elemAt (pkgs.privateValue [ "" ] "ssh-keys") 0;
      commonHooks = [ "${post-update}/bin/post-update" ];
      extraGitoliteRc = ''
        $RC{UMASK} = 0027;
        $RC{GIT_CONFIG_KEYS} = 'gitweb\..*';
      '';
    };
  };
}
