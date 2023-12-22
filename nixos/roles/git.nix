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
  cgitrc = ''
    enable-git-config=1
    remove-suffix=1
    section-from-path=1
    cache-size=1000

    source-filter=${pkgs.cgit}/lib/cgit/filters/syntax-highlighting.py
    about-filter=${pkgs.cgit}/lib/cgit/filters/about-formatting.sh
    root-title=Maralorns Projects
    root-desc=All my public projects.

    snapshots=tar.gz zip

    # Make packages cloneable
    clone-url=https://git.maralorn.de/$CGIT_REPO_URL

    # hide my name in index
    enable-index-owner=0

    # for look and feel
    enable-index-links=1
    enable-blame=1
    enable-commit-graph=1
    enable-follow-links=1
    enable-log-filecount=1
    enable-log-linecount=1
    branch-sort=age
    noplainemail=1
    side-by-side-diffs=1

    ##
    ## List of common mimetypes
    ##

    mimetype.gif=image/gif
    mimetype.html=text/html
    mimetype.jpg=image/jpeg
    mimetype.jpeg=image/jpeg
    mimetype.pdf=application/pdf
    mimetype.png=image/png
    mimetype.svg=image/svg+xml

    ## Search for these files in the root of the default branch of repositories
    ## for coming up with the about page:

    readme=:README.md
    readme=:readme.md
    readme=:README
    readme=:readme

    project-list=/var/lib/gitolite/projects.list
    scan-path=/var/lib/gitolite/repositories
  '';
in
{
  systemd.tmpfiles.rules = lib.mkAfter [
    "z ${gitoliteCfg.dataDir}/.ssh/id_ed25519 0600 ${gitoliteCfg.user} ${gitoliteCfg.group} - -"
    "v /var/cache/cgit 0700 cgit ${gitoliteCfg.group} - -"
  ];
  users.users.cgit = {
    isSystemUser = true;
    inherit (gitoliteCfg) group;
  };
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
        push( @{$RC{ENABLE}}, 'cgit' );
      '';
    };
    fcgiwrap = {
      enable = true;
      user = "cgit";
      inherit (gitoliteCfg) group;
    };

    nginx.virtualHosts."git.maralorn.de" = {
      forceSSL = true;
      enableACME = true;

      locations = {
        "~* ^.+.(css|png|ico)$" = {
          root = "${pkgs.cgit}/cgit";
        };

        "/" = {
          extraConfig = ''
            include ${pkgs.nginx}/conf/fastcgi_params;
            fastcgi_param CGIT_CONFIG ${pkgs.writeText "cgitrc" cgitrc};
            fastcgi_param SCRIPT_FILENAME ${pkgs.cgit}/cgit/cgit.cgi;
            fastcgi_split_path_info ^(/?)(.+)$;
            fastcgi_param PATH_INFO $fastcgi_path_info;
            fastcgi_param QUERY_STRING $args;
            fastcgi_param HTTP_HOST $server_name;
            fastcgi_pass unix:${config.services.fcgiwrap.socketAddress};
          '';
        };
      };
    };
  };
}
