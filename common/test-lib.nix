let
  inherit (import ../common/pkgs.nix) niv;
  inherit (import ../common/lib.nix) pkgs
    writeHaskellScript get-niv-path home-manager unstable haskellList;
  haskellBody = commandline: ''
    getNivPath dir = readTrim . get_niv_path ([i|#{dir :: String}/nix/sources.nix|] :: String)

    getNivAssign dir name = fmap process . getNivPath dir $ name
        where process str = ["-I", [i|#{name :: String}=#{str :: LBS.ByteString}|]]

    main = do
      (configDir:hostname:args) <- getArgs
      paths <- concat <$> mapM (getNivAssign configDir) ["nixpkgs", "unstable", "home-manager"]
      ${commandline}
  '';
  bins = [ get-niv-path pkgs.nix ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins;
  } (haskellBody ''
    nix $ ["build", "-f", "<nixpkgs/nixos>", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ args
  '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins;
  } (haskellBody ''
    nix $ ["build", "-f", "<home-manager/home-manager/home-manager.nix>"] ++ paths ++ ["--argstr", "confPath", [i|#{configDir}/hosts/#{hostname}/home.nix|], "--argstr", "confAttr", "", "--out-link", [i|result-home-manager-#{hostname}|], "activationPackage"] ++ args
  '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = [ "apollo" "hera" "hephaistos" ];
  keys = [ "default" "apollo" "hera" ];
  test-and-bump-config = writeHaskellScript {
    name = "test-and-bump-config";
    bins = [
      test-system-config
      test-home-config
      pkgs.git
      pkgs.coreutils
      niv
      pkgs.git-crypt
    ];
    imports = [
      "Control.Exception (bracket)"
      "System.Directory (withCurrentDirectory)"
      "Control.Monad (when)"
    ];
  } ''
    checkout :: IO FilePath
    checkout = do
      dir <- LBSC.unpack <$> (readTrim $ mktemp "-d")
      git "clone" "${repoSrc}" dir
      return dir

    main = do
      path <- readTrim pwd
      bracket checkout (rm "-rf") $ \dir -> do
        withCurrentDirectory dir $ do
          mapM_ (\x -> git_crypt "unlock" ([i|${configPath}/.git/git-crypt/keys/#{x}|] :: String)) ${
      haskellList keys
          }
          ignoreFailure $ niv "update"
        mapM_ (test_system_config dir) ${haskellList systems}
        mapM_ (test_home_config dir) ${haskellList homes}
        changed <- ((mempty /=) <$>) . readTrim $ git "-C" dir "status" "--porcelain"
        when changed $ do
          git "-C" dir "config" "user.email" "maralorn@maralorn.de"
          git "-C" dir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" dir "commit" "-am" "Update dependencies with niv"
          git "-C" dir "push"
  '';
in { inherit test-system-config test-home-config test-and-bump-config; }
