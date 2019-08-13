let
  inherit (import ../pkgs) niv;
  inherit (import ../lib)
    pkgs writeHaskellScript get-niv-path home-manager unstable haskellList;
in rec {
  haskellBody = name: commandline: ''
    getNivPath dir name = get_niv_path ([i|#{dir :: String}/nix/sources.nix|] :: String) name |> captureTrim

    getNivAssign dir name = process <$> getNivPath dir name
        where process str = ["-I", [i|#{name :: String}=#{str :: LBS.ByteString}|]]

    main = do
      (configDir:hostname:args) <- getArgs
      paths <- concat <$> mapM (getNivAssign configDir) ["nixpkgs", "unstable", "home-manager"]
      putStrLn [i|Trying to build ${name} config for #{hostname}|]
      ${commandline}
  '';
  bins = [ get-niv-path pkgs.nix ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins;
  } (haskellBody "system" ''
    nix $ ["build", "-f", "<nixpkgs/nixos>", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ args
  '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins;
  } (haskellBody "home" ''
    nix $ ["build", "-f", "<home-manager/home-manager/home-manager.nix>"] ++ paths ++ ["--argstr", "confPath", [i|#{configDir}/hosts/#{hostname}/home.nix|], "--argstr", "confAttr", "", "--out-link", [i|result-home-manager-#{hostname}|], "activationPackage"] ++ args
  '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = [ "apollo" "hera" "hephaistos" ];
  keys = [ "default" "apollo" "hera" ];
  test-config = writeHaskellScript {
    name = "test-config";
    bins = [ test-system-config test-home-config pkgs.git niv pkgs.git-crypt ];
    imports = [
      "Control.Exception (bracket)"
      "System.Directory (withCurrentDirectory)"
      "Control.Monad (when, ap)"
      "Data.Maybe (listToMaybe)"
    ];
  } ''
    checkout :: IO FilePath
    checkout = (mktemp "-d" |> captureTrim)
      >>= ((ap (<$) $ git "clone" "${repoSrc}") . LBSC.unpack)

    main = do
      path <- pwd |> captureTrim
      bump <- (maybe False (== "bump") . listToMaybe) <$> getArgs
      bracket checkout (rm "-rf") $ \dir -> do
        withCurrentDirectory dir $ do
          mapM_ (\x -> git_crypt "unlock" ([i|${configPath}/.git/git-crypt/keys/#{x}|] :: String)) ${
            haskellList keys
          }
          when bump $ ignoreFailure $ niv "update"
        mapM_ (test_system_config dir) ${haskellList systems}
        mapM_ (test_home_config dir) ${haskellList homes}
        changed <- (mempty /=) <$> (git "-C" dir "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "-C" dir "config" "user.email" "maralorn@maralorn.de"
          git "-C" dir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" dir "commit" "-am" "Update dependencies with niv"
          git "-C" dir "push"
  '';
}
