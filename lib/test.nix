let
  inherit (import ../lib)
    pkgs writeHaskellScript get-niv-path home-manager unstable haskellList;
in rec {
  haskellBody = name: commandline: ''
    getNivPath dir name = get_niv_path ([i|#{dir :: String}/nix/sources.nix|] :: String) name |> captureTrim

    getNivAssign dir name = process <$> getNivPath dir name
        where process str = ["-I" :: String, [i|#{name :: String}=#{str :: LByteString}|]]

    main = do
      (configDir:hostname:args) <- getArgs
      paths <- concat <$> mapM (getNivAssign $ toString configDir) ["nixpkgs", "unstable", "home-manager"]
      putStrLn [i|Trying to build ${name} config for #{hostname}|]
      ${commandline}
  '';
  bins = [ get-niv-path pkgs.nix ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins;
  } (haskellBody "system" ''
    nix $ ["build", "-f", "<nixpkgs/nixos>", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args
  '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins;
  } (haskellBody "home" ''
    nix $ ["build", "-f", "<home-manager/home-manager/home-manager.nix>"] ++ paths ++ ["--argstr", "confPath", [i|#{configDir}/home.nix|], "--argstr", "confAttr", hostname, "--out-link", [i|result-home-manager-#{hostname}|], "activationPackage"] ++ fmap toString args
  '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = [
    "apollo-tinkering"
    "apollo-leisure"
    "apollo-orga"
    "apollo-research"
    "hera"
    "hephaistos"
  ];
  keys = [ "default" "apollo" "hera" ];
  test-config = writeHaskellScript {
    name = "test-config";
    bins =
      [ test-system-config test-home-config pkgs.git pkgs.niv pkgs.git-crypt ];
    imports = [ "System.Directory (withCurrentDirectory)" ];
  } ''
    checkout :: IO FilePath
    checkout = do
      (decodeUtf8 -> dir) <-  mktemp "-d" |> captureTrim
      git "clone" "${repoSrc}" dir
      pure dir

    main = do
      path <- pwd |> captureTrim
      bump <- (maybe False (== "bump") . listToMaybe) <$> getArgs
      bracket checkout (rm "-rf") $ \dir -> do
        withCurrentDirectory dir $ do
          mapM_ (\x -> git_crypt "unlock" ([i|${configPath}/.git/git-crypt/keys/#{x}|] :: String)) ${
            haskellList keys
          }
          when bump $ ignoreFailure $ niv "update"
        changed <- (mempty /=) <$> (git "-C" dir "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "-C" dir "config" "user.email" "maralorn@maralorn.de"
          git "-C" dir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" dir "commit" "-am" "Update dependencies with niv"
          git "-C" dir "push" "-f" "origin" "master:version-bump"
        mapM_ (test_system_config dir) ${haskellList systems}
        mapM_ (test_home_config dir) ${haskellList homes}
        when changed $ do
          git "-C" dir "push" "origin" "master:master"
  '';
}
