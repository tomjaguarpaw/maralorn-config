let
  inherit (import ../lib)
    pkgs writeHaskellScript home-manager unstable haskellList;
in rec {
  haskellBody = name: commandline: ''
    main = do
      (configDir:hostname:args) <-  getArgs
      paths <- myNixPath $ toText configDir
      say [i|Trying to build ${name} config for #{hostname}|]
      ${commandline}
  '';
  bins = [ pkgs.nix ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins;
  } (haskellBody "system" ''
    nix_build $ ["<nixpkgs/nixos>", "-A", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args
  '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins;
  } (haskellBody "home" ''
    nix_build $ paths ++ [ [i|#{configDir}/home/target.nix|],  "-A", hostname, "-o", [i|result-home-manager-#{hostname}|]] ++ fmap toString args
  '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = pkgs.lib.attrNames (import ../home/modes.nix);
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
