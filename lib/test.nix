let
  inherit (import ../lib)
    pkgs writeHaskellScript home-manager unstable haskellList;
in rec {
  haskellBody = name: commandline: ''
    main = do
      (configDir:hostname:args) <-  getArgs
      paths <- myNixPath $ toText configDir
      say [i|Trying to build ${name} config for #{hostname}|]
      hFlush stdout
      ${commandline}
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
  bins = [ pkgs.nix ];
  imports = [ "System.IO (hFlush)" ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins;
    inherit imports;
  } (haskellBody "system" ''
    nix_build $ ["<nixpkgs/nixos>", "-A", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args
  '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins;
    inherit imports;
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
      (decodeUtf8 -> repoDir) <-  mktemp "-d" |> captureTrim
      git "clone" "${repoSrc}" repoDir
      pure repoDir

    main = do
      bump <- (maybe False (== "bump") . listToMaybe) <$> getArgs
      bracket checkout (rm "-rf") $ \repoDir -> do
        withCurrentDirectory repoDir $ do
          mapM_ (\x -> git_crypt "unlock" ([i|${configPath}/.git/git-crypt/keys/#{x}|] :: String)) ${
            haskellList keys
          }
          when bump $ ignoreFailure $ niv "update"
        changed <- (mempty /=) <$> (git "-C" repoDir "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "-C" repoDir "config" "user.email" "maralorn@maralorn.de"
          git "-C" repoDir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" repoDir "commit" "-am" "Update dependencies with niv"
          git "-C" repoDir "push" "-f" "origin" "master:version-bump"
        mapM_ (test_system_config repoDir) ${haskellList systems}
        mapM_ (test_home_config repoDir) ${haskellList homes}
        when changed $ do
          git "-C" repoDir "push" "origin" "master:master"
  '';
}
