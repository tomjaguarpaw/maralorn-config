self: super:
let
  bins = [ self.nix ];
  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = self.lib.attrNames (import ../home/modes.nix);
  keys = [ "default" "apollo" "hera" ];
  haskellBody = name: commandline: ''
    main = do
      (configDir:hostname:args) <-  getArgs
      paths <- myNixPath $ toText configDir
      say [i|Trying to build ${name} config for #{hostname} ...|]
      ${commandline}
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
in {

  test-system-config = self.writeHaskellScript {
    name = "test-system-config";
    inherit bins;
  } (haskellBody "system" ''
    nix_build $ ["<nixpkgs/nixos>", "-A", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir}/hosts/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args
  '');

  test-home-config = self.writeHaskellScript {
    name = "test-home-config";
    inherit bins;
  } (haskellBody "home" ''
    nix_build $ paths ++ [[i|#{configDir}/home/target.nix|], "-A", hostname, "-o", [i|result-home-manager-#{hostname}|]] ++ fmap toString args
  '');

  test-config = self.writeHaskellScript {
    name = "test-config";
    bins = [
      self.test-system-config
      self.test-home-config
      self.git
      self.niv
      self.git-crypt
    ];
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
            self.haskellList keys
          }
          when bump $ ignoreFailure $ niv "update"
        changed <- (mempty /=) <$> (git "-C" repoDir "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "-C" repoDir "config" "user.email" "maralorn@maralorn.de"
          git "-C" repoDir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" repoDir "commit" "-am" "Update dependencies with niv"
          git "-C" repoDir "push" "-f" "origin" "master:version-bump"
        concurrently
          (mapConcurrently_ (test_system_config repoDir) ${self.haskellList systems})
          (mapConcurrently_ (test_home_config repoDir) ${self.haskellList homes})
        when changed $ do
          git "-C" repoDir "push" "origin" "master:master"
  '';
}
