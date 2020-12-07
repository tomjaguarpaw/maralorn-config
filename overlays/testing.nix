self: super:
let
  bins = [ self.nix ];
  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = self.lib.attrNames (import ../home-manager/machines.nix);
  imports = [ "Control.Exception (onException)" ];
  haskellBody = name: commandline: ''
    main = do
      (configDir:hostname:args) <-  getArgs
      (decodeUtf8 -> homeManagerChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (decodeUtf8 -> nixpkgsChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      logFile <- mktemp |> captureTrim
      let command = (${commandline}) &!> StdOut &> Append logFile
          failHandler = do
            say [i|--- Build failure for ${name} config for #{hostname} ---|]
            cat logFile
      say [i|Trying to build ${name} config for #{hostname}. Logging to #{logFile}.|]
      onException command failHandler
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
in {

  test-system-config = self.writeHaskellScript {
    name = "test-system-config";
    inherit bins;
    inherit imports;
  } (haskellBody "system" ''nix_build $ buildSystemParams ++ paths ++ ["-I", [i|nixos-config=#{configDir}/nixos/machines/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args'');

  test-home-config = self.writeHaskellScript {
    name = "test-home-config";
    inherit bins;
    inherit imports;
  } (haskellBody "home" ''nix_build $ paths ++ [[i|#{configDir}/home-manager/target.nix|], "-A", hostname, "-o", [i|result-home-manager-#{hostname}|]] ++ fmap toString args'');

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
          when bump $ ignoreFailure $ niv "update"
        changed <- (mempty /=) <$> (git "-C" repoDir "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "-C" repoDir "config" "user.email" "maralorn@maralorn.de"
          git "-C" repoDir "config" "user.name" "maralorn (nix-auto-updater)"
          git "-C" repoDir "commit" "-am" "Update dependencies with niv"
        concurrently_
          (mapConcurrently_ (\x -> test_system_config repoDir x remoteBuildParams) ${self.haskellList systems})
          (mapConcurrently_ (\x -> test_home_config repoDir x remoteBuildParams) ${self.haskellList homes})
        git "-C" repoDir "submodule" "update" "--init"
        concurrently_
          (mapConcurrently_ (test_system_config repoDir) ${self.haskellList systems})
          (mapConcurrently_ (test_home_config repoDir) ${self.haskellList homes})
        when changed $ do
          git "-C" repoDir "push" "origin" "master:master"
  '';
}
