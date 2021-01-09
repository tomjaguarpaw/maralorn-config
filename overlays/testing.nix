self: super:
let
  bins = [ self.nix ];
  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/nixos";
  systems = [ "apollo" "hera" ];
  homes = self.lib.attrNames (import ../home-manager/machines.nix);
  imports = [ "Control.Exception (onException)" ];
  haskellBody = name: drv: target: ''
    main = do
      (configDir:hostname:_) <-  getArgs
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> homeManagerChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> nixpkgsChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      say [i|Trying to build ${name} config for #{hostname}.|]
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> derivationName) <- (nix_instantiate $ ${drv}) |> captureTrim
      exe "nix-jobs" ["realise", toString derivationName]
      nix_store ["-r", toString derivationName, "--indirect", "--add-root", ${target}]
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
in {

  test-system-config = self.writeHaskellScript {
    name = "test-system-config";
    inherit bins;
    inherit imports;
  } (haskellBody "system" ''buildSystemParams ++ paths ++ ["-I", [i|nixos-config=#{configDir}/nixos/machines/#{hostname}/configuration.nix|]]'' "[i|result-system-#{hostname}|]");

  test-home-config = self.writeHaskellScript {
    name = "test-home-config";
    inherit bins;
    inherit imports;
  } (haskellBody "home" ''paths ++ [[i|#{configDir}/home-manager/target.nix|], "-A", hostname]'' "[i|result-home-manager-#{hostname}|]");

  test-config = self.writeHaskellScript {
    name = "test-config";
    bins = [
      self.test-system-config
      self.test-home-config
      self.git
      self.niv.bin
      self.git-crypt
      self.laminar
    ];
    imports = [ "System.Directory (withCurrentDirectory)" ];
  } ''
    main = do
      bump <- maybe False (== "bump") . listToMaybe <$> getArgs
      git "clone" "${repoSrc}" "config"
      withCurrentDirectory "config" $ do
        when bump $ ignoreFailure $ niv "update"
        changed <- (mempty /=) <$> (git "status" "--porcelain" |> captureTrim)
        when changed $ do
          git "config" "user.email" "maralorn@maralorn.de"
          git "config" "user.name" "maralorn (nix-auto-updater)"
          git "commit" "-am" "Update dependencies with niv"
          git "push" "-f" "origin" "HEAD:niv-bump"
        let branch = if bump then "niv-bump" else "master"
        concurrently_
          (mapConcurrently_ (\x -> laminarc ["run", [i|system-config-#{x}|], [i|BRANCH=#{branch}|]]) ${
            self.haskellList systems
          })
          (mapConcurrently_ (\x -> laminarc ["run", [i|home-config-#{x}|], [i|BRANCH=#{branch}|]]) ${
            self.haskellList homes
          })
        when changed $ git "push" "origin" "master:master"
  '';
}
