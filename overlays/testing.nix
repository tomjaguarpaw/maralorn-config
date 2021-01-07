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
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> homeManagerChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.home-manager-channel|] :: String) |> captureTrim
      (Text.dropAround ('"' ==) . decodeUtf8 . trim -> nixpkgsChannel) <- nix_instantiate "--eval" "-E" ([i|(import #{configDir}/channels.nix).#{hostname}.nixpkgs-channel|] :: String) |> captureTrim
      paths <- aNixPath homeManagerChannel nixpkgsChannel (toText configDir)
      say [i|Trying to build ${name} config for #{hostname}.|]
      ${commandline}
      say [i|Build of ${name} config for #{hostname} was successful.|]
  '';
in {

  test-system-config = self.writeHaskellScript {
    name = "test-system-config";
    inherit bins;
    inherit imports;
  } (haskellBody "system" ''
    nix_build $ buildSystemParams ++ paths ++ ["-I", [i|nixos-config=#{configDir}/nixos/machines/#{hostname}/configuration.nix|], "-o", [i|result-system-#{hostname}|]] ++ fmap toString args'');

  test-home-config = self.writeHaskellScript {
    name = "test-home-config";
    inherit bins;
    inherit imports;
  } (haskellBody "home" ''
    nix_build $ paths ++ [[i|#{configDir}/home-manager/target.nix|], "-A", hostname, "-o", [i|result-home-manager-#{hostname}|]] ++ fmap toString args'');

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
