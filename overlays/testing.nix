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
}
