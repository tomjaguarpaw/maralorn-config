let inherit (import ./.) home-manager writeHaskellScript;
in {
  update-home = configPath:
    writeHaskellScript {
      name = "update-home";
      bins = [ home-manager ];
    } ''
      main = do
        args <- getArgs
        paths <- myNixPath "${configPath}/nix/sources.nix"
        home_manager $ paths ++ ["switch"] ++ fmap toString args
    '';
}
