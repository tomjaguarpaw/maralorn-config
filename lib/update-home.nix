let inherit (import ./.) home-manager writeHaskellScript get-niv-path;
in {
  update-home = configPath:
    writeHaskellScript {
      name = "update-home";
      bins = [ get-niv-path home-manager ];
    } ''

      getNivPath = get_niv_path "${configPath}/nix/sources.nix"

      getNivAssign name = (tag <$>) . readTrim . getNivPath $ name
        where tag str = ["-I", [i|#{name}=#{str :: LBS.ByteString}|]]

      main = do
        args <- getArgs
        paths <- concat <$> mapM getNivAssign ["home-manager", "nixpkgs", "unstable"]
        home_manager $ paths ++ ["switch"] ++ args
    '';
}
