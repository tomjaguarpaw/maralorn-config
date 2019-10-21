let inherit (import ./.) home-manager writeHaskellScript get-niv-path;
in {
  update-home = configPath:
    writeHaskellScript {
      name = "update-home";
      bins = [ get-niv-path home-manager ];
    } ''

      getNivPath name = get_niv_path "${configPath}/nix/sources.nix" name |> captureTrim

      getNivAssign name = tag <$> getNivPath name
          where tag str = ["-I", [i|#{name :: String}=#{str :: LBS.ByteString}|]] :: [String]

      main = do
        args <- getArgs
        paths <- concat <$> mapM getNivAssign ["home-manager", "nixpkgs", "unstable"]
        home_manager $ paths ++ ["switch"] ++ fmap unpack args
    '';
}
