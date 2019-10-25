nixos-rebuild:
let
  inherit (import ../lib)
    writeHaskellScript get-niv-path home-manager gcRetentionDays pkgs;
in rec {
  configPath = "/etc/nixos";
  update-system = writeHaskellScript {
    name = "update-system";
    bins = [ get-niv-path nixos-rebuild ];
  } ''
    getNivPath name = get_niv_path "${configPath}/nix/sources.nix" name |> captureTrim

    getNivAssign name = tag <$> getNivPath name
        where tag str = ["-I" :: String, [i|#{name :: String}=#{str :: LBS.ByteString}|]]

    main = do
        paths <- fmap concat . mapM getNivAssign $ ["nixpkgs", "unstable", "home-manager"]
        args <- getArgs
        nixos_rebuild (paths ++ ["switch"] ++ fmap unpack args)
  '';
}
