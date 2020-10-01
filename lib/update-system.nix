{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild pkgs.git ];
  } ''
    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        privateDeinit :: Either SomeException () <- try $ git "-C" "${configPath}" "submodule" "deinit" "private"
        when (isRight privateDeinit) $ do
           nixos_rebuild (paths ++ ["build"] ++ remoteBuildParams ++ fmap toString args)
           git "-C" "${configPath}" "submodule" "update" "--init" "private"
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
