{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild pkgs.git ];
  } ''
    privatePath = "${configPath}/private"
    canaryPath = privatePath <> "/submodule-is-checked-out"

    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        bracket (rm canaryPath) (\() -> git "-C" privatePath "restore" canaryPath) $ \() ->
           nixos_rebuild (paths ++ ["build"] ++ remoteBuildParams ++ fmap toString args)
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
