{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild ];
  } ''
    privatePath = "${configPath}/private"
    canaryPath = privatePath <> "/submodule-is-checked-out"

    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        bracket (rm canaryPath) (\() -> exe "/run/wrappers/bin/sudo" "-u" "maralorn" "git" "-C" privatePath "restore" canaryPath) $ \() -> do
           nixos_rebuild (paths ++ ["build"] ++ remoteBuildParams ++ fmap toString args)
           rm "result"
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
