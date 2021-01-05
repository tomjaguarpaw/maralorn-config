{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild pkgs.nix-output-monitor ];
  } ''
    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        setEnv "WITH_SECRETS" "false"
        nix_build (paths ++ buildSystemParams ++ ["--no-out-link"] ++ remoteBuildParams ++ fmap toString args)
        setEnv "WITH_SECRETS" "true"
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
