{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild pkgs.nix-output-monitor ];
  } ''
    privatePath = "${configPath}/private"
    canaryPath = privatePath <> "/submodule-is-checked-out"

    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        bracket (rm "-f" canaryPath) (\() -> exe "/run/wrappers/bin/sudo" "-u" "maralorn" "git" "-C" privatePath "restore" canaryPath) $ \() -> do
           nix_build (paths ++ buildSystemParams ++ ["--no-out-link"] ++ remoteBuildParams ++ fmap toString args) &!> StdOut |> nom
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args) &!> StdOut |> nom
  '';
}
