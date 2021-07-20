{ pkgs, nixos-rebuild }:
let
  configPath = "/etc/nixos";
in
{
  update-system = pkgs.writeHaskellScript
    {
      name = "update-system";
      bins = [ nixos-rebuild pkgs.nix-output-monitor pkgs.nvd ];
    } ''
    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        setEnv "WITH_SECRETS" "false"
        nom_build (paths ++ buildSystemParams ++ ["--no-out-link"] ++ remoteBuildParams ++ fmap toString args)
        setEnv "WITH_SECRETS" "true"
        oldSystem <- readlink "-f" "/run/current-system" |> captureTrim
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args) &!> StdOut |> nom
        newSystem <- readlink "-f" "/run/current-system" |> captureTrim
        nvd "diff" oldSystem newSystem
  '';
}
