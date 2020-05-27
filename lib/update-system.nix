{ pkgs, nixos-rebuild }:
let configPath = "/etc/nixos";
in {
  update-system = pkgs.writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild ];
  } ''
    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
