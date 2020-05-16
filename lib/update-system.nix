nixos-rebuild:
let inherit (import ../lib) writeHaskellScript;
in rec {
  configPath = "/etc/nixos";
  update-system = writeHaskellScript {
    name = "update-system";
    bins = [ nixos-rebuild ];
  } ''
    main = do
        paths <- myNixPath "${configPath}"
        args <- getArgs
        nixos_rebuild (paths ++ ["switch"] ++ fmap toString args)
  '';
}
