nixos-rebuild:
let
  pkgs = import <nixpkgs> { };
  inherit (import ../common/lib.nix)
    writeHaskellScript get-niv-path home-manager gcRetentionDays;
  configPath = "/etc/nixos";
  update-system = writeHaskellScript {
    name = "update-system";
    bins = [ get-niv-path nixos-rebuild ];
  } ''
    getNivPath = readTrim . get_niv_path "${configPath}/nix/sources.nix"

    getNivAssign name = tag <$> getNivPath name
        where tag str = ["-I", [i|#{name}=#{str :: LBS.ByteString}|] ]

    main = do
        paths <- fmap concat . mapM getNivAssign $ ["nixpkgs", "unstable", "home-manager"]
        args <- getArgs
        nixos_rebuild (paths ++ ["switch"] ++ args)
  '';
  system-maintenance = writeHaskellScript {
    name = "system-maintenance";
    bins = [ pkgs.nix pkgs.git update-system ];
  } ''
    main = do
      git "-C" "${configPath}" "pull"
      update_system
      nix_collect_garbage "--delete-older-than" "${toString gcRetentionDays}d"
      nix "optimise-store"
  '';
in { inherit update-system system-maintenance; }
