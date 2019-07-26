{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript get-niv-path home-manager gcRetentionDays;
  configPath = "/etc/nixos";
  update-system = writeHaskellScript {
      name = "update-system";
      imports = [ "qualified Data.ByteString.Lazy.Char8 as C" "qualified Data.List as L" ];
      bins = [ get-niv-path config.system.build.nixos-rebuild ];
    }
    ''
    getNivPath = fmap C.unpack . readTrim . get_niv_path "${configPath}/nix/sources.nix"

    getNivAssign name = fmap process . getNivPath $ name
        where process str = ["-I", name ++ "=" ++ str]

    main = do
        paths <- fmap concat . mapM getNivAssign $ ["nixpkgs", "unstable", "home-manager"]
        nixos_rebuild (paths ++ ["switch"])
    '';
  system-maintenance = writeHaskellScript
    { name = "system-maintenance"; bins = [ pkgs.nix pkgs.git update-system ];} ''
    main = do
      git "-C" "${configPath}" "pull"
      update_system
      nix_collect_garbage "--delete-older-than" "${toString gcRetentionDays}d"
      nix "optimise-store"
  '';
in
{
  environment = {
    systemPackages = [
      update-system
      system-maintenance
    ];
  };
}
