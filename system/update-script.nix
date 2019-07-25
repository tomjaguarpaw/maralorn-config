{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript getNivPath getNivAssign home-manager;
  configPath = "/etc/nixos";
  update-system = writeHaskellScript {
      name = "update-system";
      imports = [ "qualified Data.ByteString.Lazy.Char8 as C" "qualified Data.List as L" ];
      bins = [ getNivPath config.system.build.nixos-rebuild ];
    }
    ''
    getNivPath = fmap C.unpack . readTrim . get_niv_path "${configPath}/nix/sources.nix"

    getNivAssign name = fmap process . getNivPath $ name
        where process str = ["-I", name ++ "=" ++ str]

    main = do
        paths <- fmap concat . mapM getNivAssign $ ["nixpkgs", "unstable", "home-manager"]
        nixos_rebuild (paths ++ ["switch"])
    '';
in
{
  config = {
    environment = {
      systemPackages = [
        update-system
      ];
    };
  };
}
