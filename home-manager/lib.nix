let
  inherit (import ../common/lib.nix) home-manager writeHaskellScript getNivPath;
in
{
  update-home-manager = configPath: writeHaskellScript
    {
      name = "update-home-manager";
      imports = [
        "qualified Data.ByteString.Lazy.Char8 as C"
        "qualified Data.List as L"
      ];
      bins = [
        getNivPath
        home-manager
      ];
    }
    ''

    getNivAssign name = fmap tag . readTrim $ get_niv_path "${configPath}/nix/sources.nix" name
      where tag str = ["-I", name ++ "=" ++ C.unpack str]

    main = do
      paths <- mapM getNivAssign ["home-manager", "nixpkgs", "unstable"]
      home_manager (concat paths ++ ["switch"])
    '';
}
