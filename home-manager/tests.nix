{ pkgs, config, lib,  ... }:
let
  inherit (import ../common/lib.nix) writeHaskellScript getNivPath home-manager unstable niv;
  haskellBody = commandline:
    ''

    data Host = Host {
      configDir :: String,
      hostname :: String
    } deriving (Show, Data, Typeable)

    host = Host{
      configDir = def &= argPos 0,
      hostname = def &= argPos 1
    }

    getNivPath dir = readTrim . get_niv_path ([i|#{dir :: String}/nix/sources.nix|] :: String)

    getNivAssign dir name = fmap process . getNivPath dir $ name
        where process str = ["-I", [i|#{name :: String}=#{str :: LBS.ByteString}|]]

    main :: IO ()
    main = do
      host <- cmdArgs host
      paths <- (concat <$>) . mapM (getNivAssign $ configDir host) $ ["nixpkgs", "unstable", "home-manager"]
      ${commandline}
    '';

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    bins = [ getNivPath pkgs.nix ];
    imports = ["System.Console.CmdArgs.Implicit"];
    libraries = [ unstable.haskellPackages.cmdargs ];
  } (haskellBody
    ''
      nix $ ["build", "-f", "<nixpkgs/nixos>", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir host}/hosts/#{hostname host}/configuration.nix|], "-o", [i|result-system-#{hostname host}|]]
    '');

  test-home-manager-config = writeHaskellScript {
    name = "test-home-manager-config";
    bins = [ getNivPath pkgs.nix ];
    imports = ["System.Console.CmdArgs.Implicit"];
    libraries = [ unstable.haskellPackages.cmdargs ];
  } (haskellBody
    ''
      nix $ ["build", "-f", "<home-manager/home-manager/home-manager.nix>"] ++ paths ++ ["--argstr", "confPath", [i|#{configDir host}/hosts/#{hostname host}/home.nix|], "--argstr", "confAttr", "", "--out-link", [i|result-home-manager-#{hostname host}|], "activationPackage"]
    '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  bump-config = writeHaskellScript {
    name = "bump-config";
    bins = [ test-system-config test-home-manager-config pkgs.git pkgs.coreutils niv pkgs.git-crypt ];
    imports = [ "Control.Exception (bracket)" "System.Directory (withCurrentDirectory)" "Control.Monad (when)"];
  } ''
      main = do
        path <- readTrim pwd
        bracket (do
          dir <- (LT.unpack . LTE.decodeUtf8 <$>) . readTrim $ mktemp "-d"
          git "clone" "${repoSrc}" dir
          return dir)
          (rm "-rf") $
          \dir -> do
            withCurrentDirectory dir $ git_crypt "unlock" >> niv "update"
            mapM_ (test_system_config dir) ["apollo", "hera"]
            mapM_ (test_home_manager_config dir) ["apollo", "hera", "hephaistos"]
            changed <- ((mempty /=) <$>) . readTrim $ git "-C" dir "status" "--porcelain"
            when changed $ git "-C" dir "commit" "-am" "Update dependencies with niv" >> git "-C" dir "push"
    '';
in
{
  home.packages = [
    test-system-config
    test-home-manager-config
    bump-config
  ];
}
