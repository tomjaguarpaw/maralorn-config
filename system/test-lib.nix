let
  pkgs = import <nixpkgs> {};
  inherit (import ../common/lib.nix) writeHaskellScript get-niv-path home-manager unstable niv;
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
  bins = [ get-niv-path pkgs.nix ];
  imports = ["System.Console.CmdArgs.Implicit"];
  libraries = [ unstable.haskellPackages.cmdargs ];

  test-system-config = writeHaskellScript {
    name = "test-system-config";
    inherit bins imports libraries;
  } (haskellBody
    ''
      nix $ ["build", "-f", "<nixpkgs/nixos>", "system"] ++ paths ++ ["-I", [i|nixos-config=#{configDir host}/hosts/#{hostname host}/configuration.nix|], "-o", [i|result-system-#{hostname host}|]]
    '');

  test-home-config = writeHaskellScript {
    name = "test-home-config";
    inherit bins imports libraries;
  } (haskellBody
    ''
      nix $ ["build", "-f", "<home-manager/home-manager/home-manager.nix>"] ++ paths ++ ["--argstr", "confPath", [i|#{configDir host}/hosts/#{hostname host}/home.nix|], "--argstr", "confAttr", "", "--out-link", [i|result-home-manager-#{hostname host}|], "activationPackage"]
    '');

  repoSrc = "git@hera.m-0.eu:nixos-config";
  configPath = "/etc/config";
  test-and-bump-config = writeHaskellScript {
    name = "test-and-bump-config";
    bins = [ test-system-config test-home-config pkgs.git pkgs.coreutils niv pkgs.git-crypt ];
    imports = [ "Control.Exception (bracket)" "System.Directory (withCurrentDirectory)" "Control.Monad (when)"];
  } ''
      checkout :: IO FilePath
      checkout = do
        dir <- (LT.unpack . LTE.decodeUtf8 <$>) . readTrim $ mktemp "-d"
        git "clone" "${repoSrc}" dir
        return dir

      cleanup :: String -> IO ()
      cleanup = (rm "-rf")

      unlock :: IO ()
      unlock = mapM_ (\x -> git_crypt "unlock" ([i|${configPath}/.git/git-crypt/keys/#{x}|] :: String)) ["default", "apollo", "hera"]

      update :: IO ()
      update = niv "update"

      testBuild :: FilePath -> IO ()
      testBuild dir = do
        mapM_ (test_system_config dir) ["apollo", "hera"]
        mapM_ (test_home_config dir) ["apollo", "hera", "hephaistos"]

      push :: FilePath -> IO()
      push dir = do
        changed <- ((mempty /=) <$>) . readTrim $ git "-C" dir "status" "--porcelain"
        when changed $ git "-C" dir "commit" "-am" "Update dependencies with niv" >> git "-C" dir "push"

      main :: IO ()
      main = do
        path <- readTrim pwd
        bracket checkout cleanup $ \dir -> do
          withCurrentDirectory dir $ unlock >> update
          testBuild dir
          push dir
    '';
in
{
  inherit test-system-config test-home-config test-and-bump-config;
}
