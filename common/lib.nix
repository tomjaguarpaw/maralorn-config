let
  pkgs = import <nixpkgs> {};
  unstable = import <unstable> {};
  sources = import ../nix/sources.nix;
  shh = unstable.haskell.lib.overrideCabal unstable.haskellPackages.shh (drv: {
    broken = false;
    doCheck = false;
  });
  writeHaskellScript = { name ? "haskell-script", bins ? [pkgs.coreutils], libraries ? [], imports ? []}: code:
    unstable.writers.writeHaskellBin name { libraries = libraries ++ [shh unstable.haskellPackages.string-interpolate ]; } ''
      {-# LANGUAGE DeriveDataTypeable #-}
      {-# LANGUAGE TemplateHaskell #-}
      {-# LANGUAGE QuasiQuotes #-}

      import Shh
      import Data.String.Interpolate (i)
      import qualified Data.ByteString as BS
      import qualified Data.ByteString.Lazy as LBS
      import qualified Data.Text as T
      import qualified Data.Text.Lazy as LT
      import qualified Data.Text.Encoding as TE
      import qualified Data.Text.Lazy.Encoding as LTE
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins ["${builtins.concatStringsSep ''", "'' (builtins.map toString bins)}"]

      ${code}
    '';
  getNivPath = writeHaskellScript {
    name = "get-niv-path";
    bins = [pkgs.nix];
    imports = ["System.Console.CmdArgs.Implicit"];
    libraries = [ unstable.haskellPackages.cmdargs unstable.haskellPackages.text ];
  } ''

    data Path = Path {
      sources :: String,
      channel :: String
    } deriving (Show, Data, Typeable)

    path = Path{ sources = def &= argPos 0, channel = def &= argPos 1}

    trimQuotation = pureProc $ LTE.encodeUtf8 . LT.dropAround ('"' ==) . LTE.decodeUtf8 . trim

    main :: IO ()
    main = do
          path <- cmdArgs path
          let expr = [i|(import #{sources path}).#{channel path}|]
          nix_build ["-Q", "-E", expr, "--no-out-link"] &> devNull
          nix_instantiate ["--eval", "-E", [i|toString #{expr}|]] |> trimQuotation
    '';
  home-manager = pkgs.callPackage <home-manager/home-manager> {};
  niv = (import sources.niv {}).niv;
in {
    inherit writeHaskellScript home-manager getNivPath unstable niv;
}
